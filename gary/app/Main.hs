{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main (main) where

import           Control.Concurrent           (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                   ((.=))
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.KeyMap            as JSON
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Char8   as BLC
import           Data.Foldable
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Encoding
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           OpenAI.V1
import           OpenAI.V1.Chat.Completions
import           OpenAI.V1.Tool
import qualified OpenAI.V1.ToolCall           as ToolCall
import           Servant.Client.Core
import           System.Console.Haskeline
import           System.Console.Terminal.Size
import           System.Directory             (getCurrentDirectory)
import qualified System.Environment           as Environment
import           System.Exit
import           System.FilePath              (takeFileName)
import           System.Process
import           Text.Colors                  (green, red)
import           Text.Tools
import           Text.Wrap

type FullMessage = Message (V.Vector Content)

foreverM :: Monad m => a-> (a -> m a) -> m ()
foreverM a f = f a >>= flip foreverM f

consoleTool :: Tool
consoleTool =
  Tool_Function{
  function=Function{
      description=Nothing,
      name="console",
      parameters=Just $ JSON.object [
          "type" .= ("object" :: String),
          "properties" .= JSON.object [
              "command" .= JSON.object [
                  "type" .= ("string" :: String),
                  "description" .= ("" :: String)
                  ]
              ]
          ],
        strict=Nothing
      }
  }

callToString :: Monad m => ToolCall.ToolCall -> m Text
callToString call = do
  let ToolCall.Function{..} = ToolCall.function call
  let args =
        case JSON.eitherDecode $ BL.fromStrict $ encodeUtf8 arguments :: Either String JSON.Value of
          Left e  -> error e
          Right v -> v
  let commandValue =
        case args of
          JSON.Object kvs -> JSON.lookup "command" kvs
          _               -> error "function args wrong format"

  let command =
        case commandValue of
          Just (JSON.String v) -> v
          _                    -> error "command not string"

  return command

main :: IO ()
main = do
  key <- Environment.getEnv "OPENAI_KEY"

  clientEnv <- getClientEnv "https://api.openai.com"

  let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)
  let systemMsg = System{ content = V.singleton $ Text "You are Gary!  Gary is our intern who’s surprisingly helpful, weirdly fast.  You use the console often to work on tasks that we ask you to do.  If asked to run a routine command, just use the console and do it.  If asked to run a dangerous command, always ask for confirmation.", name = Nothing }

  foreverM [systemMsg] $ \history -> runInputT defaultSettings{ historyFile = Just ".gary_history" } $ do

    let outstandingCalls =
          case history of
            [] -> []
            _ -> case last history of
                   Assistant{..} -> concatMap V.toList tool_calls
                   _             -> []

    requestMessages <-
      case outstandingCalls of
        [] -> do
          cwd <- liftIO getCurrentDirectory
          let folder = takeFileName cwd
          text' <- getInputLine $ "🛠️  " ++ folder ++ "> "
          text <-
            case text' of
              Nothing -> liftIO exitSuccess
              Just v -> return $ Text.pack v
          return [User{ content = [ Text{ text } ], name = Nothing }]
        _ -> do
          forM outstandingCalls $ \call -> do
            liftIO $ threadDelay 1000
            command <- callToString call

            liftIO $ putStrLn $ green ("########## " ++ show command)
            (_, out, err) <- liftIO $ readCreateProcessWithExitCode (shell $ Text.unpack command) ""
            return Tool{content=[Text $ Text.pack $ err ++ out], tool_call_id=ToolCall.id call}

    ChatCompletionObject{ choices } <- liftIO $
      tryWithRepeat $
      createChatCompletion _CreateChatCompletion
      { messages = V.fromList $ history ++ requestMessages
      , model = "gpt-4.1"
      , tools=Just $ V.singleton consoleTool
      }

    let response = fillMessage $ message $ V.head choices

    Just Window{width=windowWidth} <- liftIO size

    liftIO $ putStrLn $ printMessage windowWidth response

    case response of
      Assistant{assistant_content=Just _} -> return $ map prune $ history ++ requestMessages ++ [response]
      _ -> return $ history ++ requestMessages ++ [response]

  return ()

tryWithRepeat :: IO a -> IO a
tryWithRepeat f = do
  result <- try f
  case result of
    Right v -> return v
    Left e ->
      case e of
        e' | Just (FailureResponse request response) <- fromException e'-> do
               case lookup "retry-after-ms" $ toList $ responseHeaders response of
                     Just pauseTimeString -> do
                       let pauseTime = read $ BC.unpack pauseTimeString
                       putStrLn $ red $ "Pausing for " ++ show (pauseTime `div` 1000) ++ " seconds...."
                       threadDelay $ 1000 * pauseTime
                       tryWithRepeat f
                     Nothing ->
                       error $ "Error FailureResponse!\n" ++ "  -request = " ++ show request ++ "\n  -response = " ++ show response ++ "\n\n  message=\n" ++ BLC.unpack (responseBody response)
        _ -> error $ show (e :: SomeException)

prune :: FullMessage -> FullMessage
prune Tool{..} = Tool{content = [Text "[redacted for brevity]"], tool_call_id=tool_call_id}
prune x = x

printMessage :: Int -> FullMessage -> String
printMessage _ Assistant{assistant_content=Nothing} = "" -- show theMessage
printMessage windowWidth Assistant{assistant_content=Just v} = box $ lines $ "😎 GARY:\n" ++ Text.unpack (wrapText defaultWrapSettings (windowWidth-4) $ tprintContent v)
printMessage _ v = error $ "unsupported case in call to printMessage: " ++ show v

tprintContent :: Vector Content -> Text
tprintContent content = Text.concat $ map tshowContentItem $ V.toList content

tshowContentItem :: Content -> Text
tshowContentItem (Text v) = v
tshowContentItem v = error $ "unsupported case in tshowContentItem: " ++ show v

fillMessage :: Message Text -> FullMessage
fillMessage System{..} = System{content=V.singleton $ Text content, name=name}
fillMessage Assistant{..} = Assistant{
                         assistant_content=fmap (V.singleton . Text) assistant_content,
                         refusal=refusal,
                         name=name,
                         assistant_audio=assistant_audio,
                         tool_calls=tool_calls
                         }
fillMessage v = error $ "unexpected value in call to fillMessage: " ++ show v

