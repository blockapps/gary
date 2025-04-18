{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main (main) where
import           Control.Concurrent         (threadDelay)

import           Control.Monad
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as JSON
import qualified Data.Aeson.KeyMap          as JSON
import qualified Data.ByteString.Lazy       as BL
import           OpenAI.V1
import           OpenAI.V1.Chat.Completions
import           OpenAI.V1.Tool
import qualified OpenAI.V1.ToolCall         as ToolCall

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Encoding
import qualified Data.Text.IO               as Text.IO
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import qualified System.Environment         as Environment
import           System.IO
import           System.Process
import           Text.Tools
import           Text.Wrap
import Text.Colors (green)

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

  foreverM [] $ \history -> do

    let outstandingCalls =
          case history of
            [] -> []
            _  -> concatMap V.toList $ tool_calls $ last history

    requestMessages <-
      case outstandingCalls of
        [] -> do
          putStr "> "
          hFlush stdout
          text <- Text.IO.getLine
          return [User{ content = [ Text{ text } ], name = Nothing }]
        _ -> do
          forM outstandingCalls $ \call -> do
            threadDelay 1000
            command <- callToString call
            
            putStrLn $ green ("########## " ++ show command)
            (_, out, err) <- readCreateProcessWithExitCode (shell $ Text.unpack command) ""
            return Tool{content=[Text $ Text.pack $ err ++ out], tool_call_id=ToolCall.id call}

    ChatCompletionObject{ choices } <-
      createChatCompletion _CreateChatCompletion
      { messages = V.fromList $ history ++ requestMessages
      , model = "gpt-4.1"
      , tools=Just $ V.singleton consoleTool
      }

    let response = fillMessage $ message $ V.head choices

    putStrLn $ printMessage response

    case response of
      Assistant{assistant_content=Just _} -> return $ map prune $ history ++ requestMessages ++ [response]
      _ -> return $ history ++ requestMessages ++ [response]

  return ()


prune :: FullMessage -> FullMessage
prune Tool{..} = Tool{content = [Text "[redacted for brevity]"], tool_call_id=tool_call_id}
prune x = x

printMessage :: FullMessage -> String
printMessage Assistant{assistant_content=Nothing} = "" -- show theMessage
printMessage Assistant{assistant_content=Just v} = box $ lines $ "😎 GARY:\n" ++ Text.unpack (wrapText defaultWrapSettings 100 $ tprintContent v)
printMessage v = error $ "unsupported case in call to printMessage: " ++ show v

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

