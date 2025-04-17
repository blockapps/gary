{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.ByteString.Lazy as BL
import OpenAI.V1
import OpenAI.V1.Chat.Completions
import OpenAI.V1.Tool
import qualified OpenAI.V1.ToolCall as ToolCall

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as V
import qualified System.Environment as Environment
import System.Process

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
          Left e -> error e
          Right v -> v
  let commandValue =
        case args of
          JSON.Object kvs -> JSON.lookup "command" kvs
          _ -> error "function args wrong format"

  let command =
        case commandValue of
          Just (JSON.String v) -> v
          _ -> error "command not string"

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
            _ -> concatMap V.toList $ tool_calls $ last history

    requestMessages <-
      case outstandingCalls of
        [] -> do
          putStr "> "
          text <- Text.IO.getLine
          return [User{ content = [ Text{ text } ], name = Nothing }]
        _ -> do
          forM outstandingCalls $ \call -> do
            command <- callToString call
              
            putStrLn $ "########## " ++ show command
            (_, stdout, stderr) <- readCreateProcessWithExitCode (shell $ Text.unpack command) ""
            return Tool{content=[Text $ Text.pack $ stderr ++ stdout], tool_call_id=ToolCall.id call}
          
    ChatCompletionObject{ choices } <-
      createChatCompletion _CreateChatCompletion
      { messages = V.fromList $ history ++ requestMessages
      , model = "gpt-4.1"
      , tools=Just $ V.singleton consoleTool
      }

    let response = message $ V.head choices

    putStrLn $ printMessage response
    
    return $ history ++ requestMessages ++ [fillMessage $ message $ V.head choices]

  return ()

printMessage :: Message Text -> String
printMessage Assistant{assistant_content=Nothing} = "" -- show theMessage
printMessage Assistant{assistant_content=Just v} = "ASSISTANT: " ++ Text.unpack v
printMessage v = error $ "unsupported case in call to printMessage: " ++ show v

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
