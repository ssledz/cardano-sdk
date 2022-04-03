module Cardano.Sdk.Script
  ( readFileScriptInAnyLang
  , deserialiseScriptInAnyLang
  ) where

import qualified Data.ByteString.Char8 as BS

import           Cardano.Api
import           RIO

data ScriptDecodeError =
       ScriptDecodeTextEnvelopeError TextEnvelopeError
     | ScriptDecodeSimpleScriptError JsonDecodeError
  deriving Show

readFileScriptInAnyLang :: FilePath -> IO ScriptInAnyLang
readFileScriptInAnyLang file = do
  scriptBytes <- BS.readFile file
  case deserialiseScriptInAnyLang scriptBytes of
    Left err     -> throwString $ show err
    Right script -> return script

deserialiseScriptInAnyLang :: ByteString
                           -> Either ScriptDecodeError ScriptInAnyLang
deserialiseScriptInAnyLang bs =
    -- Accept either the text envelope format wrapping the binary serialisation,
    -- or accept the simple script language in its JSON format.
    --
    case deserialiseFromJSON AsTextEnvelope bs of
      Left _   ->
        -- The SimpleScript language has the property that it is backwards
        -- compatible, so we can parse as the latest version and then downgrade
        -- to the minimum version that has all the features actually used.
        case deserialiseFromJSON (AsSimpleScript AsSimpleScriptV2) bs of
          Left  err    -> Left (ScriptDecodeSimpleScriptError err)
          Right script -> Right (toMinimumSimpleScriptVersion script)

      Right te ->
        case deserialiseFromTextEnvelopeAnyOf textEnvTypes te of
          Left  err    -> Left (ScriptDecodeTextEnvelopeError err)
          Right script -> Right script

  where
    textEnvTypes :: [FromSomeType HasTextEnvelope ScriptInAnyLang]
    textEnvTypes =
      [ FromSomeType (AsScript AsSimpleScriptV1)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1))

      , FromSomeType (AsScript AsSimpleScriptV2)
                     (ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2))

      , FromSomeType (AsScript AsPlutusScriptV1)
                     (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1))
      ]

    toMinimumSimpleScriptVersion :: SimpleScript SimpleScriptV2
                                 -> ScriptInAnyLang
    toMinimumSimpleScriptVersion s =
      case adjustSimpleScriptVersion SimpleScriptV1 s of
        Nothing -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV2)
                                   (SimpleScript SimpleScriptV2 s)
        Just s' -> ScriptInAnyLang (SimpleScriptLanguage SimpleScriptV1)
                                   (SimpleScript SimpleScriptV1 s')
