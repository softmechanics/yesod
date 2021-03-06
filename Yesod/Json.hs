-- | Efficient generation of JSON documents.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Json
    ( -- * Monad
      Json
    , jsonToContent
    , jsonToRepJson
      -- * Generate Json output
    , jsonScalar
    , jsonList
    , jsonMap
    , jsonRaw
#if TEST
    , testSuite
#endif
    )
    where

import qualified Data.ByteString.Char8 as S
import Data.Char (isControl)
import Yesod.Handler (GHandler)
import Numeric (showHex)
import Data.Monoid (Monoid (..))
import Text.Blaze.Builder.Core
import Text.Blaze.Builder.Utf8 (writeChar)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.ByteString.Lazy.Char8 (unpack)
import Yesod.Content hiding (testSuite)
#else
import Yesod.Content
#endif

-- | A monad for generating Json output. It wraps the Builder monoid from the
-- blaze-builder package.
--
-- This is an opaque type to avoid any possible insertion of non-JSON content.
-- Due to the limited nature of the JSON format, you can create any valid JSON
-- document you wish using only 'jsonScalar', 'jsonList' and 'jsonMap'.
newtype Json = Json { unJson :: Builder }
    deriving Monoid

-- | Extract the final result from the given 'Json' value.
--
-- See also: applyLayoutJson in "Yesod.Yesod".
jsonToContent :: Json -> GHandler sub master Content
jsonToContent = return . toContent . toLazyByteString . unJson

-- | Wraps the 'Content' generated by 'jsonToContent' in a 'RepJson'.
jsonToRepJson :: Json -> GHandler sub master RepJson
jsonToRepJson = fmap RepJson . jsonToContent

-- | Outputs a single scalar. This function essentially:
--
-- * Performs JSON encoding.
--
-- * Wraps the resulting string in quotes.
jsonScalar :: String -> Json
jsonScalar s = Json $ mconcat
    [ fromByteString "\""
    , writeList writeJsonChar s
    , fromByteString "\""
    ]
  where
    writeJsonChar '\b' = writeByteString "\\b"
    writeJsonChar '\f' = writeByteString "\\f"
    writeJsonChar '\n' = writeByteString "\\n"
    writeJsonChar '\r' = writeByteString "\\r"
    writeJsonChar '\t' = writeByteString "\\t"
    writeJsonChar '"' = writeByteString "\\\""
    writeJsonChar '\\' = writeByteString "\\\\"
    writeJsonChar c
        | not $ isControl c = writeChar c
        | c < '\x10'   = writeString $ '\\' : 'u' : '0' : '0' : '0' : hexxs
        | c < '\x100'  = writeString $ '\\' : 'u' : '0' : '0' : hexxs
        | c < '\x1000' = writeString $ '\\' : 'u' : '0' : hexxs
        where hexxs = showHex (fromEnum c) ""
    writeJsonChar c = writeChar c
    writeString = writeByteString . S.pack

-- | Outputs a JSON list, eg [\"foo\",\"bar\",\"baz\"].
jsonList :: [Json] -> Json
jsonList [] = Json $ fromByteString "[]"
jsonList (x:xs) = mconcat
    [ Json $ fromByteString "["
    , x
    , mconcat $ map go xs
    , Json $ fromByteString "]"
    ]
  where
    go = mappend (Json $ fromByteString ",")

-- | Outputs a JSON map, eg {\"foo\":\"bar\",\"baz\":\"bin\"}.
jsonMap :: [(String, Json)] -> Json
jsonMap [] = Json $ fromByteString "{}"
jsonMap (x:xs) = mconcat
    [ Json $ fromByteString "{"
    , go x
    , mconcat $ map go' xs
    , Json $ fromByteString "}"
    ]
  where
    go' y = mappend (Json $ fromByteString ",") $ go y
    go (k, v) = mconcat
        [ jsonScalar k
        , Json $ fromByteString ":"
        , v
        ]

-- | Outputs raw JSON data without performing any escaping. Use with caution:
-- this is the only function in this module that allows you to create broken
-- JSON documents.
jsonRaw :: S.ByteString -> Json
jsonRaw = Json . fromByteString

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Json"
    [ testCase "simple output" caseSimpleOutput
    ]

caseSimpleOutput :: Assertion
caseSimpleOutput = do
    let j = do
        jsonMap
            [ ("foo" , jsonList
                [ jsonScalar "bar"
                , jsonScalar "baz"
                ])
            ]
    "{\"foo\":[\"bar\",\"baz\"]}" @=? unpack (toLazyByteString $ unJson j)

#endif
