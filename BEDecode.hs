{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}

module BEDecode (
  beDecode,
  BData(..),
  FromBData, ToBData,
  fromBData, toBData
) where

import Prelude as P
import Data.Attoparsec.Char8 as AC
import Data.Attoparsec.Lazy as ACL
import Data.ByteString as B
import Data.ByteString.UTF8 as BU (fromString, toString)

import Control.Applicative ((<|>))
import Data.Map as M


class FromBData a where
  fromBData :: BData -> Maybe a

class ToBData a where
  toBData :: a -> BData

instance FromBData Integer where
  fromBData (BInt a) = Just a
  fromBData _ = Nothing

instance FromBData String where
  fromBData (BString str) = Just $ toString str
  fromBData _ = Nothing

instance FromBData ByteString where
  fromBData (BString str) = Just str
  fromBData _ = Nothing

instance ToBData Integer where
  toBData i = BInt i
instance ToBData Int where
  toBData i = BInt (fromIntegral i)

instance ToBData String where
  toBData str = BString $ fromString str

instance ToBData B.ByteString where
  toBData bs = BString bs


data BData =
    BInt Integer
  | BString ByteString
  | BList [BData]
  | BAssoc (Map ByteString BData)
  
    deriving Show

beDecode str = case ACL.parse bParser str of
  ACL.Fail _ _ _ -> Nothing
  ACL.Done _ r -> Just r

bParser = bParseAssoc <|> bParseList <|> bParseString <|> bParseInt

bParseAssoc = do
  char 'd'
  xs <- many element
  char 'e'
  return $ BAssoc (fromList xs)
  where
    element = do
      BString key <- bParseString
      val <- bParser
      return (key, val)


bParseList :: Parser BData
bParseList = do
  char 'l'
  res <- many bParser
  char 'e'
  return $ BList res

bParseString :: Parser BData
bParseString = do
  len <- decimal
  char ':'
  str <- AC.take len
  return $ BString str

bParseInt :: Parser BData
bParseInt = do
  char 'i'
  x <- signed decimal
  char 'e'
  return $ BInt x
