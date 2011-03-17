{-# LANGUAGE PatternGuards, OverloadedStrings #-}

module BEParser (
  (>>>), (<+>),
  beString, beInt,
  beList, beListConcat,
  beAssocLookup, beAssocAll,
  deep, multi,
  getChildren,
  orElse,
  formatDateTime


) where

import Prelude as P
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as BL
import Data.Map as M
import BEDecode

--For date time conversion
import System.Locale
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Clock.POSIX


{-

rawDump :: FilePath -> IO ()
rawDump file = do
  x <- BL.readFile file
  case beDecode x of
    Nothing -> printf "Couldn't decode file."
    Just bd -> printf "%s\n" (show bd)

testfunc :: FilePath -> IO ()
testfunc file = do
  x <- BL.readFile file
  case beDecode x of
    Nothing -> return ()
    Just bd -> printSummary bd
  return ()
-}


(f >>> g) bd = P.concat [g bd' | bd' <- f bd]
(f <+> g) bd = f bd ++ g bd

type BEFilter = BData -> [BData]

beString :: BEFilter
beString str@(BString _) = [str]
beString _ = []

beInt :: BEFilter
beInt i@(BInt _) = [i]
beInt _ = []

beList :: BEFilter
beList (BList list) = list
beList _ = []

beListConcat :: B.ByteString -> BEFilter
beListConcat inter (BList list) = [P.foldr doConcat (BString B.empty) (P.reverse list)]
  where
    doConcat (BString str) (BString b)
      | B.null b = BString (b `B.append` str)
      | otherwise = BString (b `B.append` inter `B.append` str)
    doConcat _ b = b

  

beAssocLookup :: B.ByteString -> BEFilter
beAssocLookup str (BAssoc assoc)
  | Just x <- M.lookup str assoc = [x]
  | otherwise = []
beAssocLookup _ _ = []

beAssocAll :: BEFilter
beAssocAll (BAssoc assoc) = M.elems assoc
beAssocAll _ = []

orElse :: BEFilter -> BEFilter -> BEFilter
orElse f g bd
  | P.null (f bd) = g bd
  | otherwise = f bd


getChildren :: BEFilter
getChildren list@(BList _) = beList list
getChildren assoc@(BAssoc _) = beAssocAll assoc
getChildren _ = []

--Get the outermost elements under this filter.
deep :: BEFilter -> BEFilter
deep bd = bd `orElse` (getChildren >>> (deep bd))

--Get every element under this filter.  Values will repeat on their way down the tree unless you restrict to string or int.
multi :: BEFilter -> BEFilter
multi bd = bd <+> (getChildren >>> multi bd)


formatDateTime :: POSIXTime -> IO String
formatDateTime secs = do
  let utc = posixSecondsToUTCTime secs
  tz <- getTimeZone utc
  return $ formatTime defaultTimeLocale "%c" $ utcToZonedTime tz utc
