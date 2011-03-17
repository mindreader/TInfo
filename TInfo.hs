{-# LANGUAGE OverloadedStrings,  ExistentialQuantification, DeriveDataTypeable #-}

module Main where

import Prelude as P
import Data.ByteString as B
import Data.ByteString.Lazy as BL

import Data.List as L (union, foldl1')
import Data.Maybe (catMaybes, listToMaybe)

import Text.Printf.Mauke

import System.Console.CmdArgs

import BEDecode
import BEParser

import System.FilePath (takeFileName)

data Options = Opt {
  color :: Bool,
  files :: [FilePath]
} deriving (Show, Data, Typeable)

options = Opt {
  color = def  &= help  "Color output",
  files = def &= args &= typ "FILES"
} &=  summary  "Display meta data of a torrent file."

main = do
  Opt color files <- cmdArgs options
  mapM_ actOnFile files
  where
    actOnFile file = do
      x <- BL.readFile file
      case beDecode x of
        Nothing -> return ()
        Just bd -> printSummary file bd

printSummary :: FilePath -> BData -> IO ()
printSummary filename bd = do
  let ann      = (fromBDataList . getMainAnnounce) bd
      annlist  = (fromBDataList . getAnnounceList) bd
      filelist = (fromBDataList . getFileList) bd :: [B.ByteString]
      date     = (maybe (0 :: Integer) id . fromBData . headDataList . getCreateDate) bd
      comment  = (maybe ("" :: B.ByteString) id . fromBData . headDataList . getComment) bd
      numfiles = P.length filelist
      filesize = (sum . fromBDataList . getFileSize) bd :: Integer
      sum      = L.foldl1' (+)

      fromBDataList :: forall a. (FromBData a) => [BData] -> [a]
      fromBDataList = catMaybes . P.map fromBData
      headDataList = maybe (BString "") id . listToMaybe

  printf "%s\n" (takeFileName filename)
  printf "  Name:     %s\n" comment
  printf "  Date:     %s\n" =<< formatDateTime (fromIntegral date)
  printf "  Trackers: %s\n" (B.intercalate ", " (L.union ann annlist))
  printf "  Size:     %s\n" (formatSize filesize)
  printf "  Files:    %d\n" numfiles
  mapM_ (printf "    %s\n") filelist
  B.putStrLn ""

getMainAnnounce = beAssocLookup "announce" >>> beString
getAnnounceList = beAssocLookup "announce-list" >>> deep beString
getComment      = beAssocLookup "comment" >>> beString
getCreateDate   = beAssocLookup "creation date" >>> beInt
getFileList     = complex `orElse` simple
  where
    simple = beAssocLookup "info" >>> beAssocLookup "name"
    complex = beAssocLookup "info" >>> beAssocLookup "files" >>> beList >>> deep (beAssocLookup "path") >>> beListConcat "/"
getFileSize    = complex `orElse` simple
  where
    simple  = beAssocLookup "info" >>> beAssocLookup "length" >>> beInt
    complex = beAssocLookup "info" >>> beAssocLookup "files" >>> deep (beAssocLookup "length") >>> beInt


formatSize :: Integer -> String
formatSize i
  | i < kilo  = printf "%dB" i
  | i < meg   = printf "%.2fKB" (sizeFloat kilo)
  | i < gig   = printf "%.2fMB" (sizeFloat meg)
  | i < peta  = printf "%.2fGB" (sizeFloat gig)
  | otherwise = printf "%dGB"   (sizeInt gig)
  where
    kilo = 1024
    meg  = kilo * 1024
    gig  = meg * 1024
    peta = gig * 1024

    sizeInt :: Integer -> Integer
    sizeInt size = i `div` size

    sizeFloat :: Integer -> Float
    sizeFloat size = fromIntegral i / fromIntegral size
