{-# LANGUAGE OverloadedStrings,  ExistentialQuantification, DeriveDataTypeable #-}

module Main where

import Prelude as P
import Data.ByteString as B
import Data.ByteString.Lazy as BL

import Data.List as L (union, foldl1')
import Data.Maybe (catMaybes, listToMaybe)

import Control.Monad (when)

import Text.Printf.Mauke

import System.Console.CmdArgs
import System.Exit
import System.IO as IO

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
  when (P.null files) $ IO.hPutStrLn stderr "tinfo: Filename required." >> exitFailure
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
      sizelist = (fromBDataList . getFileSize) bd :: [Integer]
      date     = (maybe (0 :: Integer) id . fromBData . headBDataList . getCreateDate) bd
      comment  = (maybe ("" :: B.ByteString) id . fromBData . headBDataList . getComment) bd
      numfiles = P.length filelist
      totalfilesize = (sum . fromBDataList . getFileSize) bd :: Integer
      sum      = L.foldl1' (+) --Why isn't there a strict sum function?
      filesizelist = P.zip sizelist filelist

      fromBDataList :: forall a. (FromBData a) => [BData] -> [a]
      fromBDataList = catMaybes . P.map fromBData
      headBDataList = maybe (BString "") id . listToMaybe

      printFile (size, filename) = printf "   %7s  %s\n" (formatSize size) filename

  printf "%s\n" (takeFileName filename)
  printf "  Comment:  %s\n" comment
  printf "  Date:     %s\n" =<< formatDateTime (fromIntegral date)
  printf "  Trackers: %s\n" (B.intercalate ", " (L.union ann annlist))
  printf "  Size:     %s\n" (formatSize totalfilesize)
  printf "  Files:    %d\n" numfiles
  mapM_ printFile filesizelist
  printf "\n"

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
  | i < meg   = printf "%.1fK" (sizeFloat kilo)
  | i < gig   = printf "%.1fM" (sizeFloat meg)
  | i < peta  = printf "%.1fG" (sizeFloat gig)
  | otherwise = printf "%dG"   (sizeInt gig)
  where
    kilo = 1024
    meg  = kilo * 1024
    gig  = meg * 1024
    peta = gig * 1024

    sizeInt :: Integer -> Integer
    sizeInt size = i `div` size

    sizeFloat :: Integer -> Float
    sizeFloat size = fromIntegral i / fromIntegral size

