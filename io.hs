-- DCPU-16 with single word I/O at 0x9000
import Control.Concurrent(Chan,newChan,readChan,writeChan,isEmptyChan,forkIO,threadDelay)
import Control.Monad(forever)
import Data.Array.IO(IOArray)
import Data.Array.MArray(newListArray,readArray,writeArray)
import qualified Data.ByteString as B
import Data.Char(chr,ord)
import Data.Word(Word8,Word16)
import System.Environment(getArgs)

import DCPU16(DCPU(..),Register,step)

main :: IO ()
main = do
    args <- getArgs
    image <- readImage args
    ram <- newListArray (minBound,maxBound) (image ++ [0,0..])
    registers <- newListArray (minBound,maxBound) [0,0..]
    input <- newChan
    forkIO $ run ram registers input 1000 -- 1 kHz
    forever $ fmap (fromIntegral . ord) getChar >>= writeChan input

run :: IOArray Word16 Word16 -> IOArray Register Word16 -> Chan Word16 -> Int -> IO ()
run ram registers input period = forever $ step dcpu
  where
    dcpu = DCPU {
        tick = threadDelay . (* period),
        readRegister = readArray registers,
        writeRegister = writeArray registers,
        readRAM = readRAMIO,
        writeRAM = writeRAMIO
        }
    readRAMIO addr
      | addr == io = do
            empty <- isEmptyChan input
            if empty
              then return 0
              else readChan input
      | otherwise = readArray ram addr
    writeRAMIO addr word
      | addr == io = putChar (chr (fromIntegral word))
      | otherwise = writeArray ram addr word
    io = 0x9000

readImage :: [String] -> IO [Word16]
readImage args
  | null args =
        -- 0: 7801 9000 ; SET A, [0x9000]
        -- 2: 020d      ; IFN A, 0
        -- 3: 01e1 9000 ; SET [0x9000], A
        -- 5: 81c1      ; SET PC, 0
        return [0x7801, 0x9000, 0x020d, 0x01e1, 0x9000, 0x81c1]
  | args == ["/"] =
        -- 0: 9c01      ; SET A, 7
        -- 1: 808c      ; IFE [A], 0
        -- 2: 85c1      ; SET PC, 1
        -- 3: 21e1 9000 ; SET [0x9000], [A]
        -- 5: 8402      ; ADD A, 1
        -- 6: 85c1      ; SET PC, 1
        return [0x9c01, 0x808c, 0x85c1, 0x21e1, 0x9000, 0x8402, 0x85c1,
                0x0048, 0x0045, 0x004c, 0x004c, 0x004f, 0x0020, 0x0057,
                0x004f, 0x0052, 0x004c, 0x0044, 0x0021, 0x000d, 0x000a]
  | otherwise = do
        bytes <- B.readFile (head args)
        return (toWords (B.unpack bytes))
  where
    toWords [] = []
    toWords [b] = [fromIntegral b * 256]
    toWords (b1:b2:bs) = fromIntegral b1 * 256 + fromIntegral b2 : toWords bs
