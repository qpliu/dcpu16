-- DCPU-16 with single word input at 0x9000 and 40x25 screen at 0x8c00-0x8cf7
-- Will redo this with curses.
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
    forkIO $ forever $ threadDelay 1000000 >> dumpScreen ram
    forever $ fmap (fromIntegral . ord) getChar >>= writeChan input

run :: IOArray Word16 Word16 -> IOArray Register Word16 -> Chan Word16 -> Int -> IO ()
run ram registers input period = forever $ step dcpu
  where
    dcpu = DCPU {
        tick = threadDelay . (* period),
        readRegister = readArray registers,
        writeRegister = writeArray registers,
        readRAM = readRAMIO,
        writeRAM = writeArray ram
        }
    readRAMIO addr
      | addr == io = do
            empty <- isEmptyChan input
            if empty
              then return 0
              else readChan input
      | otherwise = readArray ram addr
    io = 0x9000

readImage :: [String] -> IO [Word16]
readImage args
  | null args =
        -- 0: 7801 9000 ; SET A, [0x9000]
        -- 2: fc0e      ; IFG A, 0x1f
        -- 3: 01e1 8c00 ; SET [0x8c00], A
        -- 5: 81c1      ; SET PC, 0
        return [0x7801, 0x9000, 0xfc0e, 0x01e1, 0x8c00, 0x81c1]
  | otherwise = do
        bytes <- B.readFile (head args)
        return (toWords (B.unpack bytes))
  where
    toWords [] = []
    toWords [b] = [fromIntegral b * 256]
    toWords (b1:b2:bs) = fromIntegral b1 * 256 + fromIntegral b2 : toWords bs

screen :: Word16
screen = 0x8c00

screenWidth :: Word16
screenWidth = 40

screenHeight :: Word16
screenHeight = 25

screenSize :: Word16
screenSize = screenWidth*screenHeight

dumpScreen :: IOArray Word16 Word16 -> IO ()
dumpScreen ram = do
    putStrLn (chr 0x250c : replicate 40 (chr 0x2500) ++ [chr 0x2510])
    mapM_ dumpLine [screen,screen+40 .. screen+screenSize-1]
    putStrLn (chr 0x2514 : replicate 40 (chr 0x2500) ++ [chr 0x2518])
  where
    dumpLine addr = do
        words <- mapM (readArray ram) [addr .. addr+39]
        putStrLn (chr 0x2502 : map (chr . max 32 . fromIntegral) words ++ [chr 0x2502])
