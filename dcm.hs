-- DCPU-16 Computing Machine
-- DCPU-16 - version 1.1 of http://0x10c.com/doc/dcpu-16.txt.
-- Keyboard input buffer is at 0x9000-0x900f.
-- 40x25 screen buffer is at 0x8000-0x83f7.

import Control.Concurrent(MVar,newMVar,putMVar,takeMVar,tryPutMVar,forkIO,yield)
import Control.Monad(forever,when,void)
import Data.Array.IO(IOArray)
import Data.Array.MArray(newListArray,readArray,writeArray)
import Data.Bits((.&.),shiftR)
import qualified Data.ByteString as B
import Data.Char(chr,ord)
import Data.Word(Word8,Word16)
import System.Environment(getArgs)
import UI.HSCurses.Curses(CursorVisibility(CursorInvisible),Key(KeyChar),Pair(Pair),Color(Color),initCurses,startColor,resetParams,initPair,cursSet,wclear,stdScr,mvWAddStr,wMove,vline,wAttrSet,attr0,refresh,update,getCh,noDelay,endWin)

import DCPU16(DCPU(DCPU,tick,readRegister,writeRegister,readRAM,writeRAM),Register,step)

main :: IO ()
main = do
    args <- getArgs
    image <- readImage args
    ram <- newListArray (minBound,maxBound) (image ++ [0,0..])
    registers <- newListArray (minBound,maxBound) [0,0..]
    output <- newMVar ()
    initScreen
    forkIO $ run ram registers output
    updateScreen ram
    refresh
    forkIO $ forever $ (update >> takeMVar output >> updateScreen ram)
    done <- newMVar ()
    forkIO $ readInput ram keyboard done
    putMVar done ()
    endWin

readImage :: [String] -> IO [Word16]
readImage args
  | null args = return image0
  | otherwise = do
        bytes <- B.readFile (head args)
        return (toWords (B.unpack bytes))
  where
    toWords [] = []
    toWords [b] = [fromIntegral b * 256]
    toWords (b1:b2:bs) = fromIntegral b1 * 256 + fromIntegral b2 : toWords bs

run :: IOArray Word16 Word16 -> IOArray Register Word16 -> MVar () -> IO ()
run ram registers output = forever $ step dcpu
  where
    dcpu = DCPU {
        tick = const yield,
        readRegister = readArray registers,
        writeRegister = writeArray registers,
        readRAM = readArray ram,
        writeRAM = writeRAMIO
        }
    writeRAMIO addr word = do
        writeArray ram addr word
        when (addr >= screen && addr < screen + screenSize) (void $ tryPutMVar output ())

initScreen :: IO ()
initScreen = do
    initCurses
    startColor
    resetParams
    sequence_ [initPair (Pair (fore+back*8+1)) (Color fore) (Color back) | fore <- [0..7], back <- [0..7]]
    cursSet CursorInvisible
    noDelay stdScr True
    wclear stdScr
    mvWAddStr stdScr 1 21 "+-------+"
    mvWAddStr stdScr 2 19 ("+-+       +" ++ replicate 30 '-' ++ "+")
    wMove stdScr 3 19
    vline '|' 25
    wMove stdScr 3 60
    vline '|' 25
    mvWAddStr stdScr 28 19 ("+" ++ replicate 40 '-' ++ "+")
    mvWAddStr stdScr 2 23 "DCM I"

updateScreen :: IOArray Word16 Word16 -> IO ()
updateScreen ram = do
    sequence_ [updateCell x y | x <- [0..screenWidth-1], y <- [0..screenHeight-1]]
  where
    updateCell x y = do
        ch <- readArray ram (screen + y*screenWidth + x)
        wAttrSet stdScr (attr0,Pair (cellColor ch))
        mvWAddStr stdScr (3 + fromIntegral y) (20 + fromIntegral x) [chr (max 32 (fromIntegral ch .&. 0x7f))]
    cellColor ch
      | ch .&. 0x80 == 0 = 8
      | otherwise = 1 + fromIntegral (shiftR ch 12 .&. 7) + 8*fromIntegral (shiftR ch 8 .&. 7)

readInput :: IOArray Word16 Word16 -> Word16 -> MVar () -> IO ()
readInput ram index done
  | index >= keyboard + keyboardSize = readInput ram keyboard done
  | otherwise = do
    ch <- getCh
    case ch of
      KeyChar '\3' -> takeMVar done
      KeyChar c -> do
        writeArray ram index (fromIntegral (ord c))
        readInput ram (index + 1) done
      _ -> readInput ram (index + 1) done

screen :: Word16
screen = 0x8000

screenWidth :: Word16
screenWidth = 40

screenHeight :: Word16
screenHeight = 25

screenSize :: Word16
screenSize = screenWidth*screenHeight

keyboard :: Word16
keyboard = 0x9000

keyboardSize :: Word16
keyboardSize = 0x10

image0 :: [Word16]
image0 = [
  0x7dc1, 0x002c,        -- 00: SET PC, 002c ;init
                         --  loop0:
  0x7ca1, 0x07a0,        -- 02: SET [C], 07a0
                         --  loop1:
  0x2401,                -- 04: SET A, [B]
  0x800c,                -- 05: IFE A, 00
  0x8dc3,                -- 06: SUB PC, 03 ;loop1
  0x7ca1, 0x74a0,        -- 07: SET [C], 74a0
  0x8091,                -- 09: SET [B], 00
  0x8412,                -- 0a: ADD B, 1
  0x7c19, 0xf00f,        -- 0b: AND B, f00f
  0xb40c,                -- 0d: IFE A, 0d
  0xedc1,                -- 0e: SET PC, 1b ;scroll
  0x01fe, 0x0020,        -- 0f: IFG 0020, A
  0x89c1,                -- 11: SET PC, 02 ;loop0
  0x7c09, 0x007f,        -- 12: AND A, 007f
  0x7c0a, 0x7480,        -- 14: BOR A, 7480
  0x00a1,                -- 16: SET [C], A
  0x8422,                -- 17: ADD C, 1
  0x09fe, 0x83e8,        -- 18: IFG 83e8, C
  0x89c1,                -- 1a: SET PC, 02 ;loop0
                         --  scroll:
  0x7c21, 0x83c0,        -- 1b: SET C, 83c0
  0x7c61, 0x8000,        -- 1d: SET I, 8000
                         --  loop2:
  0x58e1, 0x0028,        -- 1f: SET [I], [0028+I]
  0x8462,                -- 21: ADD I, 1
  0x19fe, 0x83c0,        -- 22: IFG 83c0, I
  0x99c3,                -- 24: SUB PC, 0006 ;loop2
                         --  loop3:
  0x7ce1, 0x74a0,        -- 25: SET [I], 74a0
  0x8462,                -- 27: ADD I, 1
  0x19fe, 0x83e8,        -- 28: IFG 83e8, I
  0x99c3,                -- 2a: SUB PC, 06 ;loop3
  0x89c1,                -- 2b: SET PC, 02 ;loop0
                         --  init:
  0x7c21, 0x8000,        -- 2c: SET C, 8000
                         --  loop4:
  0x7ca1, 0x74a0,        -- 2e: SET [C], 74a0
  0x8422,                -- 30: ADD C, 01
  0x09fe, 0x83e8,        -- 31: IFG 83e8, C
  0x99c3,                -- 33: SUB PC, 6 ;loop4
                         --  loop5:
  0x4101, 0x8398, 0x0040,-- 34: SET [8398+A], [0040+A]
  0x8402,                -- 37: ADD A, 01
  0x810d, 0x0040,        -- 38: IFN [0040+A], 00
  0x9dc3,                -- 3a: SUB PC, 07 ;loop5
  0x7c11, 0x9000,        -- 3b: SET B, 9000
  0x7c21, 0x83c0,        -- 3d: SET C, 83c0
  0x89c1,                -- 3f: SET PC, 02 ;loop0
  0x64d2, 0x64e5, 0x64e1, 0x64e4, 0x64f9]
