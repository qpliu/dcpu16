-- DCPU-16 Computing Machine
-- DCPU-16 - version 1.1 of http://0x10c.com/doc/dcpu-16.txt.
-- Keyboard input buffer is at 0x9000-0x900f.
-- 40x25 screen buffer is at 0x8000-0x83f7.

import Control.Monad(when,zipWithM_)
import Data.Array.IO(IOUArray)
import Data.Array.MArray(newListArray,readArray,writeArray)
import Data.Bits((.&.),shiftR)
import qualified Data.ByteString as B
import Data.Char(chr,ord)
import Data.Word(Word8,Word16)
import System.Environment(getArgs)
import System.IO(stdin,hReady,BufferMode(NoBuffering),hSetBuffering,hSetEcho,hSetBinaryMode,hGetChar)
import Text.Printf(printf)
import UI.HSCurses.Curses(CursorVisibility(CursorInvisible),Pair(Pair),Color(Color),initCurses,startColor,resetParams,initPair,cursSet,wclear,stdScr,mvWAddStr,wMove,vline,wAttrSet,attr0,refresh,endWin,ulCorner,hLine)

import DCPU16(DCPU(DCPU,tick,readRegister,writeRegister,readRAM,writeRAM),Register,step)

main :: IO ()
main = do
    args <- getArgs
    (showRegisters,image) <- parseArgs args
    ram <- newListArray (minBound,maxBound) (image ++ [0,0..])
    registers <- newListArray (minBound,maxBound) [0,0..]
    initScreen ram
    run showRegisters ram registers

parseArgs :: [String] -> IO (Bool,[Word16])
parseArgs args
  | take 1 args == ["-r"] = fmap ((,) True) (readImage (tail args))
  | otherwise = fmap ((,) False) (readImage args)
  where
    readImage args
      | null args = return image0
      | not (null (tail args)) && head args == "-l" = do
            bytes <- B.readFile (head $ tail args)
            return (toWordsLE (B.unpack bytes))
      | otherwise = do
            bytes <- B.readFile (head args)
            return (toWords (B.unpack bytes))
    toWords [] = []
    toWords [b] = [fromIntegral b * 256]
    toWords (b1:b2:bs) = fromIntegral b1 * 256 + fromIntegral b2 : toWords bs
    toWordsLE [] = []
    toWordsLE [b] = [fromIntegral b]
    toWordsLE (b1:b2:bs) = fromIntegral b1 + fromIntegral b2 * 256 : toWordsLE bs

run :: Bool -> IOUArray Word16 Word16 -> IOUArray Register Word16 -> IO ()
run showRegisters ram registers = runStep keyboard
  where
    runStep index = do
        step dcpu
        if showRegisters
          then do
            wAttrSet stdScr (attr0,Pair 0)
            zipWithM_ displayRegister [minBound .. maxBound] [5..]
          else return ()
        ready <- hReady stdin
        if ready
          then do
            refresh
            newIndex <- readInput ram index
            maybe endWin runStep newIndex
          else runStep index
    dcpu = DCPU {
        tick = return (),
        readRegister = readArray registers,
        writeRegister = writeArray registers,
        readRAM = readArray ram,
        writeRAM = writeMemory
        }
    writeMemory addr word = do
        writeArray ram addr word
        updateScreen True ram addr
    displayRegister register y = do
        word <- readArray registers register
        mvWAddStr stdScr y 3 $ printf "%2s:%04x" (show register) word

initScreen :: IOUArray Word16 Word16 -> IO ()
initScreen ram = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hSetBinaryMode stdin True
    initCurses
    startColor
    resetParams
    sequence_ [initPair (Pair (fore+back*8+1)) (Color fore) (Color back) | fore <- [0..7], back <- [0..7]]
    cursSet CursorInvisible
    wclear stdScr
    wAttrSet stdScr (attr0,Pair 56)
    mvWAddStr stdScr 2 19 $ replicate 42 ' '
    wMove stdScr 3 19
    vline ' ' 25
    wMove stdScr 3 60
    vline ' ' 25
    mvWAddStr stdScr 28 19 $ replicate 42 ' '
    mvWAddStr stdScr 2 23 "DCM I"
    mapM_ (updateScreen False ram) [screen .. screen + screenSize - 1]
    refresh

updateScreen :: Bool -> IOUArray Word16 Word16 -> Word16 -> IO ()
updateScreen doRefresh ram addr
  | addr < screen || addr >= screen + screenSize = return ()
  | otherwise = do
        ch <- readArray ram addr
        wAttrSet stdScr (attr0,Pair (cellColor ch))
        mvWAddStr stdScr (3 + y) (20 + x) [chr (max 32 (fromIntegral ch .&. 0x7f))]
        when doRefresh refresh
  where
    x = fromIntegral $ (addr - screen) `mod` screenWidth
    y = fromIntegral $ (addr - screen) `div` screenWidth
    cellColor ch
      | ch .&. 0x80 == 0 = 8
      | otherwise = 1 + fromIntegral (shiftR ch 12 .&. 7) + 8*fromIntegral (shiftR ch 8 .&. 7)

readInput :: IOUArray Word16 Word16 -> Word16 -> IO (Maybe Word16)
readInput ram index = do
    ch <- hGetChar stdin
    if ch == '\3'
      then return Nothing
      else do
        writeArray ram index (fromIntegral (ord ch))
        if index + 1 >= keyboard + keyboardSize
          then return (Just keyboard)
          else return (Just (index + 1))

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
  0x7dc1, 0x0034,        -- 00: SET PC, 0034 ;init
                         --  loop0:
  0x7ca1, 0x07a0,        -- 02: SET [C], 07a0
                         --  loop1:
  0x2401,                -- 04: SET A, [B]
  0x800c,                -- 05: IFE A, 00
  0x8dc3,                -- 06: SUB PC, 03 ;loop1
  0x7ca1, 0x74a0,        -- 07: SET [C], 74a0
  0x8091,                -- 09: SET [B], 00
  0x8412,                -- 0a: ADD B, 01
  0x7c19, 0xf00f,        -- 0b: AND B, f00f
  0x7c09, 0x007f,        -- 0d: AND A, 007f
  0xb40c,                -- 0f: IFE A, 0d
  0xb9c2,                -- 10: ADD PC, 0e ;scroll
  0x01fe, 0x0020,        -- 11: IFG 0020, A
  0x89c1,                -- 13: SET PC, 02 ;loop0
  0x7c0c, 0x007f,        -- 14: IFE A, 007f
  0x7dc1, 0x0030,        -- 16: SET PC, 0030 ;backspace
  0x7c0a, 0x7480,        -- 18: BOR A, 7480
  0x00a1,                -- 1a: SET [C], A
  0x8422,                -- 1b: ADD C, 01
  0x09fe, 0x83e8,        -- 1c: IFG 83e8, C
  0x89c1,                -- 1e: SET PC, 02 ;loop0
                         --  scroll:
  0x7c21, 0x83c0,        -- 1f: SET C, 83c0
  0x7c61, 0x8000,        -- 21: SET I, 8000
                         --  loop2:
  0x58e1, 0x0028,        -- 23: SET [I], [0028+I]
  0x8462,                -- 25: ADD I, 01
  0x19fe, 0x83c0,        -- 26: IFG 83c0, I
  0x99c3,                -- 28: SUB PC, 0006 ;loop2
                         --  loop3:
  0x7ce1, 0x74a0,        -- 29: SET [I], 74a0
  0x8462,                -- 2b: ADD I, 01
  0x19fe, 0x83e8,        -- 2c: IFG 83e8, I
  0x99c3,                -- 2e: SUB PC, 06 ;loop3
  0x89c1,                -- 2f: SET PC, 02 ;loop0
                         --  backspace:
  0x7c2e, 0x83c0,        -- 30: IFG C, 83c0
  0x8423,                -- 32: SUB C, 01
  0x89c1,                -- 33: SET PC, 02 ;loop0
                         --  init:
  0x7c21, 0x8000,        -- 34: SET C, 8000
                         --  loop4:
  0x7ca1, 0x74a0,        -- 36: SET [C], 74a0
  0x8422,                -- 38: ADD C, 01
  0x09fe, 0x83e8,        -- 39: IFG 83e8, C
  0x99c3,                -- 3b: SUB PC, 6 ;loop4
                         --  loop5:
  0x4101, 0x8398, 0x0048,-- 3c: SET [8398+A], [0048+A] ;data
  0x8402,                -- 3f: ADD A, 01
  0x810d, 0x0048,        -- 40: IFN [0048+A], 00 ;data
  0x9dc3,                -- 42: SUB PC, 07 ;loop5
  0x7c11, 0x9000,        -- 43: SET B, 9000
  0x7c21, 0x83c0,        -- 45: SET C, 83c0
  0x89c1,                -- 47: SET PC, 02 ;loop0
                         --  data:
                         -- 48:
  0x64d2, 0x64e5, 0x64e1, 0x64e4, 0x64f9]
