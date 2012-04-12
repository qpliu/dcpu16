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
import Data.Maybe(listToMaybe)
import Data.Word(Word8,Word16)
import System.Environment(getArgs)
import System.IO(stdin,hReady,BufferMode(NoBuffering),hSetBuffering,hSetEcho,hSetBinaryMode,hGetChar)
import Text.Printf(printf)
import UI.HSCurses.Curses(CursorVisibility(CursorInvisible),Pair(Pair),Color(Color),initCurses,startColor,resetParams,initPair,cursSet,wclear,stdScr,mvWAddStr,wMove,vline,wAttrSet,attr0,refresh,endWin,beep)

import DCPU16(DCPU(DCPU,tick,readRegister,writeRegister,readRAM,writeRAM),Register,step)

data DCMState = DCMState {
    screenAddr, screenWidth, screenHeight :: Word16,
    keyBuffer, keyBufferSize, keyBufferIndex :: Word16,
    dumpRAM :: Bool,
    dumpRAMAddr :: Word16,
    dumpRegisters :: Bool,
    refreshNextStep :: Bool,
    readCmd :: Bool
    }

main :: IO ()
main = do
    args <- getArgs
    (dcmState,image) <- parseArgs args
    ram <- newListArray (minBound,maxBound) (image ++ [0,0..])
    registers <- newListArray (minBound,maxBound) [0,0..]
    initScreen ram dcmState
    run ram registers dcmState

parseArgs :: [String] -> IO (DCMState,[Word16])
parseArgs args = parseArgs' toWordsBE defaultDCMState args
  where
    parseArgs' toWords dcmState args
      | null args = return (dcmState,image0 dcmState)
      | head args == "-le" = parseArgs' toWordsLE dcmState (tail args)
      | head args == "-be" = parseArgs' toWordsBE dcmState (tail args)
      | parseScreenSize (head args) /= Nothing = parseArgs' toWords (setScreenSize dcmState (parseScreenSize (head args))) (tail args)
      | otherwise = readImage dcmState (head args) toWords
    defaultDCMState = DCMState {
        screenAddr = 0x8000, screenWidth = 40, screenHeight = 25,
        keyBuffer = 0x9000, keyBufferSize = 16, keyBufferIndex = 0,
        dumpRAM = False, dumpRAMAddr = 0, dumpRegisters = False,
        refreshNextStep = False, readCmd = False
        }
    readImage dcmState filename toWords = do
        bytes <- B.readFile filename
        return (dcmState,toWords (B.unpack bytes))
    toWordsBE [] = []
    toWordsBE [b] = [fromIntegral b * 256]
    toWordsBE (b1:b2:bs) = fromIntegral b1 * 256 + fromIntegral b2 : toWordsBE bs
    toWordsLE [] = []
    toWordsLE [b] = [fromIntegral b]
    toWordsLE (b1:b2:bs) = fromIntegral b1 + fromIntegral b2 * 256 : toWordsLE bs
    parseScreenSize arg = do
        arg <- if take 1 arg == "-" then Just (tail arg) else Nothing
        (w,arg) <- listToMaybe (readsPrec 0 arg)
        arg <- if take 1 arg == "x" then Just (tail arg) else Nothing
        (h,arg) <- listToMaybe (readsPrec 0 arg)
        if arg == "" then Just (max 16 w,max 1 h) else Nothing
    setScreenSize dcmState Nothing = dcmState
    setScreenSize dcmState (Just (w,h)) = dcmState { screenWidth = w, screenHeight = h }

run :: IOUArray Word16 Word16 -> IOUArray Register Word16 -> DCMState -> IO ()
run ram registers dcmState = do
    step dcpu
    wAttrSet stdScr (attr0,Pair 0)
    mapM_ displayClear [5..21]
    when (dumpRegisters dcmState) $ zipWithM_ displayRegister [minBound .. maxBound] [5..21]
    when (dumpRAM dcmState) $ zipWithM_ displayRAM [dumpRAMAddr dcmState, dumpRAMAddr dcmState + 2 ..] [5..21]
    ready <- hReady stdin
    if ready
      then do
        refresh
        ch <- hGetChar stdin
        newDCMState <- readInput ram dcmState ch
        maybe endWin (run ram registers) newDCMState
      else do
        when (refreshNextStep dcmState) refresh
        run ram registers dcmState { refreshNextStep = False }
  where
    dcpu = DCPU {
        tick = return (),
        readRegister = readArray registers,
        writeRegister = writeArray registers,
        readRAM = readArray ram,
        writeRAM = writeMemory
        }
    writeMemory addr word = do
        writeArray ram addr word
        updateScreen True dcmState ram addr
    displayClear y = mvWAddStr stdScr y 3 "              "
    displayRegister register y = do
        word <- readArray registers register
        mvWAddStr stdScr y 3 $ printf "  %2s:%04x     " (show register) word
    displayRAM addr y = do
        word1 <- readArray ram addr
        word2 <- readArray ram (addr + 1)
        mvWAddStr stdScr y 3 $ printf "%04x:%04x %04x" addr word1 word2

initScreen :: IOUArray Word16 Word16 -> DCMState -> IO ()
initScreen ram dcmState = do
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
    mvWAddStr stdScr 2 19 $ replicate (2 + fromIntegral (screenWidth dcmState)) ' '
    wMove stdScr 3 19
    vline ' ' (fromIntegral (screenHeight dcmState))
    wMove stdScr 3 (fromIntegral (20 + screenWidth dcmState))
    vline ' ' (fromIntegral (screenHeight dcmState))
    mvWAddStr stdScr (fromIntegral (3 + screenHeight dcmState)) 19 $ replicate (fromIntegral (2 + screenWidth dcmState)) ' '
    mvWAddStr stdScr 2 23 "DCM I"
    mapM_ (updateScreen False dcmState ram) [screenAddr dcmState .. screenAddr dcmState + screenWidth dcmState * screenHeight dcmState - 1]
    refresh

updateScreen :: Bool -> DCMState -> IOUArray Word16 Word16 -> Word16 -> IO ()
updateScreen doRefresh dcmState ram addr
  | addr < screenStart || addr >= screenEnd = return ()
  | otherwise = do
        ch <- readArray ram addr
        wAttrSet stdScr (attr0,Pair (cellColor ch))
        mvWAddStr stdScr (3 + y) (20 + x) [chr (max 32 (fromIntegral ch .&. 0x7f))]
        when doRefresh refresh
  where
    screenStart = screenAddr dcmState
    screenEnd = screenAddr dcmState + screenHeight dcmState*screenWidth dcmState
    x = fromIntegral $ (addr - screenStart) `mod` screenWidth dcmState
    y = fromIntegral $ (addr - screenStart) `div` screenWidth dcmState
    cellColor ch
      | ch .&. 0x80 == 0 = 8
      | otherwise = 1 + fromIntegral (shiftR ch 12 .&. 7) + 8*fromIntegral (shiftR ch 8 .&. 7)

readInput :: IOUArray Word16 Word16 -> DCMState -> Char -> IO (Maybe DCMState)
readInput ram dcmState ch
  | ch == '\3' && readCmd dcmState = return Nothing
  | ch == 'c' && readCmd dcmState = return (Just dcmState { dumpRegisters = False, dumpRAM = False, readCmd = False, refreshNextStep = True })
  | ch == 'r' && readCmd dcmState = return (Just dcmState { dumpRegisters = True, dumpRAM = False, readCmd = False, refreshNextStep = True })
  | ch == 'm' && readCmd dcmState = return (Just dcmState { dumpRegisters = False, dumpRAM = True, readCmd = False, refreshNextStep = True })
  | ch == '\14' && readCmd dcmState = scrollRAM 1
  | ch == '\16' && readCmd dcmState = scrollRAM (-1)
  | ch == 'n' && readCmd dcmState = scrollRAM 16
  | ch == 'p' && readCmd dcmState = scrollRAM (-16)
  | ch == 'N' && readCmd dcmState = scrollRAM 0x100
  | ch == 'P' && readCmd dcmState = scrollRAM (-0x100)
  | ch == '\2' && readCmd dcmState = scrollRAM (-0x1000)
  | ch == '\6' && readCmd dcmState = scrollRAM 0x1000
  | ch == 'b' && readCmd dcmState = scrollRAM (-0x2000)
  | ch == 'f' && readCmd dcmState = scrollRAM 0x2000
  | ch == 'B' && readCmd dcmState = scrollRAM (-0x4000)
  | ch == 'F' && readCmd dcmState = scrollRAM 0x4000
  | ch == '\24' && readCmd dcmState = insertChar dcmState { readCmd = False }
  | readCmd dcmState = do
        beep
        return (Just dcmState { readCmd = False })
  | ch == '\24' = return (Just dcmState { readCmd = True })
  | otherwise = insertChar dcmState
  where
    keyBufferAddr = keyBuffer dcmState + keyBufferIndex dcmState
    insertChar dcmState = do
        word <- readArray ram keyBufferAddr
        if word /= 0
          then do
            beep
            return (Just dcmState)
          else do
            writeArray ram keyBufferAddr (fromIntegral (ord ch))
            return (Just dcmState { keyBufferIndex = (keyBufferIndex dcmState + 1) `mod` keyBufferSize dcmState })
    scrollRAM count = return (Just dcmState { dumpRegisters = False, dumpRAM = True, dumpRAMAddr = dumpRAMAddr dcmState + count, readCmd = False, refreshNextStep = True })

image0 :: DCMState -> [Word16]
image0 dcmState = [
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
  0x7c19, 0xf00f,        -- 0b: AND B, f00f -- assume keyBuffer, keyBufferSize
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
  0x09fe, screenEnd,     -- 1c: IFG 83e8, C
  0x89c1,                -- 1e: SET PC, 02 ;loop0
                         --  scroll:
  0x7c21, screenLastLine,-- 1f: SET C, 83c0
  0x7c61, screenStart,   -- 21: SET I, 8000
                         --  loop2:
  0x58e1, screenWidth dcmState,
                         -- 23: SET [I], [0028+I]
  0x8462,                -- 25: ADD I, 01
  0x19fe, screenLastLine,-- 26: IFG 83c0, I
  0x99c3,                -- 28: SUB PC, 0006 ;loop2
                         --  loop3:
  0x7ce1, 0x74a0,        -- 29: SET [I], 74a0
  0x8462,                -- 2b: ADD I, 01
  0x19fe, screenEnd,     -- 2c: IFG 83e8, I
  0x99c3,                -- 2e: SUB PC, 06 ;loop3
  0x89c1,                -- 2f: SET PC, 02 ;loop0
                         --  backspace:
  0x7c2e, screenLastLine,-- 30: IFG C, 83c0
  0x8423,                -- 32: SUB C, 01
  0x89c1,                -- 33: SET PC, 02 ;loop0
                         --  init:
  0x7c21, screenStart,   -- 34: SET C, 8000
                         --  loop4:
  0x7ca1, 0x74a0,        -- 36: SET [C], 74a0
  0x8422,                -- 38: ADD C, 01
  0x09fe, screenEnd,     -- 39: IFG 83e8, C
  0x99c3,                -- 3b: SUB PC, 6 ;loop4
                         --  loop5:
  0x4101, screenLastLine,
                  0x0047,-- 3c: SET [83c0+A], [0047+A] ;data
  0x8402,                -- 3f: ADD A, 01
  0x810d, 0x0047,        -- 40: IFN [0047+A], 00 ;data
  0x9dc3,                -- 42: SUB PC, 07 ;loop5
  0x7c11, keyStart,      -- 43: SET B, 9000
  0x7dc1, 0x001f,        -- 45: SET PC, 1f ;scroll
                         --  data:
                         -- 47:
  0x64d2, 0x64e5, 0x64e1, 0x64e4, 0x64f9]
  where
    screenStart = screenAddr dcmState
    screenEnd = screenAddr dcmState + screenWidth dcmState*screenHeight dcmState
    screenLastLine = screenEnd - screenWidth dcmState
    keyStart = keyBuffer dcmState
