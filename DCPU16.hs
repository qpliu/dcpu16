-- | Implementation of version 1.1 of http://0x10c.com/doc/dcpu-16.txt.
module DCPU16(DCPU(..),Register(..),step) where

import Data.Bits((.&.),(.|.),shiftL,shiftR,xor)
import Data.Ix(Ix)
import Data.Word(Word16,Word32)

data Register = A | B | C | X | Y | Z | I | J | PC | SP | O
    deriving (Bounded,Eq,Ix,Ord)

-- For example, m could be IO, Control.Monad.ST.ST,
-- or Control.Concurrent.STM.STM.
-- Memory-mapped I/O would be implemented by readRAM and writeRAM.
data DCPU m = DCPU {
    tick :: Int -> m (),
    readRAM :: Word16 -> m Word16,
    writeRAM :: Word16 -> Word16 -> m (),
    readRegister :: Register -> m Word16,
    writeRegister :: Register -> Word16 -> m ()
    }

-- | Execute one instruction.
step :: Monad m => DCPU m -> m ()
step dcpu = do
    insn <- nextWord dcpu
    if insn .&. 0xf /= 0
      then do
        a <- arg dcpu ((insn `shiftR` 4) .&. 0x3f)
        b <- arg dcpu ((insn `shiftR` 10) .&. 0x3f)
        basic dcpu (insn .&. 0xf) a b
      else do
        a <- arg dcpu ((insn `shiftR` 10) .&. 0x3f)
        nonBasic dcpu ((insn `shiftR` 4) .&. 0x3f) a

basic :: Monad m => DCPU m -> Word16 -> Operand m -> Operand m -> m ()
basic dcpu opcode a b
  | opcode == 0x01 = fst b >>= snd a
  | opcode == 0x02 = overflow (+) (\ x y -> if x + y >= max x y then 0 else 1) 1
  | opcode == 0x03 = overflow (-) (\ x y -> if x >= y then 0 else -1) 1
  | opcode == 0x04 = overflow (*) (\ x y -> fromIntegral ((fromIntegral x * fromIntegral y :: Word32) `shiftR` 16)) 1
  | opcode == 0x05 = overflow (zerosafe div) (zerosafe (\ x y -> fromIntegral ((fromIntegral x `shiftL` 16) `div` fromIntegral y :: Word32))) 2
  | opcode == 0x06 = binop (zerosafe mod) 2
  | opcode == 0x07 = overflow (\ x y -> x `shiftL` fromIntegral y) (\ x y -> x `shiftR` (16 - fromIntegral y)) 1
  | opcode == 0x08 = overflow (\ x y -> x `shiftR` fromIntegral y) (\ x y -> x `shiftL` (16 - fromIntegral y)) 1
  | opcode == 0x09 = binop (.&.) 0
  | opcode == 0x0a = binop (.|.) 0
  | opcode == 0x0b = binop xor 0
  | opcode == 0x0c = branch (==) False
  | opcode == 0x0d = branch (==) True
  | opcode == 0x0e = branch (>) False
  | opcode == 0x0f = branch (.&.) 0
  | otherwise = error ("Unknown basic opcode " ++ show opcode)
  where
    zerosafe op x y = if y == 0 then 0 else op x y
    binop op cycles = do
        worda <- fst a
        wordb <- fst b
        snd a (worda `op` wordb)
        tick dcpu cycles
    overflow op overflowop cycles = do
        worda <- fst a
        wordb <- fst b
        snd a (worda `op` wordb)
        writeRegister dcpu O (worda `overflowop` wordb)
        tick dcpu cycles
    branch op skip = do
        worda <- fst a
        wordb <- fst b
        if worda `op` wordb == skip
          then do
            pc <- readRegister dcpu PC
            insn <- readRAM dcpu pc
            writeRegister dcpu PC (pc + fromIntegral (insnSize insn))
            tick dcpu 2
          else tick dcpu 1

nonBasic :: Monad m => DCPU m -> Word16 -> Operand m -> m ()
nonBasic dcpu opcode a
  | opcode == 0x01 = do
        word <- fst a
        pc <- readRegister dcpu PC
        sp <- readRegister dcpu SP
        writeRegister dcpu SP (sp - 1)
        writeRAM dcpu (sp - 1) pc
        writeRegister dcpu PC word
        tick dcpu 1
  | otherwise = error ("Unknown non-basic opcode " ++ show opcode)

register :: Word16 -> Register
register 0x0 = A
register 0x1 = B
register 0x2 = C
register 0x3 = X
register 0x4 = Y
register 0x5 = Z
register 0x6 = I
register 0x7 = J
register r = error ("Unknown register " ++ show r)

nextWord :: Monad m => DCPU m -> m Word16
nextWord dcpu = do
    tick dcpu 1
    pc <- readRegister dcpu PC
    writeRegister dcpu PC (pc + 1)
    readRAM dcpu pc

type Operand m = (m Word16,Word16 -> m ())

arg :: Monad m => DCPU m -> Word16 -> m (Operand m)
arg dcpu operand
  | operand < 0x08 = accessRegister (register operand)
  | operand < 0x10 = do
        r <- readRegister dcpu (register (operand .&. 0x07))
        accessRAM r
  | operand < 0x18 = do
        index <- readRegister dcpu (register (operand .&. 0x07))
        base <- nextWord dcpu
        accessRAM (index + base)
  | operand == 0x18 = do
        sp <- readRegister dcpu SP
        writeRegister dcpu SP (sp + 1)
        accessRAM sp
  | operand == 0x19 = do
        sp <- readRegister dcpu SP
        accessRAM sp
  | operand == 0x1a = do
        sp <- readRegister dcpu SP
        writeRegister dcpu SP (sp - 1)
        accessRAM (sp - 1)
  | operand == 0x1b = accessRegister SP
  | operand == 0x1c = accessRegister PC
  | operand == 0x1d = accessRegister O
  | operand == 0x1e = do
        word <- nextWord dcpu
        accessRAM word
  | operand == 0x1f = do
        word <- nextWord dcpu
        return (return word, const (return ()))
  | otherwise = return (return (operand .&. 0x1f), const (return ()))
  where
    accessRegister register = do
        word <- readRegister dcpu register
        return (return word, writeRegister dcpu register)
    accessRAM location =
        return (readRAM dcpu location, writeRAM dcpu location)

insnSize :: Word16 -> Int
insnSize insn =
    if insn .&. 0xf /= 0
      then 1 + argCycles ((insn `shiftR` 4) .&. 0x3f) + argCycles ((insn `shiftR` 10) .&. 0x3f)
      else 1 + argCycles ((insn `shiftR` 10) .&. 0x3f)

argCycles :: Word16 -> Int
argCycles operand
  | operand >= 0x10 && operand <= 0x17 = 1
  | operand == 0x1e || operand == 0x1f = 1
  | otherwise = 0
