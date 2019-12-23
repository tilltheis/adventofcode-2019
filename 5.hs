import Data.List.Split (splitOn)
import Data.Array as Array
import Data.Functor (fmap)

answer1 :: IO ()
answer1 = do
  output <- execProgram inputArray
  putStrLn $ show output

execProgram :: Array Int Int -> IO [Int]
execProgram memory = execProgram' memory 0 [] <&> reverse

execProgram' :: Array Int Int -> Int -> [Int] -> IO [Int]
execProgram' memory instructionPointer output | memory ! instructionPointer == 99 = return output
                                              | otherwise                         = do
    (newMemory, newInstructionPointer, newOutput) <- case opcode of
      1  -> return (storeValue 3 $ instruction2 (+) 1 2, instructionPointer + 4, output)
      2  -> return (storeValue 3 $ instruction2 (*) 1 2, instructionPointer + 4, output)
      3  -> putStr "> " >> getLine <&> \line -> (storeValue 1 (read line :: Int), instructionPointer + 2, output)
      4  -> return (memory, instructionPointer + 2,  (param 1) : output)
      x -> error $ "cannot handle opcode " ++ show x ++ logString
    execProgram' newMemory newInstructionPointer newOutput
  where
    instruction2 op parameter1Offset parameter2Offset = param parameter1Offset `op` param parameter2Offset
    storeValue addressParameterOffset value = memory // [(memory ! (instructionPointer + addressParameterOffset), value)]
    opcode = (memory ! instructionPointer) `mod` 100
    mode i n = n `quot` 100 `mod` 10^i `quot` 10^(i-1)
    paramMode paramOffset = case mode paramOffset (memory ! instructionPointer) of
      0 -> PositionMode
      1 -> ImmediateMode
      x -> error $ "cannot handle param mode " ++ show x ++ logString
    logString = "(output = " ++ show output ++ "instructionPointer = " ++ show instructionPointer ++ ", instruction=" ++ show (memory ! instructionPointer) ++ ")"
    param parameterOffset = case paramMode parameterOffset of
      ImmediateMode -> memory ! (instructionPointer + parameterOffset)
      PositionMode -> (memory ! (memory ! (instructionPointer + parameterOffset)))

data Mode = ImmediateMode | PositionMode

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

inputArray = Array.listArray (0, length inputList - 1) inputList

inputList :: [Int]
inputList = map read $ splitOn "," inputString

inputString = "3,225,1,225,6,6,1100,1,238,225,104,0,1102,68,5,225,1101,71,12,225,1,117,166,224,1001,224,-100,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1001,66,36,224,101,-87,224,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,26,51,225,1102,11,61,224,1001,224,-671,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,59,77,224,101,-136,224,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,1101,11,36,225,1102,31,16,225,102,24,217,224,1001,224,-1656,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,101,60,169,224,1001,224,-147,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,38,69,225,1101,87,42,225,2,17,14,224,101,-355,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1002,113,89,224,101,-979,224,224,4,224,1002,223,8,223,1001,224,7,224,1,224,223,223,1102,69,59,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,7,677,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,1108,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,404,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,434,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1006,224,494,101,1,223,223,107,677,677,224,102,2,223,223,1005,224,509,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,524,1001,223,1,223,8,226,677,224,102,2,223,223,1005,224,539,101,1,223,223,107,677,226,224,102,2,223,223,1005,224,554,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,569,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,584,1001,223,1,223,1108,226,226,224,102,2,223,223,1005,224,599,1001,223,1,223,1107,677,677,224,1002,223,2,223,1006,224,614,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,108,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226"
