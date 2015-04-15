module Data.HashId.Internal where

import           Data.Char  (ord, toUpper)
import           Data.List  (elemIndex, foldl', intersect, nub, (\\))
import           Data.Maybe (fromMaybe)

-- Options for creating encoders
data HashOptions = HashOptions {
    optSalt          :: !Salt
  , optAlphabet      :: !Alphabet
  , optMinHashLength :: !Int } deriving Show

-- Necessary data for encoding and decoding
data HashEncoder = HashEncoder {
    encSalt          :: !Salt
  , encMinHashLength :: !Int
  , encAlphabet      :: !Alphabet
  , encSeps          :: !Separators
  , encGuards        :: !Guards
  } deriving Show

newtype HashId = HashId { unHashId :: String }

type Alphabet = String
type Salt = String
type Separators = String
type Guards = String
type Hashed = String
type MinLength = Int
type OptionErrors = String

defaultAlphabet :: Alphabet
defaultAlphabet =
  concat [
    ['a'..'z']
  , ['A'..'Z']
  , ['1'..'9']
  , "0"]

defaultSeps :: Separators
defaultSeps =
  let seps = "cfhistu"
  in seps ++ map toUpper seps

defaultMinHashLength :: Int
defaultMinHashLength = 0

defaultOptions :: Salt -> HashOptions
defaultOptions salt =
  case mkOptions salt defaultAlphabet defaultMinHashLength of
    Right opt -> opt
    Left err -> error err -- library failure

mkOptions :: Salt -> Alphabet -> MinLength -> Either OptionErrors HashOptions
mkOptions salt alphabet minLength =
  if length alphabet' < minAlphabetLength
     then Left $ "Alphabet must contain at least "
                 ++ show minAlphabetLength ++ " unique characters"
     else Right $ HashOptions salt alphabet' minLength'
  where minAlphabetLength = 16
        alphabet' = filter (/= ' ') $ nub alphabet
        minLength' = if minLength < 0
                        then 0
                        else minLength

mkEncoder :: HashOptions -> HashEncoder
mkEncoder (HashOptions salt alphabet minLength) =
  let seps = defaultSeps `intersect` alphabet
      alphabet' = alphabet \\ seps
      seps' = consistentShuffle seps salt
      divCoeff = fromIntegral (length alphabet) / fromIntegral (length seps')
      (seps'', alphabet'') =
        if seps' == "" || divCoeff > sepDiv
           then let sepsLen = ceiling $ fromIntegral (length alphabet') / sepDiv
                    sepsLen' = if sepsLen == 1
                                  then sepsLen + 1
                                  else sepsLen
                in if sepsLen' > length seps'
                      then let diff = sepsLen' - length seps'
                               (fstAlpha, sndAlpha) = splitAt diff alphabet'
                           in (seps' ++ fstAlpha, sndAlpha)
                      else (take sepsLen' seps', alphabet')
           else (alphabet', seps')
      alphabet''' = consistentShuffle alphabet'' salt
      guardCount = ceiling $ fromIntegral (length alphabet''') / guardDiv
      (alphabet'''', seps''', guards) =
        if length alphabet'' < 3
           then let (guards', seps'''') = splitAt guardCount seps''
                in (alphabet'', seps'''', guards')
           else let (guards', alphabet''''') = splitAt guardCount alphabet'''
                in (alphabet''''', seps'', guards')
  in HashEncoder salt minLength alphabet'''' seps''' guards

sepDiv :: Double
sepDiv = 3.5

guardDiv :: Double
guardDiv = 12

type ReturnVal = String

encode :: HashEncoder -> [Int] -> HashId
encode (HashEncoder salt minLength alphabet seps guards) nums =
  let hash' = sum $ map (\(i, n) -> n `mod` (i+100)) $ zip [0..] nums
      ret = alphabet !! (hash' `mod` length alphabet)
      (alphabet', retVal) = encodeStep1 ret alphabet salt seps nums
      retVal' = encodeStep2 hash' guards minLength retVal
      retVal'' = encodeStep3 alphabet' minLength retVal'
  in HashId retVal''

encodeStep1 :: Char
                -> Alphabet
                -> Salt
                -> Separators
                -> [Int]
                -> (Alphabet, ReturnVal)
encodeStep1 char alphabet salt seps nums =
  foldl' foldStep (alphabet, [char]) ns
  where ns = zip [0..] nums
        foldStep (alphabet', retVal) (i, n) =
          let buffer = char : salt ++ alphabet'
              alphabet'' = consistentShuffle alphabet' $
                           take (length alphabet') buffer
              lastC = hash alphabet'' n
              retVal' = retVal ++ lastC
              retVal'' = if i + 1 < length nums
                            then
                              let n' = n `mod` ord (head lastC) + i
                                  sepsIndex = n' `mod` length seps
                              in retVal' ++ [seps !! sepsIndex]
                            else retVal'
          in (alphabet'', retVal'')

encodeStep2 :: Int -> Guards -> Int -> ReturnVal -> ReturnVal
encodeStep2 hash' guards minLength retVal =
  if length retVal < minLength
     then
       let guardIndex = hash' + ord (head retVal)
           guard = guards !! guardIndex
           retVal' = guard:retVal
       in if length retVal' < minLength
             then
               let guardIndex' = (hash' + ord (retVal' !! 2)) `mod` length guards
                   guard' = guards !! guardIndex'
               in retVal' ++ [guard']
             else retVal'
     else retVal

encodeStep3 :: Alphabet -> Int -> ReturnVal -> ReturnVal
encodeStep3 alphabet minLength retVal =
  if length retVal < minLength
     then
       let alphabet' = consistentShuffle alphabet alphabet
           retVal' = drop halfLen alphabet' ++ retVal ++ take halfLen alphabet'
           excess = length retVal' - minLength
           retVal'' = if excess > 0
                         then let startPos = excess `div` 2
                              in take (startPos + minLength) $ drop startPos retVal'
                         else retVal'
       in encodeStep3 alphabet' minLength retVal''
     else retVal
  where halfLen = length alphabet `div` 2

decode :: HashEncoder -> HashId -> [Int]
decode enc@(HashEncoder salt _ alphabet seps guards) hashId =
  let hashBreakdown = words $ map (toSpace guards) $ unHashId hashId
      i = if length hashBreakdown `elem` [2, 3] then 1 else 0
      hashBreakdown' = hashBreakdown !! i
      lottery = head hashBreakdown'
      hashBreakdown'' = words $ map (toSpace seps) $ tail hashBreakdown'
      ret = fst $ foldl' (collectVals lottery) ([], alphabet) hashBreakdown''
  in if unHashId (encode enc ret) == unHashId hashId
        then ret
        else []
  where toSpace elems c =
          if c `elem` elems
             then ' '
             else c
        collectVals lottery' (vals, alphabet') subhash =
          let buffer = lottery' : salt ++ alphabet'
              alphabet'' = consistentShuffle alphabet'
                           $ take (length alphabet') buffer
              val = unhash alphabet'' subhash
          in (vals ++ [val], alphabet'')

parse :: HashEncoder -> String -> Maybe HashId
parse _ s = Just $ HashId s

toText :: HashId -> String
toText = unHashId

hash :: Alphabet -> Int -> Hashed
hash alphabet num = hash' num alphabet ""
  where
    hash' i a output =
      if i > 0
         then let aLen = length a
                  newOut = (a !! (i `mod` aLen)) : output
              in hash' (i `div` aLen) a newOut
      else output

unhash :: Alphabet -> Hashed -> Int
unhash alphabet hash' = unhash' 0 0
  where
    unhash' i num =
      if i >= length hash'
         then num
         else let cur = hash' !! i
                  pos = fromMaybe (-1) $ elemIndex cur alphabet
                  hashLen = length hash'
                  power = length alphabet ^ (hashLen - i - 1)
                  num' = num + pos * power
              in unhash' (i + 1) num'

consistentShuffle :: Alphabet -> Salt -> Alphabet
consistentShuffle alphabet salt =
  fst $ foldl' shuffleStep (alphabet, 0) $ zip is vs
  where
    vs = cycle [0..(length salt-1)]
    is = [(length alphabet-1), (length alphabet-2)..1]
    shuffleStep (alphabet', p) (i, v) =
      let ascVal = ord $ salt !! v
          p' = p + ascVal
          j = (ascVal + v + p') `mod` i
      in (swapAt i j alphabet', p')

replaceAt :: a -> Int -> [a] -> [a]
replaceAt x ix xs = take (ix-1) xs ++ [x] ++ drop ix xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt ixA ixB xs =
  let tmpA = xs !! ixA
      tmpB = xs !! ixB
      xs' = replaceAt tmpA (ixB+1) xs
      xs'' = replaceAt tmpB (ixA+1) xs'
  in xs''
