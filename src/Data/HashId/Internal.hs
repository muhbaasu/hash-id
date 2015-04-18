module Data.HashId.Internal where

import           Data.Char  (ord, toUpper)
import           Data.List  (elemIndex, foldl', intersect, nub, (\\))
import           Data.Maybe (fromMaybe)

-- Options for creating encoders
data HashOptions = HashOptions {
    optSalt          :: !Salt
  , optAlphabet      :: !Alphabet
  , optMinHashLength :: !MinLength
  } deriving Show

-- Necessary data for encoding and decoding
data HashEncoder = HashEncoder {
    encSalt          :: !Salt
  , encMinHashLength :: !MinLength
  , encAlphabet      :: !Alphabet
  , encSeps          :: !Separators
  , encGuards        :: !Guards
  } deriving Show

newtype HashId = HashId { unHashId :: String } deriving (Show, Eq)
newtype Salt = Salt { unSalt :: String } deriving (Show, Eq)
newtype MinLength = MinLength { unMinLength :: Int } deriving (Show, Eq)

type Alphabet = String
type Separators = String
type Guards = String
type Hashed = String
type Error = String

minLength :: Int -> Either Error MinLength
minLength l | l < 0 = Left ""
minLength l = Right $ MinLength l

satisfiesLength :: MinLength -> Int -> Bool
satisfiesLength (MinLength minLen) l = l >= minLen

-- TODO
salt :: String -> Either Error Salt
salt = undefined

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

defaultMinHashLength :: MinLength
defaultMinHashLength = MinLength 0

defaultOptions :: Salt -> HashOptions
defaultOptions salt' =
  case mkOptions salt' defaultAlphabet defaultMinHashLength of
    Right opt -> opt
    Left err -> error err -- library failure

mkOptions :: Salt -> Alphabet -> MinLength -> Either Error HashOptions
mkOptions salt' alphabet minLen =
  if length alphabet' < minAlphabetLength
     then Left $ "Alphabet must contain at least "
                 ++ show minAlphabetLength ++ " unique characters"
     else Right $ HashOptions salt' alphabet' minLen
  where minAlphabetLength = 16
        alphabet' = filter (/= ' ') $ nub alphabet

mkEncoder :: HashOptions -> HashEncoder
mkEncoder (HashOptions salt' alphabet minLen) =
  let seps = defaultSeps `intersect` alphabet
      alphabet' = alphabet \\ seps
      seps' = consistentShuffle seps salt'
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
      alphabet''' = consistentShuffle alphabet'' salt'
      guardCount = ceiling $ fromIntegral (length alphabet''') / guardDiv
      (alphabet'''', seps''', guards) =
        if length alphabet'' < 3
           then let (guards', seps'''') = splitAt guardCount seps''
                in (alphabet'', seps'''', guards')
           else let (guards', alphabet''''') = splitAt guardCount alphabet'''
                in (alphabet''''', seps'', guards')
  in HashEncoder salt' minLen alphabet'''' seps''' guards

sepDiv :: Double
sepDiv = 3.5

guardDiv :: Double
guardDiv = 12

type ReturnVal = String

encode :: HashEncoder -> [Int] -> HashId
encode (HashEncoder salt' minLen alphabet seps guards) nums =
  let hash' = sum $ map (\(i, n) -> n `mod` (i+100)) $ zip [0..] nums
      ret = alphabet !! (hash' `mod` length alphabet)
      (alphabet', retVal) = encodeStep1 ret alphabet salt' seps nums
      retVal' = encodeStep2 hash' guards minLen retVal
      retVal'' = encodeStep3 alphabet' minLen retVal'
  in HashId retVal''

encodeStep1 :: Char
                -> Alphabet
                -> Salt
                -> Separators
                -> [Int]
                -> (Alphabet, ReturnVal)
encodeStep1 char alphabet salt' seps nums =
  foldl' foldStep (alphabet, [char]) ns
  where ns = zip [0..] nums
        foldStep (alphabet', retVal) (i, n) =
          let buffer = char : unSalt salt' ++ alphabet'
              alphabet'' = consistentShuffle alphabet' $ Salt
                           $ take (length alphabet') buffer
              lastC = hash alphabet'' n
              retVal' = retVal ++ lastC
              retVal'' = if i + 1 < length nums
                            then
                              let n' = n `mod` ord (head lastC) + i
                                  sepsIndex = n' `mod` length seps
                              in retVal' ++ [seps !! sepsIndex]
                            else retVal'
          in (alphabet'', retVal'')

encodeStep2 :: Int -> Guards -> MinLength -> ReturnVal -> ReturnVal
encodeStep2 hash' guards minLen retVal =
  if satisfiesLength minLen $ length retVal
     then retVal
     else
       let guardIndex = hash' + ord (head retVal)
           guard = guards !! guardIndex
           retVal' = guard:retVal
       in if satisfiesLength minLen $ length retVal'
             then retVal'
             else
               let guardIndex' = (hash' + ord (retVal' !! 2)) `mod` length guards
                   guard' = guards !! guardIndex'
               in retVal' ++ [guard']

encodeStep3 :: Alphabet -> MinLength -> ReturnVal -> ReturnVal
encodeStep3 alphabet minLen retVal =
  if satisfiesLength minLen $ length retVal
     then retVal
     else
       let alphabet' = consistentShuffle alphabet $ Salt alphabet
           retVal' = drop halfLen alphabet' ++ retVal ++ take halfLen alphabet'
           excess = length retVal' - unMinLength minLen
           retVal'' = if excess > 0
                         then let startPos = excess `div` 2
                              in take (startPos + unMinLength minLen)
                                 $ drop startPos retVal'
                         else retVal'
       in encodeStep3 alphabet' minLen retVal''
  where halfLen = length alphabet `div` 2

decode :: HashEncoder -> HashId -> [Int]
decode enc@(HashEncoder salt' _ alphabet seps guards) hashId =
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
          let buffer = lottery' : unSalt salt' ++ alphabet'
              alphabet'' = consistentShuffle alphabet' $ Salt
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
consistentShuffle alphabet (Salt salt') =
  fst $ foldl' shuffleStep (alphabet, 0) $ zip is vs
  where
    vs = cycle [0..(length salt'-1)]
    is = [(length alphabet-1), (length alphabet-2)..1]
    shuffleStep (alphabet', p) (i, v) =
      let ascVal = ord $ salt' !! v
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
