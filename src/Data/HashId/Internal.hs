module Data.HashId.Internal where

import           Data.Char       (ord, toUpper)
import           Data.List       (elemIndex, foldl', intersect, nub, (\\))
import           Data.Maybe      (fromMaybe)
import           Numeric.Natural

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
newtype Alphabet = Alphabet { unAlpha :: String } deriving (Show, Eq)

type Separators = String
type Guards = String
type Hashed = String
type Error = String

(!!!) :: [a] -> Natural -> a
(!!!) xs n = xs !! fromIntegral n

minLength :: Int -> Either Error MinLength
minLength l | l < 0 = Left ""
minLength l = Right $ MinLength l

satisfiesLength :: MinLength -> Int -> Bool
satisfiesLength (MinLength minLen) = (<=) minLen

alphabet :: String -> Either Error Alphabet
alphabet a =
  case sanitize a of
    a' | length a' < minAlphabetLength ->
           Left $ "Alphabet must contain at least " ++
             show minAlphabetLength ++ " unique characters"
    a' -> Right $ Alphabet a'
  where sanitize = filter (' ' /=) . nub

minAlphabetLength :: Int
minAlphabetLength = 16

salt :: String -> Either Error Salt
salt s =
  case sanitize s of
    s' | isValid s' -> Right $ Salt s'
    _ -> Left $ "Only characters in '" ++ validChars ++ "' are allowed"
  where sanitize = filter (' ' /=) . nub
        isValid = all $ flip elem validChars
        validChars = ['a'..'z'] ++ ['A'..'Z']

defaultAlphabet :: Alphabet
defaultAlphabet =
  Alphabet $ concat [
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
  HashOptions salt' defaultAlphabet defaultMinHashLength

options :: Salt -> Alphabet -> MinLength -> HashOptions
options = HashOptions

mkEncoder :: HashOptions -> HashEncoder
mkEncoder (HashOptions salt' (Alphabet alpha) minLen) =
  let seps = defaultSeps `intersect` alpha
      alphabet' = alpha \\ seps
      seps' = consistentShuffle seps salt'
      divCoeff = fromIntegral (length alpha) / fromIntegral (length seps')
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
  in HashEncoder salt' minLen (Alphabet alphabet'''') seps''' guards

sepDiv :: Double
sepDiv = 3.5

guardDiv :: Double
guardDiv = 12

type ReturnVal = String

encode :: HashEncoder -> [Natural] -> HashId
encode (HashEncoder salt' minLen a@(Alphabet alpha) seps guards) nums =
  let
      hash' = sum $ map (\(i, n) -> n `mod` (i+100)) $ zip [0..] nums
      lenalpha = fromIntegral $ length alpha
      ret = alpha !!! (hash' `mod` lenalpha)
      (alphabet', retVal) = encodeStep1 ret a salt' seps nums
      retVal' = encodeStep2 hash' guards minLen retVal
      retVal'' = encodeStep3 alphabet' minLen retVal'
  in HashId retVal''

encodeStep1 :: Char
                -> Alphabet
                -> Salt
                -> Separators
                -> [Natural]
                -> (Alphabet, ReturnVal)
encodeStep1 char alpha salt' seps nums =
  foldl' foldStep (alpha, [char]) ns
  where ns = zip [0..] nums
        foldStep ((Alphabet alpha'), retVal) (i, n) =
          let buffer = char : unSalt salt' ++ alpha'
              alphabet'' = consistentShuffle alpha' $ Salt
                           $ take (length alpha') buffer
              lastC = hash (Alphabet alphabet'') n
              retVal' = retVal ++ lastC
              lennums = fromIntegral $ length nums
              retVal'' = if i + 1 < lennums
                            then
                              let n' = n `mod` (fromIntegral $ ord (head lastC) + i)
                                  lenseps = fromIntegral $ length seps
                                  sepsIndex = n' `mod` lenseps
                              in retVal' ++ [seps !!! sepsIndex]
                            else retVal'
          in ((Alphabet alphabet''), retVal'')

encodeStep2 :: Natural -> Guards -> MinLength -> ReturnVal -> ReturnVal
encodeStep2 hash' guards minLen retVal =
  if satisfiesLength minLen $ length retVal
     then retVal
     else
       let guardIndex = hash' + (fromIntegral $ ord (head retVal))
           guard = guards !!! guardIndex
           retVal' = guard:retVal
       in if satisfiesLength minLen $ length retVal'
             then retVal'
             else
               let lenguards = fromIntegral $ length guards
                   guardIndex' = (hash' + (fromIntegral $ ord (retVal' !!! 2))) `mod` lenguards
                   guard' = guards !!! guardIndex'
               in retVal' ++ [guard']

encodeStep3 :: Alphabet -> MinLength -> ReturnVal -> ReturnVal
encodeStep3 (Alphabet alpha) minLen retVal =
  if satisfiesLength minLen $ length retVal
     then retVal
     else
       let alpha' = consistentShuffle alpha $ Salt alpha
           retVal' = drop halfLen alpha' ++ retVal ++ take halfLen alpha'
           excess = length retVal' - unMinLength minLen
           retVal'' = if excess > 0
                         then let startPos = excess `div` 2
                              in take (startPos + unMinLength minLen)
                                 $ drop startPos retVal'
                         else retVal'
       in encodeStep3 (Alphabet alpha') minLen retVal''
  where halfLen = length alpha `div` 2

decode :: HashEncoder -> HashId -> [Natural]
decode enc@(HashEncoder salt' _ (Alphabet alpha) seps guards) hashId =
  let hashBreakdown = words $ map (toSpace guards) $ unHashId hashId
      i = if length hashBreakdown `elem` [2, 3] then 1 else 0
      hashBreakdown' = hashBreakdown !! i
      lottery = head hashBreakdown'
      hashBreakdown'' = words $ map (toSpace seps) $ tail hashBreakdown'
      ret = fst $ foldl' (collectVals lottery) ([], alpha) hashBreakdown''
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
              val = unhash (Alphabet alphabet'') subhash
          in (vals ++ [val], alphabet'')

parse :: HashEncoder -> String -> Maybe HashId
parse _ s = Just $ HashId s

toText :: HashId -> String
toText = unHashId

hash :: Alphabet -> Natural -> Hashed
hash (Alphabet alpha) num = hash' num alpha ""
  where
    hash' i a output =
      if i > 0
         then let aLen = fromIntegral $ length a
                  newOut = (a !!! (i `mod` aLen)) : output
              in hash' (i `div` aLen) a newOut
      else output

unhash :: Alphabet -> Hashed -> Natural
unhash (Alphabet alpha) hash' = unhash' 0 0
  where
    unhash' i num =
      if i >= (fromIntegral $ length hash')
         then num
         else let cur = hash' !!! i
                  pos = fromIntegral $  fromMaybe (-1) $ elemIndex cur alpha
                  hashLen = fromIntegral $ length hash'
                  power = fromIntegral $ length alpha ^ (hashLen - i - 1)
                  num' = num + pos * power
              in unhash' (i + 1) num'

consistentShuffle :: String -> Salt -> String
consistentShuffle alpha (Salt salt') =
  fst $ foldl' shuffleStep (alpha, 0) $ zip is vs
  where
    vs = cycle [0..(length salt'-1)]
    is = [(length alpha-1), (length alpha-2)..1]
    shuffleStep (alpha', p) (i, v) =
      let ascVal = ord $ salt' !! v
          p' = p + ascVal
          j = (ascVal + v + p') `mod` i
      in (swapAt i j alpha', p')

replaceAt :: a -> Int -> [a] -> [a]
replaceAt x ix xs = take (ix-1) xs ++ [x] ++ drop ix xs

swapAt :: Int -> Int -> [a] -> [a]
swapAt ixA ixB xs =
  let tmpA = xs !! ixA
      tmpB = xs !! ixB
      xs' = replaceAt tmpA (ixB+1) xs
      xs'' = replaceAt tmpB (ixA+1) xs'
  in xs''
