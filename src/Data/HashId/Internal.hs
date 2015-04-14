module Data.HashId.Internal where

import           Data.Char  (ord, toUpper)
import           Data.List  (elemIndex, foldl', intersect, nub, (\\))
import           Data.Maybe (fromJust)

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
      (alphabet'', seps'') =
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

encode :: HashEncoder -> [Int] -> HashId
encode = undefined

decode :: HashEncoder -> HashId -> [Int]
decode = undefined

parse :: HashEncoder -> String -> Maybe HashId
parse = undefined

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
                  pos = fromJust $ elemIndex cur alphabet
                  num' = num + pos * (length alphabet ^ (length hash' - i - 1))
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
