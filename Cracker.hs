module Cracker where

import Data.Char (toLower, isAlpha, ord, chr)
import Data.List (sortBy, maximumBy)
import Data.Ord (comparing, Down(..))
import qualified Data.Map.Strict as Map
import Types
import Caesar (caesarDecrypt)

-- | Standard English letter frequencies (a-z)
englishFreq :: Map.Map Char Double
englishFreq = Map.fromList $ zip ['a'..'z']
  [ 8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094
  , 6.966, 0.153, 0.772, 4.025, 2.406,  6.749, 7.507, 1.929
  , 0.095, 5.987, 6.327, 9.056, 2.758,  0.978, 2.360, 0.150
  , 1.974, 0.074 ]

-- | Count letter frequencies in a text
letterFrequency :: String -> Map.Map Char Int
letterFrequency text =
  foldr (\c m -> Map.insertWith (+) c 1 m) Map.empty
  $ filter isAlpha (map toLower text)

-- | Convert raw counts to percentages
toPercentage :: Map.Map Char Int -> Map.Map Char Double
toPercentage freq =
  let total = fromIntegral $ sum (Map.elems freq)
  in  Map.map (\c -> fromIntegral c / total * 100) freq

-- | Score a decrypted text against English frequencies
-- Higher score = closer to English
scoreText :: String -> Double
scoreText text =
  let freq   = toPercentage (letterFrequency text)
      score  = sum [ Map.findWithDefault 0 c freq *
                     Map.findWithDefault 0 c englishFreq
                   | c <- ['a'..'z'] ]
  in  score

-- | Try all 25 shifts and rank by score
crackCaesar :: Ciphertext -> [CrackResult]
crackCaesar cipher =
  sortBy (comparing (Down . crackScore))
  [ let decrypted = caesarDecrypt shift cipher
        score     = scoreText decrypted
    in  CrackResult shift decrypted score
  | shift <- [1..25] ]

-- | Best guess
bestGuess :: Ciphertext -> CrackResult
bestGuess = head . crackCaesar

-- | Render a frequency bar chart
renderFreqChart :: String -> String
renderFreqChart text =
  let freq    = toPercentage (letterFrequency text)
      sorted  = sortBy (comparing (Down . snd)) (Map.toList freq)
      maxFreq = maximum (map snd sorted)
      bar pct = replicate (round (pct / maxFreq * 20)) '#'
  in  unlines
      [ "  " ++ [c] ++ " : " ++ bar p ++ " " ++ show (round p :: Int) ++ "%"
      | (c, p) <- sorted, p > 0 ]
