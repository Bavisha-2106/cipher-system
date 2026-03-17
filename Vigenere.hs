module Vigenere where

import Data.Char (ord, chr, isUpper, isLower, isAlpha, toLower)
import Types

-- | Cycle the key to match the length of the text (skipping non-alpha chars)
cycleKey :: VigenereKey -> String -> String
cycleKey key text = take (length alphaOnly) (cycle key)
  where alphaOnly = filter isAlpha text

-- | Get shift value from a key character
keyShift :: Char -> Int
keyShift c = ord (toLower c) - ord 'a'

-- | Vigenere encrypt a single character with a key character
vigenereShiftChar :: Int -> Char -> Char
vigenereShiftChar n c
  | isUpper c = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
  | isLower c = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
  | otherwise = c

-- | Vigenere decrypt a single character with a key character
vigenereUnshiftChar :: Int -> Char -> Char
vigenereUnshiftChar n c
  | isUpper c = chr $ (ord c - ord 'A' - n + 26) `mod` 26 + ord 'A'
  | isLower c = chr $ (ord c - ord 'a' - n + 26) `mod` 26 + ord 'a'
  | otherwise = c

-- | Encrypt plaintext using Vigenere cipher
vigenereEncrypt :: VigenereKey -> Plaintext -> Ciphertext
vigenereEncrypt key text = snd $ foldl step (keyStream, "") text
  where
    keyStream = cycle (map keyShift key)
    step (ks, acc) c
      | isAlpha c = (tail ks, acc ++ [vigenereShiftChar (head ks) c])
      | otherwise = (ks,      acc ++ [c])

-- | Decrypt ciphertext using Vigenere cipher
vigenereDecrypt :: VigenereKey -> Ciphertext -> Plaintext
vigenereDecrypt key text = snd $ foldl step (keyStream, "") text
  where
    keyStream = cycle (map keyShift key)
    step (ks, acc) c
      | isAlpha c = (tail ks, acc ++ [vigenereUnshiftChar (head ks) c])
      | otherwise = (ks,      acc ++ [c])

-- | Show key cycling for display purposes
showKeyCycle :: VigenereKey -> String -> String
showKeyCycle key text =
  let alphaOnly  = filter isAlpha text
      cycled     = take (length alphaOnly) (cycle key)
      -- Re-insert spaces at original positions
      reinsert [] _ = []
      reinsert (c:cs) ks
        | isAlpha c = head ks : reinsert cs (tail ks)
        | otherwise = c       : reinsert cs ks
  in reinsert text cycled
