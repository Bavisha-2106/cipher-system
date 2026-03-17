module Caesar where

import Data.Char (ord, chr, isUpper, isLower, isAlpha)
import Types

-- | Shift a single character by n positions
shiftChar :: ShiftKey -> Char -> Char
shiftChar n c
  | isUpper c = chr $ (ord c - ord 'A' + n) `mod` 26 + ord 'A'
  | isLower c = chr $ (ord c - ord 'a' + n) `mod` 26 + ord 'a'
  | otherwise = c  -- preserve spaces, punctuation, numbers

-- | Encrypt plaintext using Caesar cipher
caesarEncrypt :: ShiftKey -> Plaintext -> Ciphertext
caesarEncrypt n = map (shiftChar n)

-- | Decrypt ciphertext using Caesar cipher
-- Decryption is just encryption with the inverse shift
caesarDecrypt :: ShiftKey -> Ciphertext -> Plaintext
caesarDecrypt n = caesarEncrypt (26 - n `mod` 26)

-- | Show step-by-step character mapping
caesarSteps :: ShiftKey -> String -> [(Char, Char)]
caesarSteps n text =
  [ (c, shiftChar n c) | c <- text, isAlpha c ]
