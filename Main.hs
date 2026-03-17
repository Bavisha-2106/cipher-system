module Main where

import Data.Char (toLower, isAlpha)
import System.IO (hFlush, stdout)
import Types
import Caesar
import Vigenere
import Cracker

-- Helpers

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

divider :: String
divider = replicate 42 '-'

banner :: String -> IO ()
banner title = do
  putStrLn divider
  putStrLn $ "  " ++ title
  putStrLn divider

-- Validation

validateText :: String -> Either String String
validateText "" = Left "Text cannot be empty."
validateText t  = Right t

validateShift :: String -> Either String ShiftKey
validateShift s =
  case reads s of
    [(n, "")] | n >= 1 && n <= 25 -> Right n
    _ -> Left "Shift must be a number between 1 and 25."

validateKey :: String -> Either String VigenereKey
validateKey "" = Left "Key cannot be empty."
validateKey k
  | all isAlpha k = Right (map toLower k)
  | otherwise     = Left "Key must contain only letters."

-- Direction Menu

directionMenu :: IO Direction
directionMenu = do
  putStrLn "  1. Encrypt"
  putStrLn "  2. Decrypt"
  choice <- prompt "Choice: "
  case choice of
    "1" -> return Encrypt
    "2" -> return Decrypt
    _   -> do
      putStrLn "[!] Invalid choice. Defaulting to Encrypt."
      return Encrypt

-- Caesar Mode

caesarMode :: IO ()
caesarMode = do
  banner "CAESAR CIPHER"
  dir      <- directionMenu
  rawText  <- prompt "\nEnter text  : "
  rawShift <- prompt "Enter shift : "

  case (validateText rawText, validateShift rawShift) of
    (Left e, _) -> putStrLn $ "[!] " ++ e
    (_, Left e) -> putStrLn $ "[!] " ++ e
    (Right text, Right shift) -> do
      let result = case dir of
                     Encrypt -> caesarEncrypt shift text
                     Decrypt -> caesarDecrypt shift text
          label1 = case dir of Encrypt -> "Plaintext "; Decrypt -> "Ciphertext"
          label2 = case dir of Encrypt -> "Ciphertext"; Decrypt -> "Plaintext "

      putStrLn $ "\n" ++ divider
      putStrLn $ "  " ++ label1 ++ " : " ++ text
      putStrLn $ "  Shift      : " ++ show shift
      putStrLn $ "  " ++ label2 ++ " : " ++ result
      putStrLn divider

-- Vigenere Mode

vigenereMode :: IO ()
vigenereMode = do
  banner "VIGENERE CIPHER"
  dir     <- directionMenu
  rawText <- prompt "\nEnter text : "
  rawKey  <- prompt "Enter key  : "

  case (validateText rawText, validateKey rawKey) of
    (Left e, _) -> putStrLn $ "[!] " ++ e
    (_, Left e) -> putStrLn $ "[!] " ++ e
    (Right text, Right key) -> do
      let result   = case dir of
                       Encrypt -> vigenereEncrypt key text
                       Decrypt -> vigenereDecrypt key text
          label1   = case dir of Encrypt -> "Plaintext "; Decrypt -> "Ciphertext"
          label2   = case dir of Encrypt -> "Ciphertext"; Decrypt -> "Plaintext "
          keyCycle = showKeyCycle key text

      putStrLn $ "\n" ++ divider
      putStrLn $ "  " ++ label1    ++ " : " ++ text
      putStrLn $ "  Key (cycled)   : " ++ keyCycle
      putStrLn $ "  " ++ label2    ++ " : " ++ result
      putStrLn divider

-- Cracker Mode

crackerMode :: IO ()
crackerMode = do
  banner "FREQUENCY ANALYSIS CRACKER"
  putStrLn "  Attempts to crack a Caesar-encrypted ciphertext"
  putStrLn "  by analysing letter frequency patterns.\n"

  rawText <- prompt "Enter ciphertext: "

  case validateText rawText of
    Left e -> putStrLn $ "[!] " ++ e
    Right text -> do
      putStrLn "\nAnalysing letter frequencies...\n"
      putStrLn "Letter Frequencies:"
      putStr (renderFreqChart text)

      let results = crackCaesar text
          best    = head results

      putStrLn $ "\n" ++ divider
      putStrLn   "  Top 5 Possible Decryptions:"
      putStrLn divider
      mapM_ (\r -> putStrLn $
        "  Shift " ++ show (crackShift r) ++
        " : " ++ crackDecrypted r)
        (take 5 results)
      putStrLn divider

      putStrLn $ "\n>> Best Guess (Shift " ++ show (crackShift best) ++ "):"
      putStrLn $ "   " ++ crackDecrypted best

-- Main Menu

mainMenu :: IO ()
mainMenu = do
  putStrLn "\n+------------------------------------------+"
  putStrLn   "|           CIPHER SYSTEM                  |"
  putStrLn   "|   Classical Cryptography in Haskell      |"
  putStrLn   "+------------------------------------------+"
  putStrLn   "|  1. Caesar Cipher                        |"
  putStrLn   "|  2. Vigenere Cipher                      |"
  putStrLn   "|  3. Frequency Analysis Cracker           |"
  putStrLn   "|  4. Quit                                 |"
  putStrLn   "+------------------------------------------+"

main :: IO ()
main = gameLoop

gameLoop :: IO ()
gameLoop = do
  mainMenu
  choice <- prompt "Choice: "
  case choice of
    "1" -> caesarMode   >> gameLoop
    "2" -> vigenereMode >> gameLoop
    "3" -> crackerMode  >> gameLoop
    "4" -> putStrLn "\nGoodbye!\n"
    _   -> putStrLn "[!] Invalid choice." >> gameLoop
