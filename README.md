# Cipher System
### Classical Cryptography in Haskell
### 23CSE212 - Principles of Functional Language

## Features
- Caesar Cipher - encrypt & decrypt with shift key
- Vigenere Cipher - encrypt & decrypt with keyword
- Frequency Analysis Cracker - automatically crack Caesar ciphertext
- Input validation throughout

## Project Structure
```
cipher-system/
├── Main.hs
├── Types.hs
├── Caesar.hs 
├── Vigenere.hs  
├── Cracker.hs 
└── cipher-system.cabal
```

## How to Run
Install GHC via https://www.haskell.org/ghcup/

With Cabal:
```
  cabal update
  cabal run
```
With GHC directly (Windows):
```
  ghc -o cipher Main.hs Types.hs Caesar.hs Vigenere.hs Cracker.hs
  .\cipher.exe
````

## Functional Concepts Used
- Algebraic Data Types
- Pattern Matching
- Higher-Order Functions (map, foldl, filter)
- Infinite Lists (cycle)
- Either for Validation
- Data.Map for frequency analysis
