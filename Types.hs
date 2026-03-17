module Types where

-- | A key for Caesar cipher is just a shift amount
type ShiftKey = Int

-- | A key for Vigenere cipher is a keyword string
type VigenereKey = String

-- | Plaintext and Ciphertext are both just Strings
type Plaintext  = String
type Ciphertext = String

-- | Result of frequency analysis
data CrackResult = CrackResult
  { crackShift     :: ShiftKey
  , crackDecrypted :: Plaintext
  , crackScore     :: Double
  } deriving (Show)

-- | Menu choices
data CipherChoice = CaesarMode | VigenereMode | CrackerMode | QuitMode
  deriving (Eq, Show)

-- | Encrypt or Decrypt
data Direction = Encrypt | Decrypt
  deriving (Eq, Show)
