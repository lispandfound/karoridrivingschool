module Enquiry (Enquiry (..), PhoneNumber, unPhoneNumber, unAge, Licence (..), Age, Experience (..), emailP, phoneNumberP, licenceP, experienceP, ageP) where

import Text.Email.Parser (EmailAddress)
import Text.Email.Validate (validate)
import Text.Megaparsec as M
import Text.Megaparsec.Char (char, digitChar, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void Text

newtype PhoneNumber = PhoneNumber Text
    deriving (Show)

unPhoneNumber :: PhoneNumber -> Text
unPhoneNumber (PhoneNumber t) = t

data Licence = Learner | Restricted | Overseas
    deriving stock (Show)

newtype Age = Age Integer deriving (Show)

unAge :: Age -> Integer
unAge (Age age) = age

emailP :: Parser EmailAddress
emailP = M.some (satisfy (/= ' ')) >>= either fail pure . validate . encodeUtf8 . toText

data Experience = None | LessThan10 | From10To30 | More | Returning deriving (Show)

data Enquiry = Enquiry
    { fullName :: Text
    , mobileNumber :: PhoneNumber
    , emailAddress :: EmailAddress
    , suburb :: Text
    , licence :: Licence
    , age :: Age
    , drivingExperience :: Experience
    , info :: Text
    }
    deriving (Show)

-- | Helper to parse a string and return a specific constructor
symbolic :: Text -> a -> Parser a
symbolic s val = string s $> val

phoneNumberP :: Parser PhoneNumber
phoneNumberP = PhoneNumber . toText <$> M.some (digitChar <|> char ' ' <|> char '+')

licenceP :: Parser Licence
licenceP =
    choice
        [ symbolic "Learner" Learner
        , symbolic "Restricted" Restricted
        , symbolic "Overseas" Overseas
        ]

experienceP :: Parser Experience
experienceP =
    choice
        [ symbolic "None" None
        , symbolic "LessThan10" LessThan10
        , symbolic "From10To30" From10To30
        , symbolic "More" More
        , symbolic "Returning" Returning
        ]

ageP :: Parser Age
ageP = Age <$> decimal
