module Enquiry (Enquiry (..), PhoneNumber, unPhoneNumber, unAge, Licence (..), Age, Experience (..), Pronouns (..), StudentOrAdult (..), emailP, phoneNumberP, licenceP, experienceP, ageP, pronounsP, studentOrAdultP, fieldLabel, fieldHint) where

import Text.Email.Parser (EmailAddress)
import Text.Email.Validate (validate)
import Text.Megaparsec as M
import Text.Megaparsec.Char (char, digitChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Web.FormUrlEncoded (Form, FromForm (..), lookupMaybe, lookupUnique)

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

data Pronouns = HeHim | SheHer | TheyThem | OtherPronoun Text | PreferNotToSay deriving (Show)

data StudentOrAdult = Student | Adult deriving (Show)

data Enquiry = Enquiry
    { fullName :: Text
    , mobileNumber :: PhoneNumber
    , emailAddress :: EmailAddress
    , suburb :: Text
    , licence :: Licence
    , age :: Age
    , drivingExperience :: Experience
    , pronouns :: Pronouns
    , studentOrAdult :: StudentOrAdult
    , info :: Text
    }
    deriving (Show)

instance FromForm Enquiry where
    fromForm f =
        Enquiry
            <$> lookupOrMissing "fullName" f
            <*> parseP "mobileNumber" phoneNumberP f
            <*> parseP "emailAddress" emailP f
            <*> lookupOrMissing "suburb" f
            <*> parseP "licence" licenceP f
            <*> parseP "age" ageP f
            <*> parseP "experience" experienceP f
            <*> parsePronounsF f
            <*> parseP "studentOrAdult" studentOrAdultP f
            <*> (fmap (fromMaybe mempty) . lookupMaybe "info") f

parsePronounsF :: Form -> Either Text Pronouns
parsePronounsF f = do
    val <- lookupOrMissing "pronouns" f
    case val of
        "OtherPronoun" -> OtherPronoun . fromMaybe "" <$> lookupMaybe "pronounsOther" f
        _ -> parseP "pronouns" pronounsP f

parseP :: Text -> Parser a -> Form -> Either Text a
parseP l p = (>>= first (const (fieldHint l)) . parse p mempty) . lookupOrMissing l

lookupOrMissing :: Text -> Form -> Either Text Text
lookupOrMissing l = first (const (missingField l)) . lookupUnique l

missingField :: Text -> Text
missingField l = "Missing required field: " <> fieldLabel l <> "."

fieldLabel :: Text -> Text
fieldLabel "mobileNumber" = "Phone Number"
fieldLabel "emailAddress" = "Email Address"
fieldLabel "age" = "Age"
fieldLabel "licence" = "Licence"
fieldLabel "experience" = "Experience"
fieldLabel "fullName" = "Full Name"
fieldLabel "suburb" = "Suburb"
fieldLabel "pronouns" = "Pronouns"
fieldLabel "studentOrAdult" = "Student or Adult"
fieldLabel f = f

fieldHint :: Text -> Text
fieldHint "mobileNumber" = "Phone number should contain only digits, spaces, or a leading '+' (e.g. 021 123 4567)."
fieldHint "emailAddress" = "Email address should be a valid address (e.g. name@example.com)."
fieldHint "age" = "Age should be a whole number (e.g. 17)."
fieldHint "licence" = "Licence type must be one of: Learner, Restricted, or Overseas."
fieldHint "experience" = "Driving experience must be one of the provided options."
fieldHint "pronouns" = "Pronouns must be one of: He/Him, She/Her, They/Them, Other, or Prefer not to say."
fieldHint "studentOrAdult" = "Please indicate whether you are a Student or an Adult."
fieldHint f = fieldLabel f <> " contains an invalid value."

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

pronounsP :: Parser Pronouns
pronounsP =
    choice
        [ symbolic "HeHim" HeHim
        , symbolic "SheHer" SheHer
        , symbolic "TheyThem" TheyThem
        , symbolic "PreferNotToSay" PreferNotToSay
        ]

studentOrAdultP :: Parser StudentOrAdult
studentOrAdultP =
    choice
        [ symbolic "Student" Student
        , symbolic "Adult" Adult
        ]
