module Main (main) where

import Data.Maybe (fromJust)
import Email
import Enquiry
import Text.Email.Validate
import Text.Megaparsec
import Text.Megaparsec.Error (errorBundlePretty)
import Web.Scotty as S

type Parser = Parsec Void Text

parseFormParam :: Parser a -> LText -> ActionM a
parseFormParam p path = formParam path >>= either (\e -> throw (FailedToParseParameter (toStrict path) "" (toText $ errorBundlePretty e))) pure . parse p (toString path) . toStrict

testEmail :: EmailAddress
testEmail = (fromJust . emailAddress) "me@test.dev"

testQueue :: FilePath
testQueue = "./jobs/"

inquiry :: EmailAddress -> FilePath -> ScottyM ()
inquiry bookingEmail queue =
    S.post "/enquire/" $
        do
            name <- formParam "fullName"
            age <- parseFormParam ageP "age"
            mobileNumber <- parseFormParam phoneNumberP "mobileNumber"
            emailAddress <- parseFormParam emailP "emailAddress"
            suburb <- parseFormParam suburbP "suburb"
            licence <- parseFormParam licenceP "licence"
            experience <- parseFormParam experienceP "experience"
            info <- formParam "info"
            let inquiry =
                    Enquiry
                        { fullName = name
                        , mobileNumber = mobileNumber
                        , age = age
                        , emailAddress = emailAddress
                        , suburb = suburb
                        , licence = licence
                        , drivingExperience = experience
                        , info = info
                        }
            uuid <- getEmailID
            path <- writeEmail uuid bookingEmail inquiry
            sendEmail queue uuid path
            redirect "/"

main :: IO ()
main = scotty 3000 $ do
    inquiry testEmail testQueue
