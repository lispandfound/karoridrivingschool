module Main (main) where

import Data.Maybe (fromJust)
import Email
import Enquiry
import Options.Applicative as O
import Text.Email.Validate
import Text.Megaparsec
import Web.Scotty as S

type MParser = Parsec Void Text

parseFormParam :: MParser a -> LText -> ActionM a
parseFormParam p path = formParam path >>= either (\e -> throw (FailedToParseParameter (toStrict path) "" (toText $ errorBundlePretty e))) pure . parse p (toString path) . toStrict

enquiry :: EmailAddress -> FilePath -> ScottyM ()
enquiry bookingEmail queue =
    S.post "/enquire/" $
        do
            name' <- formParam "fullName"
            age' <- parseFormParam ageP "age"
            mobileNumber' <- parseFormParam phoneNumberP "mobileNumber"
            emailAddress' <- parseFormParam emailP "emailAddress"
            suburb' <- formParam "suburb"
            licence' <- parseFormParam licenceP "licence"
            experience' <- parseFormParam experienceP "experience"
            info' <- formParam "info"
            let enquiry' =
                    Enquiry
                        { fullName = name'
                        , mobileNumber = mobileNumber'
                        , age = age'
                        , emailAddress = emailAddress'
                        , suburb = suburb'
                        , licence = licence'
                        , drivingExperience = experience'
                        , info = info'
                        }
            uuid <- getEmailID
            path <- writeEmail uuid bookingEmail enquiry'
            sendEmail queue uuid path
            redirect "/"

data Config = Config
    { cfgPort :: Int
    , cfgBookingEmail :: EmailAddress
    , cfgQueuePath :: FilePath
    }

-- 2. Define the Parser
configParser :: Config -> Parser Config
configParser (Config{..}) =
    Config
        <$> O.option
            auto
            ( long "port"
                <> short 'p'
                <> help "Port to run the Scotty server on"
                <> showDefault
                <> value cfgPort
            )
        <*> O.option
            (eitherReader (validate . encodeUtf8)) -- Custom reader for EmailAddress
            ( long "email"
                <> short 'e'
                <> help "Recipient email for booking inquiries"
                <> showDefault
                <> value cfgBookingEmail
            )
        <*> strOption
            ( long "queue"
                <> short 'q'
                <> help "Path to the filed job queue directory"
                <> showDefault
                <> value cfgQueuePath
            )

defaults :: Config
defaults = Config{cfgPort = 3000, cfgBookingEmail = (fromJust . emailAddress) "lessons@karoridrivingschool.co.nz", cfgQueuePath = "/jobs"}

main :: IO ()
main = do
    let opts = info (configParser defaults <**> helper) (fullDesc <> progDesc "Start the Karori Driving School enquiry API")
    (Config{..}) <- execParser opts

    scotty cfgPort $ do
        enquiry cfgBookingEmail cfgQueuePath
