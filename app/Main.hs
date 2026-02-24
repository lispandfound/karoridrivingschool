module Main (main) where

import Data.Maybe (fromJust)
import Email
import Enquiry
import Lucid
import Network.HTTP.Types (badRequest400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative as O
import Text.Email.Validate
import Web.Scotty as S

errorDesc :: ScottyException -> Text
errorDesc (FailedToParseParameter field _ _) = "Invalid " <> fieldLabel field <> ": " <> fieldHint field <> " Please go back to fix the form."
errorDesc (MalformedForm e) = e
errorDesc _ = "An unexpected error occurred. Please check your submission and try again."

renderErrorPage :: Text -> Text
renderErrorPage = toStrict . renderText . errorPageHtml

errorPageHtml :: Text -> Html ()
errorPageHtml msg = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            title_ "Form Error - Karori Driving School"
            link_ [rel_ "icon", type_ "image/x-icon", href_ "/favicon.ico"]
            link_ [rel_ "stylesheet", href_ "/static/style.css"]
        body_ $
            div_ [class_ "page-container"] $
                div_ [class_ "content-area"] $
                    section_ [class_ "section-block"] $ do
                        h1_ "Submission Error"
                        p_ (toHtml msg)
                        a_ [href_ "javascript:history.back()"] "\x2190 Go Back"

sendError :: ScottyException -> ActionM ()
sendError = (status badRequest400 >>) . html . fromStrict . renderErrorPage . errorDesc

processEnquiry :: EmailAddress -> FilePath -> ActionM ()
processEnquiry bookingEmail queue = do
    enquiry' <- formData
    uuid <- getEmailID
    path <- writeEmail uuid bookingEmail enquiry'
    putStrLn $ "Queueing email job " <> show uuid <> " for " <> path
    sendEmail queue uuid path
    redirect "/success.html"

enquiry :: EmailAddress -> FilePath -> ScottyM ()
enquiry bookingEmail queue =
    S.post "/enquire/" $
        processEnquiry bookingEmail queue
            `catch` sendError

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
        middleware logStdoutDev
        enquiry cfgBookingEmail cfgQueuePath
