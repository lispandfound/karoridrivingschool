module Main (main) where

import Data.Maybe (fromJust)
import Database.SQLite.Simple (withConnection)
import Email
import Enquiry
import Lucid
import Network.HTTP.Types (badRequest400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Options.Applicative as O
import Review
import Text.Email.Validate
import Web.Scotty as S

errorDesc :: ScottyException -> Text
errorDesc (FailedToParseParameter field _ _) = "Invalid " <> fieldLabel field <> ": " <> fieldHint field <> " Please go back to fix the form."
errorDesc (MalformedForm e) = e
errorDesc _ = "An unexpected error occurred. Please check your submission and try again."

renderErrorPage :: Text -> LText
renderErrorPage = renderText . errorPageHtml

errorPageHtml :: Text -> Html ()
errorPageHtml msg = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            title_ "Form Error - Karori Driving School"
            link_ [rel_ "icon", type_ "image/x-icon", href_ "/favicon.ico"]
            link_ [rel_ "stylesheet", href_ "/style.css"]
        body_ $
            div_ [class_ "page-container"] $
                div_ [class_ "content-area"] $
                    section_ [class_ "section-block"] $ do
                        h1_ "Submission Error"
                        p_ (toHtml msg)
                        a_ [href_ "javascript:history.back()"] "\x2190 Go Back"

sendError :: ScottyException -> ActionM ()
sendError = (status badRequest400 >>) . html . renderErrorPage . errorDesc

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

allValues :: Text -> [(Text, Text)] -> [Text]
allValues k = map snd . filter ((== k) . fst)

review :: FilePath -> ScottyM ()
review dbPath = do
    S.post "/reviews" $ do
        p <- formParams
        let names = allValues "name" p
            contents = allValues "review" p
            reviews = zipWith Review names contents
        liftIO $ withConnection dbPath (\conn -> setReviews conn reviews)
        redirect "/admin/reviews"
    S.get "/reviews" $ do
        reviews <- liftIO $ withConnection dbPath getReviews
        json reviews
    S.get "/admin/reviews" $ do
        reviews <- liftIO $ withConnection dbPath getReviews
        (html . renderText . reviewAdminPage) reviews
    S.get "/reviews/new" $ do
        (html . renderText) newReviewFragment

data Config = Config
    { cfgPort :: Int
    , cfgBookingEmail :: EmailAddress
    , cfgQueuePath :: FilePath
    , cfgDBPath :: FilePath
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
        <*> strOption
            ( long "db"
                <> short 'd'
                <> help "Path to review database"
                <> showDefault
                <> value cfgDBPath
            )

defaults :: Config
defaults = Config{cfgPort = 3000, cfgBookingEmail = (fromJust . emailAddress) "lessons@karoridrivingschool.co.nz", cfgQueuePath = "/jobs", cfgDBPath = "/db/reviews.db"}

main :: IO ()
main = do
    let opts = info (configParser defaults <**> helper) (fullDesc <> progDesc "Start the Karori Driving School enquiry API")
    (Config{..}) <- execParser opts
    withConnection cfgDBPath createTable
    scotty cfgPort $ do
        middleware logStdoutDev
        enquiry cfgBookingEmail cfgQueuePath
        review cfgDBPath
