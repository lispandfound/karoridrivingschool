module Email (renderEnquiryEmail, sendEmail, getEmailID, writeEmail) where

import Data.UUID (UUID)
import Enquiry
import Fmt
import Lucid
import System.Random (randomIO)
import Text.Email.Parser (EmailAddress, toByteString)

-- | Main entry point to generate the HTML string
renderEnquiryEmail :: Enquiry -> Text
renderEnquiryEmail inquiry = (toStrict . renderText) (inquiryTemplate inquiry)

showDrivingExperience :: Experience -> Text
showDrivingExperience None = "None"
showDrivingExperience LessThan10 = "Less than 10 hours"
showDrivingExperience From10To30 = "Between 10 and 30 hours"
showDrivingExperience More = "More than 30 hours"
showDrivingExperience Returning = "Returning driver"

-- | The HTML Template
inquiryTemplate :: Enquiry -> Html ()
inquiryTemplate (Enquiry{..}) = do
    doctype_
    html_ [lang_ "en"] $ do
        head_ $ do
            meta_ [charset_ "utf-16"]
            style_ "body { font-family: sans-serif; line-height: 1.5; color: #333; }"
        body_ [style_ "margin: 0; padding: 20px;"] $ do
            h2_ "New Driving Lesson Enquiry"
            p_ "A new inquiry has been received with the following details:"

            table_ [style_ "width: 100%; border-collapse: collapse; max-width: 600px;"] $ do
                row "Full Name" fullName
                row "Mobile" (unPhoneNumber mobileNumber)
                row "Email" ((decodeUtf8 . toByteString) emailAddress)
                row "Suburb" (show suburb)
                row "Licence" (show licence)
                row "Age" ((show . unAge) age)
                row "Experience" (showDrivingExperience drivingExperience)

            hr_ []
            h3_ "Additional Info"
            div_ [style_ "background: #f9f9f9; padding: 15px; border-left: 4px solid #ccc;"] $
                toHtml info

asEmail :: EmailAddress -> Enquiry -> Text
asEmail to inquiry@(Enquiry{fullName = fullName}) =
    fmt $
        "To: " +| (decodeUtf8 . toByteString :: EmailAddress -> Text) to |+ "\nSubject: Driving lesson inquiry from " +| fullName |+ "\nMIME-Version: 1.0\nContent-Type: text/html; charset=UTF-8\n\n" +| renderEnquiryEmail inquiry |+ ""

-- | Helper for table rows to keep the DSL clean
row :: Text -> Text -> Html ()
row label value = tr_ $ do
    td_ [style_ "padding: 8px; border-bottom: 1px solid #eee; font-weight: bold; width: 30%;"] (toHtml label)
    td_ [style_ "padding: 8px; border-bottom: 1px solid #eee;"] (toHtml value)

getEmailID :: (MonadIO m) => m UUID
getEmailID = liftIO (randomIO :: IO UUID)

writeEmail :: (MonadIO m) => UUID -> EmailAddress -> Enquiry -> m FilePath
writeEmail uuid to inquiry = do
    let path = "/tmp/email-" <> show uuid
    writeFileText path (asEmail to inquiry)
    return path

sendEmail :: (MonadIO m) => FilePath -> UUID -> FilePath -> m ()
sendEmail queue uuid path = let queuePath = queue <> "/pending/" <> show uuid in writeFileText queuePath ("/usr/bin/send-email.sh " <> toText path)
