module Review (Review (..), createTable, setReviews, getReviews, reviewAdminPage, newReviewFragment) where

import Data.Aeson
import Database.SQLite.Simple
import Lucid
import Lucid.Base (makeAttribute)
import Web.FormUrlEncoded (FromForm (..))

data Review = Review
    { name :: Text
    , review :: Text
    }
    deriving (Generic, Show)

instance ToJSON Review
instance FromRow Review
instance ToRow Review
instance FromForm Review

createTable :: Connection -> IO ()
createTable conn = execute_ conn "CREATE TABLE IF NOT EXISTS reviews (reviewId INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, review TEXT NOT NULL)"

dropTable :: Connection -> IO ()
dropTable conn = execute_ conn "DROP TABLE reviews"

setReviews :: Connection -> [Review] -> IO ()
setReviews conn reviews = dropTable conn >> createTable conn >> executeMany conn "INSERT INTO reviews (name, review) VALUES (?, ?)" reviews

getReviews :: Connection -> IO [Review]
getReviews conn = query_ conn "SELECT name, review FROM reviews"

-- HTMX specific attributes
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

reviewAdminPage :: [Review] -> Html ()
reviewAdminPage reviews = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "UTF-8"]
        title_ "Manage Reviews - Karori Driving School"
        link_ [rel_ "stylesheet", href_ "/style.css"]
        script_ [src_ "/htmx.min.js"] ("" :: Text)

    body_ $ div_ [class_ "page-container"] $ div_ [class_ "content-area"] $ do
        section_ [class_ "section-block form-section", style_ "max-width: 950px;"] $ do
            h1_ [class_ "section-title", style_ "margin-bottom: 1.5rem;"] "Review Management"

            form_
                [ id_ "review-update-form"
                , hxPost_ "/api/reviews"
                , hxTarget_ "#toast"
                ]
                $ do
                    div_ [class_ "table-scroll-container"] $
                        table_ [] $ do
                            thead_ $ tr_ $ do
                                th_ [style_ "width: 25%;"] "Student Name"
                                th_ [style_ "width: 60%;"] "Review Content"
                                th_ [style_ "width: 15%; text-align: center;"] $ do
                                    span_ [style_ "margin-right: 8px;"] "Action"
                                    -- PERSISTENT ADD BUTTON (in header)
                                    button_
                                        [ type_ "button"
                                        , class_ "action-btn header-add"
                                        , title_ "Add new review at top"
                                        , hxGet_ "/api/reviews/new"
                                        , hxTarget_ "#reviews-tbody"
                                        , hxSwap_ "afterbegin"
                                        ]
                                        "+"

                            tbody_ [id_ "reviews-tbody"] $ do
                                mapM_ reviewRow reviews

                    div_ [class_ "submit-container", style_ "margin-top: 1.5rem;"] $ do
                        button_ [type_ "submit", class_ "button submit-btn"] "Bulk Save All Changes"
                        output_ [id_ "toast", style_ "display: block; margin-top: 1rem; color: #059669;"] ""

-- | The Row Helper
reviewRow :: Review -> Html ()
reviewRow (Review rName rText) = tr_ [style_ "border-bottom: 1px solid #f3f4f6;"] $ do
    td_ [style_ "padding: 0; vertical-align: top; border-bottom: 1px solid #f3f4f6;"] $
        input_
            [ type_ "text"
            , name_ "name"
            , class_ "name-input"
            , value_ rName
            , required_ "required"
            ]

    td_ [class_ "full-cell-editor", style_ "border-bottom: 1px solid #f3f4f6;"] $
        textarea_
            [ name_ "review"
            , required_ "required"
            ]
            (toHtml rText)

    td_ [style_ "vertical-align: middle; border-bottom: 1px solid #f3f4f6;"] $
        div_ [class_ "action-container"] $ do
            -- INLINE ADD BUTTON (Insert below current)
            button_
                [ type_ "button"
                , class_ "action-btn btn-add"
                , title_ "Insert review below"
                , hxGet_ "/api/reviews/new"
                , hxTarget_ "closest tr"
                , hxSwap_ "afterend"
                ]
                "+"

            -- DELETE BUTTON (With confirmation)
            button_
                [ type_ "button"
                , class_ "action-btn btn-del"
                , title_ "Remove"
                , makeAttribute "hx-on:click" "if(confirm('Are you sure you want to delete this review?')) this.closest('tr').remove()"
                ]
                "✕"

newReviewFragment :: Html ()
newReviewFragment = reviewRow (Review mempty mempty)
