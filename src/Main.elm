module Main exposing (..)

import Array as A exposing (Array)
import Browser
import Css exposing (..)
import FontAwesome as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode as D exposing (Decoder)
import Random
import Tailwind.Breakpoints as Tb
import Tailwind.Theme as Tc
import Tailwind.Utilities as Tw
import Time exposing (..)


type alias Review =
    { name : String
    , review : String
    }


type alias Model =
    { reviews : Array Review
    , reviewIndex : Int
    }


type Msg
    = SetReviews (List Review)
    | PrevReview
    | NextReview
    | SetReviewIndex Int


nav_link : String -> String -> Html Msg
nav_link href name =
    a
        [ Attr.href href
        , pStd
        , css
            [ Tw.block
            , Tw.font_extrabold
            , Tw.mr_4
            , Tw.text_color Tc.white
            , Tw.no_underline
            , Tw.decoration_color Tc.indigo_500
            , Tw.decoration_4
            , Css.hover
                [ Tw.underline
                ]
            , Tb.lg
                [ Tw.my_0
                , Tw.inline_block
                ]
            , Tw.my_8
            ]
        ]
        [ text name ]


links : Html Msg
links =
    div [] [ nav_link "#about" "About Me", nav_link "#tutoring" "What to Expect", nav_link "#contact" "Get in Touch" ]


xlText : Style
xlText =
    fontSize (px 96)


lText : Style
lText =
    fontSize (px 64)


mText : Style
mText =
    fontSize (px 24)


sText : Style
sText =
    fontSize (px 16)


h1Std : Attribute Msg
h1Std =
    css [ mText, Tb.md [ lText ], Tw.font_sans, Tw.font_extrabold, Tw.text_color Tc.gray_800 ]


h2Std : Attribute Msg
h2Std =
    css [ sText, Tb.md [ mText ], Tw.font_extrabold, Tw.text_color Tc.gray_800 ]


pStd : Attribute Msg
pStd =
    css [ Tw.mr_2, sText, Tb.md [ mText ] ]


point : String -> String -> Html Msg
point heading body =
    div [ css [ Tw.flex, Tw.flex_col, Tw.flex_grow, Tw.mx_1 ] ]
        [ h3 [ h2Std, css [ Tw.mb_0, Tw.font_sans ] ] [ text heading ]
        , p [ pStd, css [ Tw.font_sans ] ] [ text body ]
        ]


heroButton : String -> String -> Html Msg
heroButton href name =
    a [ Attr.href href, css [ Tw.mx_3, Tw.mt_2, Tw.bg_color Tc.indigo_500, textDecoration none, Tw.text_color Tc.white, Tw.font_sans, Tw.rounded, Tw.border_none, Tw.px_6, Tw.py_4, Tw.font_bold, Tw.shadow_md, Tb.md [ mText ], fontSize (rem 0.9) ] ] [ text name ]


callToAction : Html Msg
callToAction =
    div [ css [ Tw.mx_2, Tw.items_center, Tw.flex, Tw.flex_col, Tw.content_center, justifyContent center, textAlign center, Tw.flex_grow ] ]
        [ h1 [ h1Std, css [ Tw.mb_0, Tw.font_extrabold, fontSize (rem 2), Tb.md [ fontSize (rem 6) ], Tw.text_color Tc.white, Tw.font_sans ] ] [ text "Karori Driving School" ]
        , h2 [ css [ Tw.font_extrabold, fontSize (rem 1), Tb.md [ fontSize (rem 2) ], Tw.text_color Tc.gray_200, Tw.font_sans ] ] [ text "Expert driving instruction, personalised." ]
        , h2 [ css [ Tw.mt_0, Tw.font_extrabold, fontSize (rem 1), Tb.md [ fontSize (rem 2) ], Tw.text_color Tc.gray_200, Tw.font_sans ] ] [ text "One hour lessons: $75 on weekdays, $80 on weekends." ]
        , div [ css [ Tw.flex, Tw.content_center ] ]
            [ heroButton "sms:021440260" "Book Via SMS"
            , heroButton "mailto:lessons@karoridrivingschool.co.nz" "Book Via Email"
            ]
        ]


blendedStyle : Style
blendedStyle =
    property "background-image" "linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(\"/hero.jpg\")"


hero : Html Msg
hero =
    div
        [ css
            [ Tw.relative
            , height (vh 60)
            , Tw.w_full
            , Tw.bg_cover
            , Tw.bg_center
            , blendedStyle
            , Tw.flex
            , Tw.flex_col
            ]
        ]
        [ callToAction
        ]


aboutMsg : List String
aboutMsg =
    [ "Hi, my name is Marcel. I’m a dad to two teenage daughters. Karori has been my home since 2007. Prior to becoming an instructor, I was working in the TV-news industry. I now enjoy running my own driving school business full time. I lived in Switzerland for 6 years and speak German/Swiss German as a second language."
    , "I'm a calm, friendly, and qualified instructor that coaches driving in a positive way. I aim to make each lesson a fun and enjoyable hour. My driving experience stretches back more than 30 years, and I have been instructing since 2020. My students have a very high pass rate, you can trust me to prepare you with confidence for your test. "
    ]


aboutMe : Html Msg
aboutMe =
    div [ Attr.id "about", css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ Tw.mb_0 ] ] [ text "Your Driving Instructor" ]
        , div [ css [ Tw.my_4, Tw.mr_3 ] ]
            [ img [ Attr.src "/marcel.jpg", css [ Tw.w_full, Tb.md [ Tw.w_3over5 ], marginLeft auto, marginRight auto, display block ], Attr.alt "Marcel, your driving instructor, standing beside the Karori Driving School vehicle." ] []
            , div [] (List.map (\msg -> p [ pStd ] [ text msg ]) aboutMsg)
            ]
        ]


carMsg : List (Html Msg)
carMsg =
    [ text "The current vehicle is a 2016 automatic Toyota Aqua fitted with dual controls, so I have a brake and an accelerator to take the stress out of tricky situations. The Aqua is a great, little, and safe car to start your driving experience in."
    ]


aboutCar : Html Msg
aboutCar =
    div [ Attr.id "car", css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ Tw.mb_0 ] ] [ text "The Car" ]
        , div [ css [ Tb.md [ Tw.flex, Css.flexDirection Css.rowReverse ], Tw.my_4, Tw.mr_3 ] ]
            [ img [ Attr.src "/car.jpg", Attr.alt "A white Toyota Aqua with Karori Driving School signage parked on the side of a road.", css [ Tw.w_full, Tb.md [ width (px 500) ] ] ] []
            , div [] (List.map (\msg -> p [ pStd ] [ msg ]) carMsg)
            ]
        ]


lessonsMsg : List (Html Msg)
lessonsMsg =
    [ text "Lessons are tailed to any skill level from the first time behind the wheel to overseas license conversion lessons. Pick up and drop off from your address or place of choice. Covering suburbs including:"
    , ul []
        [ li [] [ text "Karori" ]
        , li [] [ text "Wilton" ]
        , li [] [ text "Northland" ]
        , li [] [ text "Crofton Downs" ]
        , li [] [ text "Kelburn" ]
        , li [] [ text "Thorndon" ]
        , li [] [ text "Wellington Central" ]
        ]
    , text "After each lesson, a feedback and progress chart is emailed to you, which tracks your driving skills. Once you meet test level, we'll practice on the Thorndon test route. Then, you are ready to book your test!"
    ]


aboutLessons : Html Msg
aboutLessons =
    div [ Attr.id "lessons", css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ Tw.mb_0 ] ] [ text "The Lessons" ]
        , div [ css [ Tw.my_4, Tw.mr_3 ] ]
            [ div []
                [ img [ Attr.src "/student.jpg", Attr.alt "Me and a student in my car, mid-lesson, smiling.", css [ Tw.w_full, Tb.md [ Tw.w_3over5 ], marginLeft auto, marginRight auto, display block ] ] []
                , div [] (List.map (\msg -> p [ pStd ] [ msg ]) lessonsMsg)
                ]
            ]
        ]


aboutAvailability : Html Msg
aboutAvailability =
    div [ Attr.id "times", css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ Tw.mb_0 ] ] [ text "Rates and Times" ]
        , div [ css [ Tw.my_4, Tw.mr_3 ] ]
            [ div [ css [ Tw.w_full ] ]
                [ p [ pStd ]
                    [ text "Lessons are $75 per hour on weekdays, and $80 per hour on weekends. My general operating hours are as follows:" ]
                , Html.Styled.table
                    [ css [ Tw.w_full ], pStd ]
                    [ tr []
                        [ td [] [ text "Monday" ]
                        , td [] [ text "Closed" ]
                        ]
                    , tr []
                        [ td [] [ text "Tuesday" ]
                        , td [] [ text "8am — 7pm" ]
                        ]
                    , tr []
                        [ td [] [ text "Wednesday" ]
                        , td [] [ text "8am — 7pm" ]
                        ]
                    , tr []
                        [ td [] [ text "Thursday" ]
                        , td [] [ text "8am — 7pm" ]
                        ]
                    , tr []
                        [ td [] [ text "Friday" ]
                        , td [] [ text "8am — 7pm" ]
                        ]
                    , tr []
                        [ td [] [ text "Saturday" ]
                        , td [] [ text "8am — 7pm" ]
                        ]
                    , tr []
                        [ td [] [ text "Sunday" ]
                        , td [] [ text "Closed" ]
                        ]
                    ]
                ]
            , p [ pStd ] [ text "Don't see a time that works for you? Don't stress! Send me a text or an email and we may be able to work out an alternative time." ]
            ]
        ]


whatToExpect : Html Msg
whatToExpect =
    div [ css [ Tw.mb_7, Tw.px_6 ] ]
        []


getInTouch : Html Msg
getInTouch =
    div [ Attr.id "contact", css [ Tw.mb_12, Tw.flex, Tw.justify_center, Tw.w_full ] ]
        [ div [ css [ Tw.flex, width (pct 90), Tw.flex_col, Tb.md [ Tw.flex_row ], Tb.lg [ Tw.w_3over5 ], justifyContent spaceBetween ] ]
            [ span [ css [ Tw.font_bold ], pStd ]
                [ Icon.envelopeOpen |> Icon.styled [] |> Icon.view |> fromUnstyled
                , a [ Attr.href "mailto:lessons@karoridrivingschool.co.nz", css [ Tw.ml_2 ] ] [ text "lessons@karoridrivingschool.co.nz" ]
                ]
            , span [ css [ Tw.font_bold ], pStd ]
                [ Icon.phone |> Icon.view |> fromUnstyled
                , a [ Attr.href "sms:021440260", css [ Tw.ml_2 ] ] [ text "021440260" ]
                ]
            ]
        ]


split12 : Html Msg -> Html Msg -> Html Msg
split12 left right =
    div [ css [ Tb.lg [ Tw.flex ], Tw.mb_4 ] ]
        [ div [ css [ Tb.lg [ Tw.w_1over3 ] ] ] [ left ]
        , div
            [ css [ Tb.lg [ Tw.w_2over3 ] ] ]
            [ right ]
        ]


rule : Html Msg
rule =
    hr [ css [ Tw.w_full, Tw.my_12 ] ] []


faCss : Html Msg
faCss =
    fromUnstyled Icon.css


reviewBlock : Review -> Html Msg
reviewBlock review =
    div [ css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ justifyContent center ] ]
            [ text <| "Review by " ++ review.name ]
        , p
            [ pStd, css [ height (px 200), Css.overflowY scroll ] ]
            [ text review.review ]
        ]


reviewCarousel : Model -> Html Msg
reviewCarousel model =
    div []
        [ case A.get model.reviewIndex model.reviews of
            Just review ->
                reviewBlock review

            Nothing ->
                div [] []
        , div [ css [ Tw.mx_6, Tw.flex, Tw.justify_center ] ]
            [ button [ css [ Tw.mr_3, Tw.px_3, Tw.py_1, width (px 150) ], onClick PrevReview ] [ Icon.arrowLeft |> Icon.styled [] |> Icon.view |> fromUnstyled ]
            , button [ css [ Tw.px_3, Tw.py_1, width (px 150) ], onClick NextReview ] [ Icon.arrowRight |> Icon.styled [] |> Icon.view |> fromUnstyled ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ css [ Tb.md [ Tw.flex, Tw.flex_col, width (pct 100) ], Tw.items_center, Tw.font_sans ] ]
        [ faCss
        , hero
        , div [ css [ Tb.lg [ width (px 1024) ] ] ]
            [ reviewCarousel model
            , rule
            , aboutMe
            , rule
            , aboutCar
            , rule
            , aboutLessons
            , rule
            , aboutAvailability
            , rule
            , getInTouch
            ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetReviews rs ->
            ( { model | reviews = A.fromList rs }, Random.generate SetReviewIndex (Random.int 0 (List.length rs - 1)) )

        NextReview ->
            ( { model | reviewIndex = modBy (A.length model.reviews) (model.reviewIndex + 1) }, Cmd.none )

        PrevReview ->
            ( { model | reviewIndex = modBy (A.length model.reviews) (model.reviewIndex - 1) }, Cmd.none )

        SetReviewIndex ind ->
            ( { model | reviewIndex = ind }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


loadReviews : Cmd Msg
loadReviews =
    Http.get
        { url = "/reviews.json"
        , expect =
            Http.expectJson
                (SetReviews << Result.withDefault [])
                reviewDecoder
        }


reviewDecoder : Decoder (List Review)
reviewDecoder =
    D.list (D.map2 Review (D.field "name" D.string) (D.field "review" D.string))


init : () -> ( Model, Cmd Msg )
init _ =
    ( { reviews = A.empty
      , reviewIndex = 0
      }
    , Cmd.batch [ loadReviews ]
    )


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = \m -> view m |> toUnstyled }
