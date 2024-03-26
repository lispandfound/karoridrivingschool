module Main exposing (..)

import Browser
import Css exposing (..)
import FontAwesome as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles as Icon
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Encode as E
import Tailwind.Breakpoints as Tb
import Tailwind.Theme as Tc
import Tailwind.Utilities as Tw
import Task
import Time exposing (..)
import Time.Extra exposing (fromIso8601Date, setHour, toIso8601DateTimeUTC)


type Msg
    = Start String
    | Holder String
    | When String
    | HolderEmail String
    | HolderPhone String
    | Location String
    | Submit
    | SubmitOutcome SubmitState
    | SetTime Posix
    | SetHere Zone


logo : Html Msg
logo =
    div
        [ css
            [ Tw.flex_shrink_0
            , Tw.text_color Tc.white
            , Tw.mr_6
            ]
        ]
        [ span
            [ pStd
            , css
                [ Tb.lg
                    [ Tw.justify_end
                    ]
                , Tw.font_semibold
                , Tw.tracking_tight
                ]
            ]
            [ text "Jake Faulkner | NCEA Tutor" ]
        ]


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


navbar : Html Msg
navbar =
    nav
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_between
            , Tw.font_sans
            , Tw.flex_wrap
            , Tw.p_6
            ]
        ]
        [ logo
        , links
        ]


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
    css [ sText, Tb.md [ lText ], Tw.font_sans, Tw.font_extrabold, Tw.text_color Tc.gray_800 ]


h2Std : Attribute Msg
h2Std =
    css [ sText, Tb.md [ mText ], Tw.font_extrabold, Tw.text_color Tc.gray_800 ]


pStd : Attribute Msg
pStd =
    css [ Tw.mr_2, sText ]


lipsum : String
lipsum =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed nec erat"
        ++ "nunc. Nulla et est orci. Etiam in tempus nibh. Quisque luctus iaculis ex, at "
        ++ "varius quam ornare convallis. Maecenas vestibulum sapien quis nisi euismod."


point : String -> String -> Html Msg
point heading body =
    div [ css [ Tw.flex, Tw.flex_col, Tw.flex_grow, Tw.mx_1 ] ]
        [ h3 [ h2Std, css [ Tw.mb_0, Tw.font_sans ] ] [ text heading ]
        , p [ pStd, css [ Tw.font_sans ] ] [ text body ]
        ]


bulletPoints : Html Msg
bulletPoints =
    div [ css [ Tb.md [ Tw.flex ], Tw.my_7, justifyContent center, Tw.mx_6 ] ]
        [ point "Quality Tuition" lipsum
        , point "Working for You" lipsum
        , point "Innovative Teaching Models" lipsum
        ]


heroButton : String -> String -> Html Msg
heroButton href name =
    a [ Attr.href href, css [ Tw.mx_3, Tw.mt_2, Tw.bg_color Tc.indigo_500, textDecoration none, Tw.text_color Tc.white, Tw.font_sans, Tw.rounded, Tw.border_none, Tw.p_6, Tw.font_bold, Tw.shadow_md, Tb.md [ mText ], sText ] ] [ text name ]


callToAction : Html Msg
callToAction =
    div [ css [ Tw.mx_2, Tw.items_center, Tw.flex, Tw.flex_col, Tw.content_center, justifyContent center, Tw.flex_grow ] ]
        [ h1 [ h1Std, css [ Tw.mb_0, Tw.font_extrabold, Tw.text_color Tc.white, Tw.font_sans ] ] [ text "Quality Tutoring" ]
        , h2 [ css [ Tw.mt_0, Tw.font_extrabold, fontSize (rem 2), Tw.text_color Tc.gray_200, Tw.font_sans ] ] [ text "Some compelling sell" ]
        , div [ css [ Tw.flex, Tw.content_center ] ]
            [ heroButton "tel:0226219016" "Call Now"
            , heroButton "mailto:jakefaulkn@gmail.com" "Book Via Email"
            ]
        ]


blendedStyle : Style
blendedStyle =
    property "background-image" "linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url(\"https://images.unsplash.com/photo-1459262838948-3e2de6c1ec80?ixlib=rb-1.2.1&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=800&q=80\")"


hero : Html Msg
hero =
    div
        [ css
            [ Tw.relative
            , height (vh 75)
            , Tb.md [ height (vh 65) ]
            , Tw.w_full
            , Tw.bg_cover
            , Tw.bg_center
            , blendedStyle
            , Tw.flex
            , Tw.flex_col
            ]
        ]
        [ navbar
        , callToAction
        ]


aboutMe : Html Msg
aboutMe =
    div [ Attr.id "about", css [ Tw.mt_7, Tw.mx_6 ] ]
        [ h1 [ h1Std, css [ Tw.mb_0 ] ] [ text "About Me" ]
        , div [ css [ Tw.flex, Tw.my_7, Tw.mr_3 ] ]
            [ div [ css [ Tw.mr_3 ] ]
                [ p [ pStd ] [ text lipsum ]
                , h2 [ h2Std ] [ text "Subjects I Tutor" ]
                , ul [ pStd ]
                    [ li [] [ text "English" ]
                    , li [] [ text "Mathematics" ]
                    , li [] [ text "Statistics" ]
                    , li [] [ text "Physics" ]
                    , li [] [ text "Digital Technoligies and Computer Science" ]
                    ]
                ]
            , img [ Attr.src "https://placehold.co/150x260", Attr.alt "Headshot of me!", css [ maxWidth (px 150), maxWidth (px 260), alignSelf baseline ] ] []
            ]
        , h1
            [ Attr.id "tutoring"
            , h1Std
            , css [ Tw.mb_0 ]
            ]
            [ text "How I Tutor" ]
        , p [ pStd ] [ text <| lipsum ++ "\n" ++ lipsum ]
        ]


whatToExpect : Html Msg
whatToExpect =
    div [ css [ Tw.mb_7, Tw.px_6 ] ]
        []


getInTouch : Html Msg
getInTouch =
    div [ Attr.id "contact", css [ Tw.mb_12, Tw.flex, Tw.justify_center, Tw.w_full ] ]
        [ div [ css [ Tw.flex, width (pct 90), Tb.lg [ Tw.w_3over5 ], justifyContent spaceBetween ] ]
            [ span [ css [ Tw.font_bold ] ]
                [ Icon.envelopeOpen |> Icon.styled [] |> Icon.view |> fromUnstyled
                , a [ Attr.href "mailto:jakefaulkn@gmail.com", pStd, css [ Tw.ml_2 ] ] [ text "jakefaulkn@gmail.com" ]
                ]
            , span [ css [ Tw.font_bold ] ]
                [ Icon.phone |> Icon.view |> fromUnstyled
                , a [ Attr.href "tel:0226219016", pStd, css [ Tw.ml_2 ] ] [ text "0226219016" ]
                ]
            ]
        ]


submissionError : Html Msg
submissionError =
    p [ css [ Tw.text_color Tc.red_800 ] ] [ text "Something went wrong! Try again later, or contact me below." ]


bookNow : Model -> Html Msg
bookNow model =
    div [ Attr.id "book", css [ Tw.mb_7, Tw.px_6 ] ]
        [ h1 [ css [ Tw.mb_0, Tw.font_extrabold, xlText, Tb.lg [ lText ], Tw.font_sans, Tw.text_color Tc.gray_800 ] ] [ text "Book a Tutorial" ]
        , div [ css [ Tw.mr_2, Tw.my_4 ] ]
            ([ bookInput "text" "Full Name" "Jane Doe" Holder
             , bookInput "email" "Email" "janedoe@example.com" HolderEmail
             , bookInput "tel" "Phone Number" "" HolderPhone
             , dateInput model
             ]
                ++ (Maybe.withDefault [] <| Maybe.map List.singleton <| timeSelector model)
                ++ [ selector "Where" Location [ ( "Online", "Online" ), ( "In Person (To be arranged)", "In Person" ) ]
                   , bookButton
                   ]
                ++ (if model.submitted == Error then
                        [ submissionError ]

                    else
                        []
                   )
            )
        ]


bookButton : Html Msg
bookButton =
    button [ onClick Submit, css [ Tw.mt_2, Tw.w_full, Tw.bg_color Tc.indigo_500, Tw.text_color Tc.white, Tw.font_sans, Tw.rounded, Tw.border_none, Tw.text_xl, Tw.p_2, Tw.font_bold ] ] [ text "Book" ]


hourToTime : Int -> String
hourToTime t =
    let
        suffix =
            if modBy 24 t >= 12 then
                "pm"

            else
                "am"
    in
    case modBy 12 t of
        0 ->
            "12" ++ suffix

        h ->
            String.fromInt h ++ suffix


selector : String -> (String -> Msg) -> List ( String, String ) -> Html Msg
selector l toMsg opts =
    let
        options =
            List.map (\( o, v ) -> option [ Attr.value v ] [ text o ]) opts
    in
    split12 (bookingLabel l) (select [ onInput toMsg, css [ Tw.text_xl, Tw.w_full, Tw.text_xl, Tw.border_0, Tw.border_color Tc.gray_200, boxSizing borderBox, Tw.rounded, Tw.py_2, Tw.px_4, Tw.text_color Tc.gray_700, Tw.bg_color Tc.gray_200, Tw.leading_tight ] ] options)


availabilityToLabel : List Int -> List ( String, String )
availabilityToLabel =
    List.map
        (\i ->
            ( hourToTime i
                ++ "-"
                ++ hourToTime
                    (i + 1)
            , String.fromInt i
            )
        )


timeSelector : Model -> Maybe (Html Msg)
timeSelector model =
    Maybe.map (selector "Time" When << availabilityToLabel) model.availability


bookingLabel : String -> Html Msg
bookingLabel l =
    label [ css [ Tw.block, Tw.font_bold, Tw.pr_4, lText, Tb.lg [ mText ], Tw.my_2, Tb.lg [ Tw.my_0 ] ] ] [ text l ]


split12 : Html Msg -> Html Msg -> Html Msg
split12 left right =
    div [ css [ Tb.lg [ Tw.flex ], Tw.mb_4 ] ]
        [ div [ css [ Tb.lg [ Tw.w_1over3 ] ] ] [ left ]
        , div
            [ css [ Tb.lg [ Tw.w_2over3 ] ] ]
            [ right ]
        ]


bookInput : String -> String -> String -> (String -> Msg) -> Html Msg
bookInput t l place toMsg =
    div [ css [ Tw.my_4, Tb.lg [ Tw.my_0 ] ] ]
        [ split12 (bookingLabel l)
            (input
                [ onInput toMsg, Attr.type_ t, Attr.placeholder place, css [ Tw.w_full, lText, Tb.lg [ mText ], Tw.appearance_none, Tw.border_8, Tb.lg [ Tw.border_2 ], Tw.border_color Tc.transparent, boxSizing borderBox, Tw.rounded, Tw.py_2, Tw.px_4, Tw.text_color Tc.gray_700, Tw.bg_color Tc.gray_200, Tw.leading_tight, focus [ Tw.outline_none, Tw.bg_color Tc.white, Tw.border_2, Tw.border_color Tc.indigo_500 ] ] ]
                []
            )
        ]


monthToInt : Month -> Int
monthToInt m =
    case m of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


toIsoString : Zone -> Posix -> String
toIsoString zone p =
    (toYear zone p |> String.fromInt) ++ (toMonth zone p |> monthToInt |> String.fromInt |> String.padLeft 2 '0') ++ (toDay zone p |> String.fromInt |> String.padLeft 2 '0')


dateInput : Model -> Html Msg
dateInput model =
    split12 (bookingLabel "When")
        (input
            ([ onInput Start
             , Attr.type_ "date"
             , css [ Tw.w_full, Tw.text_xl, Tw.appearance_none, Tw.border_2, Tw.border_color Tc.transparent, boxSizing borderBox, Tw.rounded, Tw.py_2, Tw.px_4, Tw.text_color Tc.gray_700, Tw.bg_color Tc.gray_200, Tw.leading_tight, focus [ Tw.outline_none, Tw.bg_color Tc.white, Tw.border_2, Tw.border_color Tc.indigo_500 ] ]
             ]
                ++ (case ( model.here, model.today ) of
                        ( Just zone, Just today ) ->
                            [ Attr.min <| toIsoString zone today ]

                        _ ->
                            []
                   )
            )
            []
        )


rule : Html Msg
rule =
    hr [ css [ Tw.w_full, Tw.my_12 ] ] []


faCss : Html Msg
faCss =
    fromUnstyled Icon.css


submitted : Html Msg
submitted =
    h1 [ css [ Tw.mb_0, Tw.font_extrabold, fontSize (rem 4), Tw.font_sans, Tw.text_color Tc.gray_800 ] ] [ text "All set! I will be touch shortly" ]


view : Model -> Html Msg
view _ =
    div [ css [ Tb.md [ Tw.flex, Tw.flex_col, width (pct 100) ], Tw.items_center, Tw.font_sans ] ]
        [ faCss
        , hero
        , div [ css [ Tb.lg [ width (px 1024) ] ] ]
            [ bulletPoints
            , rule
            , aboutMe
            , whatToExpect
            , rule
            , getInTouch
            ]
        ]


type SubmitState
    = InProgress
    | Submitted
    | Error


type alias Model =
    { start : Maybe Posix
    , duration : Int
    , when : Maybe Int
    , kind : Maybe String
    , holder : Maybe String
    , holderEmail : Maybe String
    , holderPhone : Maybe String
    , location : Maybe String
    , description : Maybe String
    , submitted : SubmitState
    , today : Maybe Posix
    , here : Maybe Zone
    , availability : Maybe (List Int)
    }


doThen : Maybe a -> (a -> Maybe b) -> Maybe b
doThen m f =
    Maybe.andThen f m


encode : Model -> Maybe E.Value
encode model =
    -- AAAHHHHHHH!!!!!!! Why does elm not have monads and do notation :(
    -- In haskell this is just
    -- do
    --   uuid <- model.uuid
    --   etc...
    --   E.object [...]
    -- No nesting!
    -- I did check reddit and stack overflow to see if there was a canonical "elmish" way to do something like this.
    -- No, no there is not. Elm is too opinionated to have a solution for this
    doThen model.start <|
        \start ->
            doThen model.when <|
                \when ->
                    doThen model.kind <|
                        \kind ->
                            doThen model.holder <|
                                \holder ->
                                    doThen model.holderEmail <|
                                        \holderEmail ->
                                            doThen model.holderPhone <|
                                                \holderPhone ->
                                                    doThen model.location <|
                                                        \location ->
                                                            doThen model.here <|
                                                                \zone ->
                                                                    Just <|
                                                                        E.object
                                                                            [ -- ( "start", E.string <| Date.toIsoString start ++ "T" ++ String.padLeft 2 '0' (String.fromInt when) ++ ":00Z" )
                                                                              ( "start", E.string <| toIso8601DateTimeUTC <| setHour zone when <| start )
                                                                            , ( "duration", E.int model.duration )
                                                                            , ( "kind", E.string kind )
                                                                            , ( "holder", E.string holder )
                                                                            , ( "holderEmail", E.string holderEmail )
                                                                            , ( "holderPhone", E.string holderPhone )
                                                                            , ( "location", E.string location )
                                                                            , ( "description", E.string <| Maybe.withDefault "" <| model.description )
                                                                            ]


createBooking : Model -> Cmd Msg
createBooking model =
    case encode model of
        Just json ->
            Http.post
                { url = "http://localhost:8081/reservation"
                , body =
                    Http.jsonBody
                        json
                , expect =
                    Http.expectWhatever
                        (\res ->
                            case res of
                                Ok _ ->
                                    SubmitOutcome Submitted

                                Err _ ->
                                    SubmitOutcome Error
                        )
                }

        Nothing ->
            Cmd.none


resultToMaybe : Result a b -> Maybe b
resultToMaybe r =
    case r of
        Ok x ->
            Just x

        _ ->
            Nothing



-- Elm doesn't have a native function for this (why?). So we have to do this ourselves...


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start s ->
            case model.here of
                Just zone ->
                    ( { model | start = fromIso8601Date zone s, availability = Just (List.range 0 23) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Holder s ->
            ( { model | holder = Just s }, Cmd.none )

        When h ->
            ( { model | when = String.toInt h }, Cmd.none )

        HolderEmail e ->
            ( { model | holderEmail = Just e }, Cmd.none )

        HolderPhone p ->
            ( { model | holderPhone = Just p }, Cmd.none )

        Location l ->
            ( { model | location = Just l }, Cmd.none )

        Submit ->
            ( model, createBooking model )

        SetTime t ->
            ( { model | today = Just t }, Cmd.none )

        SetHere h ->
            ( { model | here = Just h }, Cmd.none )

        SubmitOutcome o ->
            ( { model | submitted = o }, Cmd.none )


now : Cmd Msg
now =
    Task.perform SetTime Time.now


here : Cmd Msg
here =
    Task.perform SetHere Time.here


init : () -> ( Model, Cmd Msg )
init _ =
    ( { start = Nothing
      , duration = 60 * 60
      , when = Nothing
      , kind = Just "Tutorial"
      , holder = Nothing
      , holderEmail = Nothing
      , holderPhone = Nothing
      , location = Just "Online"
      , description = Nothing
      , submitted = InProgress
      , today = Nothing
      , availability = Nothing
      , here = Nothing
      }
    , Cmd.batch [ here, now ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = \m -> view m |> toUnstyled }
