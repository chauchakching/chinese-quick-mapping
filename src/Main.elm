port module Main exposing (main)

-- import ChineseQuickMapping exposing (chineseQuickMapping)
-- import Debug exposing (log)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, load, pushUrl)
import Colors exposing (blue, blueFilterStyle, green1, green1FilterStyle, green2, green2FilterStyle, orange, orangeFilterStyle, red, redFilterStyle)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, br, button, div, figcaption, figure, footer, h1, h2, header, img, main_, p, span, text, textarea)
import Html.Attributes exposing (attribute, class, classList, href, placeholder, rows, src, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import KeyToQuickUnit exposing (keyToQuickUnit)
import List exposing (head)
import List.Extra exposing (last)
import Maybe.Extra exposing (traverse)
import QS
import Task exposing (attempt)
import Url exposing (Url)



-- dummy log for production build


log src x =
    x


repoHref : String
repoHref =
    "https://github.com/chauchakching/chinese-quick-mapping"


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = updateWithStorage
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type Msg
    = Typing String
    | Clear
    | ClickedQuickMode
    | ClickedNonQuickMode
    | NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotQuickMapping (Result Http.Error QuickMapping)
    | OpenCharDecompModal Char
    | OpenAboutUsModal
    | CloseModal


type alias QuickMapping =
    Dict String String


type ModalViewCase
    = CharDecompView
    | AboutUsView


type alias Model =
    { key : Key
    , url : Url
    , count : Int

    -- to convert texts to quick or cangjie codes
    , quick : Bool

    -- user's input for conversion
    , content : String
    , inputHistory : InputHistory
    , quickMapping : QuickMapping
    , modalVisible : Bool
    , modalChar : Char
    , modalViewCase : ModalViewCase
    }



-- dummy flag


type alias Flags =
    E.Value


type alias DecodedFlags =
    { inputHistory : InputHistory
    , quickMapping : QuickMapping
    }


type alias Storage =
    { inputHistory : InputHistory
    }


type alias InputHistory =
    List String


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        q =
            getTextFromUrl url

        decodedFlags =
            case D.decodeValue decoder flags of
                Ok x ->
                    x

                Err _ ->
                    log "Error in parsing flags data" <| DecodedFlags [] Dict.empty

        fetchQuickMapping =
            Http.get
                { url = "assets/ChineseQuickMappingSmall.json"
                , expect = Http.expectJson GotQuickMapping (D.dict D.string)
                }
    in
    ( { key = key
      , url = url
      , count = 0
      , quick = True
      , content = Maybe.withDefault "速成輸入法，或稱簡易輸入法，亦作速成或簡易，為倉頡輸入法演化出來的簡化版本。" q
      , inputHistory = decodedFlags.inputHistory
      , quickMapping = decodedFlags.quickMapping
      , modalVisible = False
      , modalChar = '速'
      , modalViewCase = CharDecompView
      }
    , Cmd.batch [ focusTextarea, fetchQuickMapping ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        ClickedQuickMode ->
            ( { model | quick = True }, Cmd.none )

        ClickedNonQuickMode ->
            ( { model | quick = False }, Cmd.none )

        Typing newContent ->
            onContentUpdated newContent model

        Clear ->
            let
                ( newModel, cmd ) =
                    onContentUpdated "" model
            in
            ( newModel, Cmd.batch [ cmd, focusTextarea ] )

        GotQuickMapping result ->
            case result of
                Ok quickMapping ->
                    ( { model | quickMapping = quickMapping }
                    , setQuickMapping (encodeQuickMapping quickMapping)
                    )

                Err _ ->
                    ( { model | quickMapping = Dict.empty }, Cmd.none )

        OpenCharDecompModal char ->
            ( { model | modalVisible = True, modalChar = char, modalViewCase = CharDecompView }, Cmd.none )

        OpenAboutUsModal ->
            ( { model | modalVisible = True, modalViewCase = AboutUsView }, Cmd.none )

        CloseModal ->
            ( { model | modalVisible = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode <| Storage oldModel.inputHistory), cmds ]
    )



-- update model & url query


onContentUpdated : String -> Model -> ( Model, Cmd Msg )
onContentUpdated newContent model =
    let
        newUrl =
            updateQuery "q" newContent model.url

        newInputHistory =
            updateInputHistory newContent model.inputHistory
    in
    ( { model | content = newContent, url = newUrl, inputHistory = newInputHistory }
    , pushUrl model.key <| Url.toString newUrl
    )


updateInputHistory : String -> InputHistory -> InputHistory
updateInputHistory newContent inputHistory =
    let
        shouldAppendHistory =
            newContent
                /= ""
                && (head inputHistory
                        |> Maybe.map (String.startsWith newContent)
                        |> Maybe.withDefault False
                        |> not
                   )
                && (not <| List.member newContent inputHistory)

        shouldUpdateLastHistory =
            head inputHistory
                |> Maybe.map (\x -> String.startsWith x newContent)
                |> Maybe.withDefault False
    in
    List.take 10
        (if shouldAppendHistory && shouldUpdateLastHistory then
            List.append [ newContent ] (Maybe.withDefault [] <| List.tail inputHistory)

         else if shouldAppendHistory then
            List.append [ newContent ] inputHistory

         else
            inputHistory
        )


view : Model -> Document Msg
view model =
    { title = "速成查字"
    , body =
        [ div [ classes [ "container", "mx-auto", "px-4", "h-screen", "flex", "flex-col", "justify-between" ], style "max-width" "60rem" ]
            [ main_ []
                -- head
                [ header []
                    [ h1 [ classes [ "text-5xl", "text-gray-900", "text-center", "pt-12", "pb-8", "sm:pt-24", "sm:pb-16" ] ] [ text "速成查字" ] ]

                -- buttons
                , div []
                    [ div [ classes [ "flex", "flex-row", "justify-between", "mb-4" ] ]
                        [ div [ classes [ "flex", "flex-row" ] ]
                            [ div [ classes [ "flex-0", "w-20", "-mr-4" ] ] [ clearButton "清空" [ classes [ "rounded", "bg-white" ], onClick Clear ] ] ]
                        , div [ classes [ "flex", "flex-row" ] ]
                            [ div
                                [ classes [ "flex-0", "w-20", "-mr-4" ] ]
                                [ topButton model.quick "速成" [ class "rounded-l", onClick ClickedQuickMode ] ]
                            , div
                                [ classes [ "flex-0", "w-20", "-mr-3" ] ]
                                [ topButton (not model.quick) "倉頡" [ class "rounded-r", onClick ClickedNonQuickMode ] ]
                            ]
                        ]
                    ]

                -- 2 boxes
                , div []
                    [ div [ classes [ "flex", "flex-col", "sm:flex-row", "items-stretch" ] ]
                        [ div
                            -- "flex" makes the child textarea height dynamic
                            [ classes [ "flex-1", "p-2", "border", "rounded-t", "sm:rounded-b", "bg-white", "shadow-md", "flex" ] ]
                            [ textarea
                                [ Html.Attributes.id "user-input"
                                , placeholder "輸入字句"
                                , value model.content
                                , onInput Typing
                                , rows 6
                                , classes [ "w-full", "outline-none", "resize-none" ]
                                ]
                                []
                            ]
                        , div
                            [ classes
                                [ "flex-1"
                                , "px-2"
                                , "pt-3"
                                , "pb-1"
                                , "border-l"
                                , "border-r"
                                , "border-b"

                                -- , "sm:border-t"
                                , "sm:border-0"
                                , "rounded-b"
                                , "sm:rounded-t"
                                , "sm:ml-4"
                                , "flex"
                                , "content-start"
                                , "flex-wrap"
                                , "bg-white"
                                , "shadow-md"
                                , "relative"
                                ]
                            , attribute "data-testid" "char-box-container"
                            ]
                            (model.content
                                |> String.toList
                                |> List.map
                                    (chineseToParts model.quickMapping model.quick >> (\( ch, parts ) -> charBox ch parts))
                            )
                        ]
                    ]

                -- history entries
                , div
                    [ classes [ "flex", "flex-row", "flex-wrap", "items-stretch" ]
                    , attribute "data-testid" "history-entries"
                    ]
                    [ div
                        [ classes [ "flex-1", "mt-2" ] ]
                        (List.map (\x -> historyEntry x [ onClick <| Typing x ]) model.inputHistory)
                    , div [ classes [ "sm:flex-1" ] ] []
                    ]
                ]

            -- footer
            , footer
                [ classes [ "py-4", "flex", "flex-row", "items-center" ] ]
                [ div [ classes [ "flex-1" ] ] [ button [ classes [ "text-sm", "opacity-60", "cursor-pointer" ], onClick OpenAboutUsModal ] [ text "關於速成查字" ] ]
                , a [ href repoHref ] [ img [ src "assets/GitHub-Mark-64px.png", classes [ "h-8" ] ] [] ]
                ]
            ]

        -- modal
        , div
            [ classes
                [ "fixed"
                , "z-10"
                , "w-full"
                , "h-full"
                , "left-0"
                , "top-0"
                , "p-8"
                , "flex"
                , "items-center"
                , "justify-center"
                ]
            , classes
                (if model.modalVisible then
                    []

                 else
                    [ "pointer-events-none", "opacity-0" ]
                )
            , style "background-color" "rgba(0,0,0,0.4)"
            , style "transition" "opacity 0.15s ease"
            , attribute "data-testid" "modal"
            , onClick CloseModal
            ]
            [ button
                [ classes
                    [ if model.modalVisible then
                        ""

                      else
                        "hidden"
                    , "bg-white"
                    , "rounded"
                    , "flex"
                    , "flex-col"
                    , "items-center"
                    , "justify-center"
                    ]
                ]
                (viewModal model)
            ]
        ]
    }


viewModal : Model -> List (Html Msg)
viewModal model =
    case model.modalViewCase of
        CharDecompView ->
            [ decompositionImages model.modalChar
            , decompositionCodes (chineseToParts model.quickMapping False model.modalChar |> Tuple.second)
            ]

        AboutUsView ->
            [ div [ classes [ "p-8", "w-full", "max-w-lg", "text-left" ] ]
                [ h2 [ classes [ "text-center", "text-4xl", "font-bold" ] ] [ text "咩黎？" ]
                , br [] []
                , p [] [ text "查找中文字嘅速成碼或者倉頡碼。點擊有速成碼嘅字，會顯示字碼拆解圖。" ]
                , br [] []
                , p [] [ text "呢個係Single page app，亦唔需要經backend做字碼轉換，所以真係\"超快\"。所有資料同運算都只會喺你部手機／電腦上。" ]
                , br [] []
                , p [] [ text "暫時主要係手機上面用。如果喺電腦用嘅話，要輸入中文字係一個有雞先定蛋先嘅問題。" ]
                , br [] []
                , p [] [ text "會考慮加入滑鼠寫字功能。" ]
                ]
            ]



-- no `select` porting from elm core yet


port select : String -> Cmd msg


port setStorage : E.Value -> Cmd msg


port setQuickMapping : E.Value -> Cmd msg


getTextFromUrl : Url -> Maybe String
getTextFromUrl url =
    url.query
        |> Maybe.withDefault ""
        |> QS.parse QS.config
        |> QS.getAsStringList "q"
        |> head


updateQuery : String -> String -> Url -> Url
updateQuery k v url =
    let
        newQuery =
            url.query
                |> Maybe.withDefault ""
                |> QS.parse QS.config
                |> QS.setStr k v
                |> QS.serialize QS.config
                -- remove excess leading "?"
                |> String.dropLeft 1
    in
    { url | query = Just newQuery }


focusTextarea : Cmd Msg
focusTextarea =
    Cmd.batch [ attempt (\_ -> NoOp) (focus "user-input"), select "user-input" ]


alphabetToQuickUnit : Char -> Maybe Char
alphabetToQuickUnit a =
    Dict.get a keyToQuickUnit


chineseToParts : QuickMapping -> Bool -> Char -> ( Char, String )
chineseToParts mapping isQuick ch =
    let
        keyboardKeys : Maybe String
        keyboardKeys =
            Dict.get (String.fromChar ch) mapping

        chineseToQuickUnits : Maybe String
        chineseToQuickUnits =
            keyboardKeys
                |> Maybe.andThen
                    (\keys ->
                        keys
                            |> String.toList
                            |> traverse alphabetToQuickUnit
                            |> Maybe.map String.fromList
                    )

        quickUnits =
            chineseToQuickUnits
                |> Maybe.withDefault ""

        parts =
            if isQuick then
                quickUnitsToParts quickUnits

            else
                quickUnits
    in
    ( ch, parts )


quickUnitsToParts : String -> String
quickUnitsToParts quickUnits =
    if String.length quickUnits < 2 then
        quickUnits

    else
        let
            charsToString a b =
                String.fromList [ a, b ]

            firstChar =
                quickUnits
                    |> String.toList
                    |> head

            lastChar =
                quickUnits
                    |> String.toList
                    |> last
        in
        Maybe.map2 charsToString firstChar lastChar
            |> Maybe.withDefault ""


clearButton : String -> List (Attribute Msg) -> Html Msg
clearButton content extraHtmlAttributes =
    let
        htmlAttributes =
            List.append
                [ classes
                    [ "text-center"
                    , "block"
                    , "border"

                    -- , "border-blue-500"
                    , "rounded"
                    , "py-1"
                    , "px-4"
                    , "text-white"
                    , "text-blue-500"
                    , "hover:bg-gray-200"
                    , "shadow"
                    ]
                ]
                extraHtmlAttributes
    in
    button htmlAttributes [ text content ]


topButton : Bool -> String -> List (Attribute Msg) -> Html Msg
topButton active content extraHtmlAttributes =
    let
        htmlAttributes =
            List.append
                [ classes
                    [ "text-center"
                    , "block"
                    , "border"
                    , "border-blue-300"

                    -- , "rounded"
                    , "py-1"
                    , "px-4"
                    , "text-white"
                    , "shadow"
                    ]
                , classList
                    [ ( "bg-blue-500", active )
                    , ( "hover:bg-blue-700", active )

                    -- , ("border-blue-500", active)
                    -- , ("hover:border-gray-200", not active)
                    , ( "text-blue-500", not active )
                    , ( "bg-white", not active )
                    , ( "hover:bg-gray-200", not active )
                    ]
                ]
                extraHtmlAttributes
    in
    button htmlAttributes [ text content ]


charBox : Char -> String -> Html Msg
charBox chineseWord parts =
    let
        hasParts =
            String.length parts > 0

        dom =
            if hasParts then
                button

            else
                div
    in
    dom
        [ classes
            [ "flex"
            , "flex-col"
            , "items-center"
            , "mx-1"
            , "mb-2"
            , "px-2.5"
            , "py-1.5"
            ]
        , classList
            [ ( "hover:bg-gray-100", hasParts )
            , ( "border", hasParts )
            , ( "rounded-lg", hasParts )
            , ( "shadow-sm", hasParts )
            ]
        , attribute "data-testid" "char-box"
        , attribute "data-box-char" (String.fromChar chineseWord)
        , onClick
            (if hasParts then
                OpenCharDecompModal chineseWord

             else
                NoOp
            )
        ]
        [ div [ classes [ "flex", "flex-row", "text-2xl", "leading-8" ] ] [ text (String.fromChar chineseWord) ]
        , div [ classes [ "flex", "flex-row", "text-xs", "text-gray-500" ] ] [ text parts ]
        ]


historyEntry : String -> List (Attribute Msg) -> Html Msg
historyEntry str extraAttributes =
    button
        (List.append
            [ classes
                [ "bg-white"
                , "hover:bg-gray-200"
                , "border"
                , "mr-1"
                , "mt-2"
                , "px-2"
                , "rounded-full"
                , "truncate"
                ]
            , style "max-width" "180px"
            , attribute "data-testid" "history-entry-button"
            ]
            extraAttributes
        )
        [ text str ]


decompositionImages : Char -> Html Msg
decompositionImages char =
    figure
        [ classes [ "relative", "mt-12" ], style "width" "270px", style "height" "192px" ]
        [ img [ src ("static/chars/" ++ String.fromChar char ++ "/part_0.svg"), classes [ "w-full", "h-auto", "absolute", "opacity-10" ] ] []
        , img [ src ("static/chars/" ++ String.fromChar char ++ "/part_1.svg"), classes [ "w-full", "h-auto", "absolute" ], getColorFilter 0 ] []
        , img [ src ("static/chars/" ++ String.fromChar char ++ "/part_2.svg"), classes [ "w-full", "h-auto", "absolute" ], getColorFilter 1 ] []
        , img [ src ("static/chars/" ++ String.fromChar char ++ "/part_3.svg"), classes [ "w-full", "h-auto", "absolute" ], getColorFilter 2 ] []
        , img [ src ("static/chars/" ++ String.fromChar char ++ "/part_4.svg"), classes [ "w-full", "h-auto", "absolute" ], getColorFilter 3 ] []
        , img [ src ("static/chars/" ++ String.fromChar char ++ "/part_5.svg"), classes [ "w-full", "h-auto", "absolute" ], getColorFilter 4 ] []
        ]


decompositionCodes : String -> Html Msg
decompositionCodes parts =
    let
        chars =
            String.split "" parts
    in
    figcaption
        [ classes [ "flex", "justify-center", "items-center" ] ]
        (List.indexedMap (\i s -> div [ classes [ "text-2xl", "mt-4", "mb-12", "mx-2" ], style "color" (getColor i) ] [ text s ]) chars)


getColor : Int -> String
getColor i =
    colors |> Array.fromList |> Array.get i |> Maybe.withDefault blue


colors : List String
colors =
    [ blue
    , red
    , green2
    , orange
    , green1
    ]


getColorFilter : Int -> Attribute msg
getColorFilter i =
    colorFilters |> Array.fromList |> Array.get i |> Maybe.withDefault blueFilterStyle


colorFilters : List (Attribute msg)
colorFilters =
    [ blueFilterStyle
    , redFilterStyle
    , green2FilterStyle
    , orangeFilterStyle
    , green1FilterStyle
    ]


classes : List String -> Attribute msg
classes xs =
    List.map (\x -> ( x, True )) xs
        |> classList


encode : Storage -> E.Value
encode storage =
    E.object
        [ ( "inputHistory", E.list E.string storage.inputHistory )
        ]


decoder : D.Decoder DecodedFlags
decoder =
    D.map2 DecodedFlags
        (D.field "inputHistory" (D.list D.string))
        (D.field "quickMapping" (D.dict D.string))


encodeQuickMapping : QuickMapping -> E.Value
encodeQuickMapping quickMapping =
    E.dict identity E.string quickMapping
