port module Main exposing (main)

-- import ChineseQuickMapping exposing (chineseQuickMapping)
-- import Debug exposing (log)

import Browser exposing (Document)
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, load, pushUrl)
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, h1, img, text, textarea)
import Html.Attributes exposing (class, classList, href, placeholder, rows, src, style, value, attribute)
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


type alias QuickMapping =
    Dict String String


type alias Model =
    { key : Key
    , url : Url
    , count : Int
    , quick : Bool
    , content : String
    , inputHistory : InputHistory
    , quickMapping : QuickMapping
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
        [ div [ classes [ "container", "mx-auto", "px-4", "max-w-5xl", "h-screen", "flex", "flex-col", "justify-between" ] ]
            [ div []
                [ div []
                    [ h1 [ classes [ "text-5xl", "text-gray-900", "text-center", "pt-12", "pb-8", "sm:pt-24", "sm:pb-16" ] ] [ text "速成查字" ] ]
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
                                , "p-2"
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
            , div
                [ classes [ "self-end", "py-4", "flex", "flex-row", "items-center" ] ]
                [ a [ href repoHref ] [ img [ src "assets/GitHub-Mark-64px.png", classes [ "h-8" ] ] [] ] ]
            ]
        ]
    }



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
                    , "border-blue-500"

                    -- , "rounded"
                    , "py-1"
                    , "px-4"
                    , "text-white"
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
    div [ classes [ "flex", "flex-col", "items-center", "mx-1", "mb-2" ]
        , attribute "data-testid" "char-box"
        , attribute "data-box-char" (String.fromChar chineseWord) 
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
