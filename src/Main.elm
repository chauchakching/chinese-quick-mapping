port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, pushUrl)
import ChineseQuickMapping exposing (chineseQuickMapping)
import Debug exposing (log)
import Dict
import Html exposing (Attribute, Html, button, div, h1, text, textarea)
import Html.Attributes exposing (class, classList, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import KeyToQuickUnit exposing (keyToQuickUnit)
import List exposing (head)
import List.Extra exposing (last)
import Maybe.Extra exposing (traverse)
import QS
import Task exposing (attempt)
import Url exposing (Url)
import Url.Parser as UrlParser
import Url.Parser.Query as UrlQuery


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = updateWithStorage
        , view = view
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type Msg
    = Typing String
    | Clear
    | ClickedQuickMode
    | ClickedNonQuickMode
    | NoOp


type alias Model =
    { key : Key
    , url : Url
    , count : Int
    , quick : Bool
    , content : String
    , inputHistory : InputHistory
    }



-- dummy flag


type alias Flags =
    E.Value


type alias InputHistory =
    List String


init : E.Value -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        q =
            getTextFromUrl url

        inputHistory =
            case D.decodeValue decoder flags of
                Ok x ->
                    x

                Err _ ->
                    log "Error in parsing localStorage data" []
    in
    ( { key = key
      , url = url
      , count = 0
      , quick = True
      , content = Maybe.withDefault "速成輸入法，或稱簡易輸入法，亦作速成或簡易，為倉頡輸入法演化出來的簡化版本。" q
      , inputHistory = inputHistory
      }
    , focusTextarea
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    NoOp


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        NoOp ->
            ( model, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel.inputHistory), cmds ]
    )



-- update model & url query


onContentUpdated : String -> Model -> ( Model, Cmd Msg )
onContentUpdated newContent model =
    let
        newUrl =
            updateQuery "q" newContent model.url

        shouldAppendHistory =
            newContent
                /= ""
                && (last model.inputHistory
                        |> Maybe.map (String.startsWith newContent)
                        |> Maybe.withDefault False
                        |> not
                   )
                && (not <| List.member newContent model.inputHistory)

        shouldUpdateLastHistory =
            last model.inputHistory
                |> Maybe.map (\x -> String.startsWith x newContent)
                |> Maybe.withDefault False

        newInputHistory =
            List.take 10
                (if shouldAppendHistory && shouldUpdateLastHistory then
                    List.append [ newContent ] (Maybe.withDefault [] <| List.tail model.inputHistory)

                 else if shouldAppendHistory then
                    List.append [ newContent ] model.inputHistory

                 else
                    model.inputHistory
                )
    in
    ( { model | content = newContent, url = newUrl, inputHistory = newInputHistory }
    , pushUrl model.key <| Url.toString newUrl
    )


view : Model -> Document Msg
view model =
    { title = "速成查字"
    , body =
        [ div [ classes [ "container", "mx-auto", "px-4", "max-w-5xl" ] ]
            [ div [ class "row" ]
                [ h1 [ classes [ "text-5xl", "text-center", "pt-12", "pb-8", "sm:pt-24", "sm:pb-16" ] ] [ text "速成查字" ] ]
            , div [ classes [ "row" ] ]
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
            , div [ class "row" ]
                [ div [ classes [ "flex", "flex-col", "sm:flex-row", "items-stretch" ] ]
                    [ div
                        [ classes [ "flex-1", "p-2", "border", "rounded-t", "sm:rounded-b", "bg-white" ] ]
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
                            ]
                        ]
                        (model.content
                            |> String.toList
                            |> List.map
                                (chineseToParts model.quick >> (\( ch, parts ) -> charBox ch parts))
                        )
                    ]
                ]
            , div
                [ classes [ "row", "flex", "flex-row", "flex-wrap", "items-stretch" ] ]
                [ div
                    [ classes [ "flex-1" ] ]
                    (List.map (\x -> historyEntry x [ onClick <| Typing x ]) model.inputHistory)
                , div [ classes [ "sm:flex-1" ] ] []
                ]
            ]
        ]
    }



-- no `select` porting from elm core yet


port select : String -> Cmd msg


port setStorage : E.Value -> Cmd msg


getTextFromUrl : Url -> Maybe String
getTextFromUrl url =
    url
        |> UrlParser.parse (UrlParser.query (UrlQuery.string "q"))
        |> Maybe.Extra.join


updateQuery : String -> String -> Url -> Url
updateQuery k v url =
    let
        newQuery =
            url.query
                |> Maybe.withDefault ""
                |> QS.parse QS.config
                |> QS.setStr k v
                |> QS.serialize QS.config
                |> String.dropLeft 1

        -- remove excess leading "?"
    in
    { url | query = Just newQuery }


focusTextarea : Cmd Msg
focusTextarea =
    Cmd.batch [ attempt (\_ -> NoOp) (focus "user-input"), select "user-input" ]


getKeyboardKeys : Char -> Maybe String
getKeyboardKeys c =
    Dict.get c chineseQuickMapping


alphabetToQuickUnit : Char -> Maybe Char
alphabetToQuickUnit a =
    Dict.get a keyToQuickUnit


chineseToQuickUnits : Char -> Maybe String
chineseToQuickUnits ch =
    ch
        |> getKeyboardKeys
        |> Maybe.andThen
            (\keys ->
                keys
                    |> String.toList
                    |> traverse alphabetToQuickUnit
                    |> Maybe.map String.fromList
            )


chineseToParts : Bool -> Char -> ( Char, String )
chineseToParts isQuick ch =
    let
        quickUnits =
            chineseToQuickUnits ch
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
    div [ classes [ "flex", "flex-col", "items-center", "mx-1", "mb-2" ] ]
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
            ]
            extraAttributes
        )
        [ text str ]


classes : List String -> Attribute msg
classes xs =
    List.map (\x -> ( x, True )) xs
        |> classList


encode : InputHistory -> E.Value
encode =
    E.list E.string


decoder : D.Decoder InputHistory
decoder =
    D.list D.string
