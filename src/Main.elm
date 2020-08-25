port module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Dom exposing (focus)
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (Attribute, Html, h1, button, div, text, textarea)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class, classList, rows, placeholder)
import Debug exposing (log)
import ChineseQuickMapping exposing (chineseQuickMapping)
import Dict
import KeyToQuickUnit exposing (keyToQuickUnit)
import Maybe.Extra exposing (traverse)
import List.Extra exposing (last)
import List exposing (head)
import Task exposing (attempt)
import Url exposing (Url)
import Url.Parser as UrlParser
import Url.Parser.Query as UrlQuery
import QS

main : Program Flags Model Msg
main = Browser.application 
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view 
  , onUrlRequest = onUrlRequest
  , onUrlChange = onUrlChange
  }

type Msg = 
  Typing String
  | ClickedQuickMode
  | ClickedNonQuickMode
  | NoOp

type alias Model = 
  { key : Key
  , url : Url
  , count : Int
  , quick : Bool
  , content : String
  }

-- dummy flag
type alias Flags = Bool
 
init : Flags -> Url -> Key -> (Model, Cmd Msg)
init quick url key = 
  let
      q = getTextFromUrl url
  in
  ( { key = key
    , url = url
    , count = 0
    , quick = quick
    , content = Maybe.withDefault "速成輸入法，或稱簡易輸入法，亦作速成或簡易，為倉頡輸入法演化出來的簡化版本。" q
    }
  , focusTextarea
  )

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ = NoOp

onUrlChange : Url -> Msg
onUrlChange _ = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    ClickedQuickMode ->
      ({ model | quick = True }, Cmd.none)
    ClickedNonQuickMode ->
      ({ model | quick = False }, Cmd.none)
    Typing newContent ->
      let
        newUrl = updateQuery "q" newContent model.url
      in
      ({ model | content = newContent, url = newUrl }
      , pushUrl model.key <| Url.toString newUrl
      )
    NoOp -> (model, Cmd.none)
    

view : Model -> Document Msg
view model = 
  { title = "速成查字"
  , body = 
    [ div [classes ["container", "mx-auto", "px-4"]]
        [ div [class "row"]
          [ h1 [classes ["text-5xl", "text-center", "my-8", "sm:my-16"]] [text "速成查字"]

          ]

        , div [classes ["row"]]
          [ div [classes ["flex", "flex-row", "justify-end", "mb-4"]]
            [ div [classes ["flex-0", "w-20", "-mr-4"]] [topButton model.quick "速成" [class "rounded-l", onClick ClickedQuickMode]]
            , div [classes ["flex-0", "w-20", "-mr-3"]] [topButton (not model.quick) "倉頡" [class "rounded-r", onClick ClickedNonQuickMode]]]
          ]
          
        , div [class "row"] 
          [ div [classes ["flex", "flex-col", "sm:flex-row", "items-stretch"]]
            [ div [classes ["flex-1", "p-2", "border", "rounded-t", "sm:rounded-b"]] 
              [ textarea [Html.Attributes.id "user-input", placeholder "輸入字句", value model.content, onInput Typing, rows 8, classes ["w-full", "outline-none", "resize-none"]] []
              ]
            , div [classes ["flex-1", "p-2", "border-l", "border-r", "border-b", "sm:border-t", "rounded-b", "sm:rounded-t", "sm:ml-4", "flex", "content-start", "flex-wrap"]] (
                model.content
                  |> String.toList
                  |> List.map (
                    chineseToParts model.quick >> (\(ch, parts) -> charBox ch parts)
                  )
              )
            ]
          ]
        ]
    ]
  }

-- no `select` porting from elm core yet
port select : String -> Cmd msg

getTextFromUrl : Url -> Maybe String
getTextFromUrl url = url
  |> UrlParser.parse (UrlParser.query (UrlQuery.string "q"))
  |> Maybe.Extra.join


updateQuery : String -> String -> Url -> Url
updateQuery k v url = 
  let
    newQuery = url.query
      |> Maybe.withDefault "" 
      |> QS.parse QS.config 
      |> QS.setStr k v 
      |> QS.serialize QS.config
      |> String.dropLeft 1 -- remove excess leading "?"
  in
  { url | query = Just newQuery }

focusTextarea : Cmd Msg
focusTextarea  = Cmd.batch [attempt (\_ -> NoOp) (focus "user-input"), select "user-input"]

getKeyboardKeys : Char -> Maybe String
getKeyboardKeys c = Dict.get c chineseQuickMapping

alphabetToQuickUnit : Char -> Maybe Char
alphabetToQuickUnit a = Dict.get a keyToQuickUnit

chineseToQuickUnits : Char -> Maybe String
chineseToQuickUnits ch = ch
  |> getKeyboardKeys
  |> Maybe.andThen (\keys -> keys
    |> String.toList
    |> traverse alphabetToQuickUnit
    |> Maybe.map String.fromList
  )

chineseToParts : Bool -> Char -> (Char, String)
chineseToParts isQuick ch = 
  let
    quickUnits = chineseToQuickUnits ch
      |> Maybe.withDefault ""
    parts = if isQuick then quickUnitsToParts quickUnits else quickUnits
  in
  (ch, parts)

quickUnitsToParts : String -> String
quickUnitsToParts quickUnits = if String.length quickUnits < 2 then quickUnits else
  let
      charsToString a b = String.fromList [a,b]
      firstChar = quickUnits
        |> String.toList
        |> head
      lastChar = quickUnits
        |> String.toList
        |> last
  in
  Maybe.map2 charsToString firstChar lastChar
  |> Maybe.withDefault ""
  
topButton : Bool -> String -> List (Attribute Msg) -> Html Msg
topButton active content extraHtmlAttributes = 
  let 
    htmlAttributes = List.append 
      [classes 
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
        [ ("bg-blue-500", active)
        , ("hover:bg-blue-700", active)
        -- , ("border-blue-500", active)

        -- , ("hover:border-gray-200", not active)
        , ("text-blue-500", not active)
        , ("hover:bg-gray-200", not active)
        ]
      ]
      extraHtmlAttributes
  in
  button htmlAttributes [text content]

charBox : Char -> String -> Html Msg
charBox chineseWord parts = div [classes ["flex", "flex-col", "items-center", "mx-1", "mb-2"]]
  [ div [classes ["flex", "flex-row", "text-2xl", "leading-8"]] [text (String.fromChar chineseWord)]
  , div [classes ["flex", "flex-row", "text-xs", "text-gray-500"]] [text parts]
  ]

classes : List String -> Attribute msg
classes xs = List.map (\x -> (x, True)) xs
  |> classList