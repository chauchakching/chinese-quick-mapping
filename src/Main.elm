module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, textarea, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (value, class, classList)
import Debug exposing (log)
import ChineseQuickMapping exposing (chineseQuickMapping)
import Dict
import String exposing (split)
import String exposing (join)
import KeyToQuickUnit exposing (keyToQuickUnit)
import Maybe.Extra exposing (traverse)
import Html exposing (Attribute)
import Html exposing (h1)
import Html.Attributes exposing (rows)
import List
import Array.Extra exposing (zip)
import List.Extra exposing (last)
import List exposing (head)

main = Browser.sandbox { init = init, update = update, view = view }

type Msg = 
  Typing String
  | ClickedQuickMode
  | ClickedNonQuickMode

type alias Model = 
  { count : Int
  , quick : Bool
  , content : String
  }
 
init : Model
init = 
  { count = 0
  , quick = True
  , content = "速成輸入法，或稱簡易輸入法，亦作速成或簡易，為倉頡輸入法演化出來的簡化版本。"
  }

update : Msg -> Model -> Model
update msg model = 
  case msg of
    ClickedQuickMode ->
      { model | quick = True }
    ClickedNonQuickMode ->
      { model | quick = False }
    Typing newContent ->
      let
        k = log "hello" "hello"
      in
      { model | content = newContent }
    

view : Model -> Html Msg
view model = 
  div [classes ["container", "mx-auto", "px-4"]]
    [ div [class "row"]
      [ h1 [classes ["text-5xl", "text-center", "my-16"]] [text "速成查字"]

      ]

    , div [classes ["row"]]
      [ div [classes ["flex", "flex-row", "justify-end", "mb-4"]]
        [ div [classes ["flex-0", "w-20", "-mr-4"]] [topButton model.quick "速成" [class "rounded-l", onClick ClickedQuickMode]]
        , div [classes ["flex-0", "w-20", "-mr-3"]] [topButton (not model.quick) "倉頡" [class "rounded-r", onClick ClickedNonQuickMode]]]
      ]
      
    , div [class "row"] 
      [ div [classes ["flex", "flex-row", "items-stretch"]]
        [ div [classes ["flex-1", "p-2", "border", "rounded"]] 
          [ textarea [value model.content, onInput Typing, rows 8, classes ["w-full", "outline-none", "resize-none"]] []
          ]
        , div [classes ["flex-1", "p-2", "border", "rounded", "ml-4", "flex", "content-start", "flex-wrap" ]] (
            model.content
              |> String.toList
              |> List.map (
                chineseToParts model.quick >> (\(ch, parts) -> charBox ch parts)
              )
          )
        ]
      ]
    ]


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