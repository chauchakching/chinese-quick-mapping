module Colors exposing (..)
import Html.Attributes exposing (style)

-- for better color accessibility
-- https://color.adobe.com/zh/create/color-accessibility
blue = "#423BE0"
orange = "#DB993D"
green1 = "#63E7F0"
green2 = "#1E9423"
red = "#E31B36"

-- try the algo on https://codepen.io/sosuke/pen/Pjoqqp
blueFilterStyle = style "filter" "invert(18%) sepia(73%) saturate(4208%) hue-rotate(242deg) brightness(91%) contrast(94%)"
orangeFilterStyle = style "filter" "invert(47%) sepia(68%) saturate(404%) hue-rotate(356deg) brightness(118%) contrast(96%)"
green1FilterStyle = style "filter" "invert(82%) sepia(85%) saturate(2170%) hue-rotate(148deg) brightness(100%) contrast(88%)"
green2FilterStyle = style "filter" "invert(43%) sepia(44%) saturate(866%) hue-rotate(73deg) brightness(94%) contrast(93%)"
redFilterStyle = style "filter" "invert(13%) sepia(79%) saturate(6156%) hue-rotate(347deg) brightness(95%) contrast(86%)"
