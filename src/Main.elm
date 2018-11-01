import Browser
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (autofocus, id, style, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput)
import Json.Decode as Json
import Random
import String
import Task
import Time exposing (every)

import Words exposing (Word, allWords)

nbStepsBeforeGameOver = 12
nbPixelsPerStep = 40

main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- TYPES

type alias Position = Int
type alias PositionedWord = {
    word: Word,
    pos: Position
  }

-- UTILS

onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

-- MODEL

type alias Model =
  { nbTicks : Int,
    wordPosList : List PositionedWord,
    currentEntry : String,
    score: Int,
    nbMisses: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    initModel = 
      { nbTicks = 0,
        wordPosList = [ { word = "go", pos = 0 } ],
        currentEntry = "",
        score = 0,
        nbMisses = 0
      }
  in
    (initModel, Cmd.none)


-- LIB

shiftWordPos : PositionedWord -> PositionedWord
shiftWordPos { word, pos } = { word = word, pos = pos + 1 }

displayWordPos : PositionedWord -> Html msg
displayWordPos {word, pos} =
  let
    pixelPadding = String.fromInt(pos * nbPixelsPerStep) ++ "px"
    color =
      if pos < nbStepsBeforeGameOver // 2
        then "green"
      else
        if pos < 3 * nbStepsBeforeGameOver // 4
          then "yellow"
        else
          "red"
  in
    span
      [ style "padding" pixelPadding
      , style "color" color
      ]
      [ text word
      ]

addWord : List PositionedWord -> Word -> List PositionedWord
addWord list w = list ++ [{ word = w, pos = 0 }]

checkWord : List PositionedWord -> Word -> (List PositionedWord, Bool)
checkWord lpw w =
  let
    f = \{word, pos} -> if String.toUpper word /= String.toUpper w then Just {word=word, pos=pos} else Nothing
    newLpw = List.filterMap f lpw
    hasChanged = (List.length lpw) /= List.length newLpw
  in
    (newLpw, hasChanged)

getRandomValueFromList : List a -> Random.Generator (Maybe a)
getRandomValueFromList l =
  let
    index = Random.int 0 (List.length l - 1)
    maybeVal = Random.map (\ind -> List.head (List.drop ind l)) index
  in
    maybeVal


isGameOver : List PositionedWord -> Bool
isGameOver = List.any (\{word, pos} -> pos >= nbStepsBeforeGameOver)

-- UPDATE


type Msg
  = Tick
  | ChangeEntry String
  | KeyDown Int
  | AddWord (Maybe Word)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let
        newWordPosList = List.map shiftWordPos model.wordPosList
        newModel =
          {
            model |
            nbTicks = model.nbTicks + 1,
            wordPosList = newWordPosList
          }
        rythm =
          if model.score < 25
            then 3
          else
            if model.score < 50
              then 2
            else
              1
        cmd = if (modBy rythm model.nbTicks) == 0 then Random.generate AddWord (getRandomValueFromList allWords) else Cmd.none
      in
        (newModel, cmd)
    AddWord maybeWord ->
      case maybeWord of
        Just w ->
          ({ model | wordPosList = addWord model.wordPosList w }, Cmd.none)
        Nothing ->
          ({ model | wordPosList = addWord model.wordPosList "Nothing" }, Cmd.none)
    ChangeEntry word ->
      ({ model | currentEntry = word }, Cmd.none)
    KeyDown key ->
      if key == 13 then
        let
          (newWordPosList, found) = checkWord model.wordPosList model.currentEntry
          newScore = if found then model.score + (String.length model.currentEntry) else model.score
          newNbMisses = if found then model.nbMisses else model.nbMisses + 1
        in
          ({ model | wordPosList = newWordPosList, currentEntry = "", score = newScore, nbMisses = newNbMisses }, Cmd.none)
      else
        (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  every 1000.0 (\_ -> Tick)


-- VIEW


view : Model -> Html Msg
view model =
  if isGameOver model.wordPosList then
    div [] [
      span [] [ text "You lose!" ]
      , br [] []
      , span [] [ text (" Score = " ++ String.fromInt model.score) ]
      , br [] []
      , span [] [ text (" Misses = " ++ String.fromInt model.nbMisses) ]
    ]
  else
    let
      wordDisplayList = List.map displayWordPos model.wordPosList
      areaWidth = String.fromInt (nbStepsBeforeGameOver * nbPixelsPerStep) ++ "px"
    in
      div [] [
        div
          [ style "height" "400px"
          , style "width" areaWidth
          , style "background-color" "black"
          ]
          ((List.intersperse (br [] []) wordDisplayList) ++
          [ br [] []
          ])
        , input [
            id "input",
            type_ "text",
            value model.currentEntry,
            onInput ChangeEntry,
            onKeyDown KeyDown,
            autofocus True
          ] []
        , span [] [ text (" Score = " ++ String.fromInt model.score) ]
        , span [] [ text (" Misses = " ++ String.fromInt model.nbMisses) ]
      ]
