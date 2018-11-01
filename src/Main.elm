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
type alias Score = Int

-- UTILS

onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

-- MODEL

type alias Model =
  { nbTicks : Int,
    wordPosList : List PositionedWord,
    currentEntry : String,
    score: Score
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    initModel = 
      { nbTicks = 0,
        wordPosList = [ { word = "go", pos = 0 } ],
        currentEntry = "",
        score = 0
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

addWords : List PositionedWord -> List Word -> List PositionedWord
addWords list words = list ++ List.map (\w -> { word = w, pos = 0 }) words

checkWord : List PositionedWord -> Word -> (List PositionedWord, Bool)
checkWord lpw w =
  let
    f = \{word, pos} -> if String.toUpper word /= String.toUpper w then Just {word=word, pos=pos} else Nothing
    newLpw = List.filterMap f lpw
    hasChanged = (List.length lpw) /= List.length newLpw
  in
    (newLpw, hasChanged)

getValueAtIndex : Int -> List a -> Maybe a
getValueAtIndex ind = List.head << List.drop ind

getRandomWordsFromList : Int -> List Word -> Random.Generator (List Word)
getRandomWordsFromList n l =
  let
    randomIndexes = Random.list n (Random.int 0 (List.length l - 1))
  in
    Random.map (
            \indexes -> List.map (
                    \ind -> getValueAtIndex ind l |> Maybe.withDefault "Nothing"
                    ) indexes
            ) randomIndexes


isGameOver : List PositionedWord -> Bool
isGameOver = List.any (\{word, pos} -> pos >= nbStepsBeforeGameOver)

getLatencyAndBandwidth : Score -> (Int, Int)
getLatencyAndBandwidth s =
        if s < 25 then
                (2, 1)
        else if s < 50 then
                (1, 1)
        else if s < 75 then
                (2, 3)
        else if s < 100 then
                (1, 2)
        else if s < 125 then
                (2, 5)
        else
                (1, 3)

-- UPDATE


type Msg
  = Tick
  | ChangeEntry String
  | AddWords (List Word)


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
        (lcy, bwidth) = getLatencyAndBandwidth model.score
        cmd = if (modBy lcy model.nbTicks) == 0 then
                      Random.generate AddWords (getRandomWordsFromList bwidth allWords)
              else
                      Cmd.none
      in
        (newModel, cmd)
    AddWords words ->
        ({ model | wordPosList = addWords model.wordPosList words }, Cmd.none)
    ChangeEntry word ->
        let
          (newWordPosList, found) = checkWord model.wordPosList word
          newScore = if found then model.score + (String.length model.currentEntry) else model.score
          newCurrentEntry = if found then "" else word
        in
        ({ model | wordPosList = newWordPosList, currentEntry = newCurrentEntry, score = newScore }, Cmd.none)

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
            autofocus True
          ] []
        , span [] [ text (" Score = " ++ String.fromInt model.score) ]
      ]
