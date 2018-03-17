import Dom
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (autofocus, id, style, type_, value)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput)
import Json.Decode as Json
import Random
import Task
import Time exposing (Time, second)

nbStepsBeforeGameOver = 12
nbPixelsPerStep = 40

allWords = [
  "Apple",
  "Animal",
  "Banana",
  "Bear",
  "Beard",
  "Cake",
  "Clone",
  "Direction",
  "Enable",
  "Fuck",
  "Go",
  "Hello",
  "Hell",
  "King",
  "Language",
  "Maybe",
  "Nothing",
  "Queen",
  "Silence",
  "Understand",
  "Zigzag"
  ]

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- TYPES

type alias Word = String
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


init : (Model, Cmd Msg)
init =
  let
    initModel = 
      { nbTicks = 0,
        wordPosList = [ { word = "Go", pos = 0 } ],
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
displayWordPos {word, pos} = span [ style [ ("padding", toString(pos * nbPixelsPerStep) ++ "px"), ("color", "white") ] ] [ text word ]

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
  -- those two actions are for keeping the focus on the input
  -- see https://stackoverflow.com/questions/31901397/how-to-set-focus-on-an-element-in-elm
  | FocusOnInput
  | FocusResult (Result Dom.Error ())


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
        cmd = if (model.nbTicks % 3) == 0 then Random.generate AddWord (getRandomValueFromList allWords) else Cmd.none
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
    FocusOnInput ->
      ( model, Dom.focus "input" |> Task.attempt FocusResult )
    FocusResult result ->
        -- handle success or failure here
        case result of
            Err (Dom.NotFound id) ->
              -- unable to find dom 'id'
              (model, Cmd.none)
            Ok () ->
              -- successfully focus the dom
              (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> Tick)


-- VIEW


view : Model -> Html Msg
view model =
  if isGameOver model.wordPosList then
    div [] [
      span [] [ text "You lose!" ]
      , br [] []
      , span [] [ text (" Score = " ++ toString model.score) ]
      , br [] []
      , span [] [ text (" Misses = " ++ toString model.nbMisses) ]
    ]
  else
    let
      wordDisplayList = List.map displayWordPos model.wordPosList
      areaWidth = toString (nbStepsBeforeGameOver * nbPixelsPerStep) ++ "px"
    in
      div [] [
        div [ style [ ("height" , "400px"), ("width" , areaWidth), ("background-color", "black") ]]
          ((List.intersperse (br [] []) wordDisplayList) ++
          [ br [] []
          ])
        , input [
            id "input",
            type_ "text",
            value model.currentEntry,
            onInput ChangeEntry,
            onKeyDown KeyDown,
            autofocus True,
            onBlur FocusOnInput
          ] []
        , span [] [ text (" Score = " ++ toString model.score) ]
        , span [] [ text (" Misses = " ++ toString model.nbMisses) ]
      ]
