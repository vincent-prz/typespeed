import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Time exposing (Time, second)


nbStepsBeforeGameOver = 12

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
type alias Duration = Int
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
  { duration : Duration,
    wordPosList : List PositionedWord,
    currentEntry : String
  }


init : (Model, Cmd Msg)
init =
  let
    initModel = 
      { duration = 0,
        wordPosList = [
          { word = "Hello", pos = 0 },
          { word = "Bye", pos = 1}
        ],
        currentEntry = ""
      }
  in
    (initModel, Cmd.none)


-- LIB

shiftWordPos : PositionedWord -> PositionedWord
shiftWordPos { word, pos } = { word = word, pos = pos + 1 }

displayWordPos : PositionedWord -> Html msg
displayWordPos {word, pos} = span [ style [("padding", toString(pos * 10) ++ "px") ] ] [ text word ]


checkWord : List PositionedWord -> Word -> (List PositionedWord, Bool)
checkWord lpw w =
  let
    f = \{word, pos} -> if word /= w then Just {word=word, pos=pos} else Nothing
    newLpw = List.filterMap f lpw
    hasChanged = (List.length lpw) /= List.length newLpw
  in
    (newLpw, hasChanged)

isGameOver : List PositionedWord -> Bool
isGameOver = List.any (\{word, pos} -> pos >= nbStepsBeforeGameOver)

-- UPDATE


type Msg
  = Tick
  | ChangeEntry String
  | KeyDown Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let
        newModel =
          {
            model |
            duration = model.duration + 1,
            wordPosList = List.map shiftWordPos model.wordPosList
          }
      in
        (newModel, Cmd.none)
    ChangeEntry word ->
      ({ model | currentEntry = word }, Cmd.none)
    KeyDown key ->
      if key == 13 then
        let
          (newWordPosList, _) = checkWord model.wordPosList model.currentEntry
        in
          ({ model | wordPosList = newWordPosList }, Cmd.none)
      else
        (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> Tick)


-- VIEW


view : Model -> Html Msg
view model =
  if isGameOver model.wordPosList then
    span [] [ text "You are too slow!" ]
  else
    let
      wordDisplayList = List.map displayWordPos model.wordPosList
    in
      div []
        ((List.intersperse (br [] []) wordDisplayList) ++
        [ br [] []
        , input [ type_ "text", onInput ChangeEntry, onKeyDown KeyDown ] []
        ])
