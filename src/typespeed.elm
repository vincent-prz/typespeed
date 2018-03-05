import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (style, type_)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, second)


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

-- UPDATE


type Msg
  = Tick
  | ChangeEntry String
  | Submit String


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
    Submit word ->
      let
        (newWordPosList, _) = checkWord model.wordPosList word
      in
        ({ model | wordPosList = newWordPosList }, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second (\_ -> Tick)


-- VIEW


view : Model -> Html Msg
view model =
  let
    wordDisplayList = List.map displayWordPos model.wordPosList
  in 
    div []
      ((List.intersperse (br [] []) wordDisplayList) ++
      [ br [] []
      , input [type_ "text", onInput ChangeEntry ] []
      , button [ onClick (Submit model.currentEntry) ] [ text "Submit" ]
      ])
