import Dom
import Html exposing (Html, br, button, div, input, span, text)
import Html.Attributes exposing (autofocus, id, style, type_)
import Html.Events exposing (keyCode, on, onBlur, onClick, onInput)
import Json.Decode as Json
import Task
import Time exposing (Time, second)


nbStepsBeforeGameOver = 9

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
    currentEntry : String
  }


init : (Model, Cmd Msg)
init =
  let
    initModel = 
      { nbTicks = 0,
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

addWord : List PositionedWord -> Word -> List PositionedWord
addWord list w = list ++ [{ word = w, pos = 0 }]

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
  -- those two actions are for keeping the focus on the input
  -- see https://stackoverflow.com/questions/31901397/how-to-set-focus-on-an-element-in-elm
  | FocusOnInput
  | FocusResult (Result Dom.Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let
        tempList = List.map shiftWordPos model.wordPosList
        newWordPosList = if (model.nbTicks % 3) == 0 then addWord tempList "fuck" else tempList
        newModel =
          {
            model |
            nbTicks = model.nbTicks + 1,
            wordPosList = newWordPosList
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
    span [] [ text "You are too slow!" ]
  else
    let
      wordDisplayList = List.map displayWordPos model.wordPosList
    in
      div []
        ((List.intersperse (br [] []) wordDisplayList) ++
        [ br [] []
        , input [ id "input", type_ "text", onInput ChangeEntry, onKeyDown KeyDown, autofocus True, onBlur FocusOnInput ] []
        ])
