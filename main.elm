import Color exposing (rgb, red, blue, white, black)
import Graphics.Collage exposing (Form, collage, rect, filled, move, moveX, text, circle, segment, traced, solid, toForm, group)
import Graphics.Element exposing (Element, color, container, middle, midLeft, midRight, topRight, topLeft, bottomRight, bottomLeft, midTop, midBottom, flow, down)
import Graphics.Input exposing (clickable)
import List exposing (repeat)
import StartApp
import Html exposing (div)
import Debug
import Signal
                         
type Player = PlayerX | PlayerO

nth : List a -> Int -> Maybe a
nth list n = list |> List.drop n |> List.head

nth2 : List (List a) -> (Int, Int) -> Maybe a
nth2 llist (x,y) =
    case nth llist x of
      Nothing -> Nothing
      Just list -> nth list y
             
showPlayer : Player -> Form
showPlayer player =
    case player of
      PlayerX -> rect 40 40 |> filled red
      PlayerO -> circle 20 |> filled blue

showMaybePlayer : Maybe Player -> Form
showMaybePlayer maybePlayer =
    case maybePlayer of
      Just p -> showPlayer p
      Nothing -> rect 40 40 |> filled white

drawBlackLine p1 p2 = traced (solid black) (segment p1 p2)
                 
showGrid : Form
showGrid = [ drawBlackLine (-75, -25) (75, -25),
             drawBlackLine (-75, 25) (75, 25),
             drawBlackLine (-25, -75) (-25, 75),
             drawBlackLine (25, -75) (25, 75) ]
           |> collage 150 150
           |> toForm
                 
type alias Board = List (List (Maybe Player))
type alias Coords = (Int, Int)

clickHandler : Signal.Mailbox (Maybe Coords)
clickHandler = Signal.mailbox Nothing

showBoard : Board -> Form
showBoard board =
    let move pos coord obj = [showMaybePlayer obj] |> collage 40 40 |> container 150 150 pos |> clickable (Signal.message clickHandler.address (Just coord)) |> toForm
        coords = List.concatMap (\x -> List.map (\y-> (x,y)) [0..2]) [0..2]
        positions = [ topLeft,    midTop,    topRight
                    , midLeft,    middle,    midRight
                    , bottomLeft, midBottom, bottomRight ]
    in board |> List.concat |> List.map3 move positions coords |> group

initialBoard : Board
initialBoard = repeat 3 <| repeat 3 Nothing

winner : Board -> Maybe Player
winner board =
    case board of
      [[Just x, Just x, Just x], _, _] -> Just x
      [_, [Just x, Just x, Just x], _] -> Just x
      [_, _, [Just x, Just x, Just x]] -> Just x

      [[Just x, _, _], [Just x, _, _], [Just x, _, _]] -> Just x
      [[_, Just x, _], [_, Just x, _], [_, Just x, _]] -> Just x
      [[_, _, Just x], [_, _, Just x], [_, _, Just x]] -> Just x

      [[Just x, _, _], [_, Just x, _], [_, _, Just x]] -> Just x
      [[_, _, Just x], [_, Just x, _], [Just x, _, _]] -> Just x

      _ -> Nothing

showWinner : Maybe Player -> Form
showWinner maybePlayer =
    case maybePlayer of
      Just player -> showPlayer player
      Nothing -> rect 10 10 |> filled white

otherPlayer : Player -> Player
otherPlayer player =
    case player of
      PlayerX -> PlayerO
      PlayerO -> PlayerX

type alias Model = { board: Board, turn: Player }
type alias Action = Coords

view action model = flow down
                    [ [ showGrid, showBoard model.board ] |> collage 150 150
                    , [ model.board |> winner |> showWinner ] |> collage 40 40
                    , [ showPlayer model.turn ] |> collage 40 40 ]
                    |> Html.fromElement    

update : Action -> Model -> Model
{-update action {board, turn} =
    if action == turn then
-}
{-
update (x,y) { board, turn } =
    let cell = board |> x |> y
    in
      case (cell, turn) of
        (Nothing, turn) -> { board = board, turn = turn } --TODO
        _ -> { board = board, turn = turn }
-}
update coords { board, turn } =
    case nth2 board coords of
      Nothing -> { board = board, turn = turn }
      
                               
main = StartApp.start { model = {board = initialBoard, turn = PlayerX}, view = view, update = update }
