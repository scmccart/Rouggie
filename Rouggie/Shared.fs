module Shared

open System

// Map
type Coord = int * int

type Door =
    | Open
    | Closed

type Stair =
    | Up
    | Down

type Tile =
    | Wall
    | Floor
    | Door of Door
    | Stairs of Stair

type Level = {
    tiles: Map<Coord, Tile>
}

type World = {
    hero: Coord
    shouldExit: bool
    maxX: int
    maxY: int
    currentLevel: Level
    levels: Level list
}

// Tile Utils

let isUpStairs tile =
    match tile with
    | Tile.Stairs(Stair.Up) -> true
    | _ -> false

let isPassable tile =
    match tile with
    | Tile.Wall -> false
    | Tile.Door(Door.Closed) -> false
    | _ -> true

// Actions
type Direction =
    | Up
    | Down
    | Left
    | Right

type Action =
    | Movement of Direction
    | Exit

let inline (+) (x,y) (dir) =
    match dir with
    | Direction.Up -> (x, y + 1)
    | Direction.Down -> (x, y - 1)
    | Direction.Left -> (x - 1, y)
    | Direction.Right -> (x + 1, y)

let tryMove current direction level =
    let futurePos = current + direction
    let futureTile = level.tiles.Item futurePos

    if isPassable futureTile
        then futurePos
        else current