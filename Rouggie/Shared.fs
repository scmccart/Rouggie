module Shared

open System

// Basic Utility
let rec insert v i l =
    match i, l with
    | 0, xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | i, [] -> failwith "index out of range"

let rec remove i l =
    match i, l with
    | 0, x::xs -> xs
    | i, x::xs -> x::remove (i - 1) xs
    | i, [] -> failwith "index out of range"

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
    index: int;
    tiles: Map<Coord, Tile>;
}

type World = {
    hero: Coord;
    shouldExit: bool;
    maxX: int;
    maxY: int;
    currentLevel: Level;
    levels: List<Level>;
}

// Tile Utils

let isUpStairs tile =
    match tile with
    | Tile.Stairs(Stair.Up) -> true
    | _ -> false

let isDownStairs tile =
    match tile with
    | Tile.Stairs(Stair.Down) -> true
    | _ -> false

let isPassable tile =
    match tile with
    | Tile.Wall -> false
    | Tile.Door(Door.Closed) -> false
    | _ -> true

let isActionable tile =
    match tile with
    | Tile.Door(Door.Closed) -> true
    | Tile.Stairs(_) -> true
    | _ -> false

let locateUpStairs level =
    level.tiles
        |> Map.toSeq 
        |> Seq.filter (fun (coords, tile) -> isUpStairs tile)
        |> Seq.map fst
        |> Seq.head

let locateDownStairs level =
    level.tiles
        |> Map.toSeq
        |> Seq.filter (fun (coords, tile) -> isDownStairs tile)
        |> Seq.map fst
        |> Seq.head

// Actions
type Direction =
    | Up
    | Down
    | Left
    | Right

type Action =
    | Movement of Direction
    | Exit

//Movement

let add (x,y) (dir) =
    match dir with
    | Direction.Up -> (x, y + 1)
    | Direction.Down -> (x, y - 1)
    | Direction.Left -> (x - 1, y)
    | Direction.Right -> (x + 1, y)

let tryMove current direction level =
    let futurePos = add current direction
    let futureTile = level.tiles.[futurePos]

    if isPassable futureTile
        then Some futurePos
        else None

let moveToLevel world levelIndex positionLocator =
    let toLevel = world.levels.[levelIndex]
    let newHeroPos = positionLocator toLevel

    let levels = insert world.currentLevel world.currentLevel.index (remove world.currentLevel.index world.levels)

    { world with currentLevel = toLevel; hero = newHeroPos; levels = levels }

//Bump Interaction

let applyBumpAction world position =
    let tile = world.currentLevel.tiles.[position]

    match tile with
    | Tile.Door(Door.Closed) -> 
        let level = { world.currentLevel with tiles = world.currentLevel.tiles.Remove(position).Add(position, Tile.Door(Door.Open)) }
        {world with currentLevel = level}

    | Tile.Stairs(Stair.Down) ->
        let nextIndex = world.currentLevel.index + 1

        if nextIndex < world.levels.Length
            then moveToLevel world nextIndex locateUpStairs
            else world

    | Tile.Stairs(Stair.Up) ->
        let prevIndex = world.currentLevel.index - 1

        if prevIndex >= 0
            then moveToLevel world prevIndex locateDownStairs
            else world

    | _ -> world

let tryBumpAction world direction =
    let futurePos = add world.hero direction
    let futureTile = world.currentLevel.tiles.[futurePos]

    if isActionable futureTile
        then Some (applyBumpAction world futurePos)
        else None