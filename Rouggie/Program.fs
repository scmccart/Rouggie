module Program

open System
open System.IO
open Display
open Input
open Shared
open Actors

let window = new Window ("Rouggie", 80, 40)
let actor = new HumanActor() :> IActor

let rec gameloop world = 
    let draw() =
        window.drawLevel world.currentLevel
        window.drawHero world.hero

    window.frame(draw)

    let action = actor.GetWorldAction()

    let world = match action with
                | Action.Movement(dir) -> 
                    { world with hero = tryMove world.hero dir world.currentLevel }
                | Action.Exit -> { world with shouldExit = true }
                | _ -> world

    if not world.shouldExit
        then gameloop world

let map1 = [| "##############"
            ; "#>           #          ######"
            ; "#            ############    #"
            ; "#            -          +    #"
            ; "#            ############    #"
            ; "#            #          #    #"
            ; "#            #          # <  #"
            ; "##############          ######" |]

let charToTile char =
    match char with
    | '#' -> Tile.Wall
    | '>' -> Tile.Stairs(Stair.Up)
    | '<' -> Tile.Stairs(Stair.Down)
    | '-' -> Tile.Door(Door.Open)
    | '+' -> Tile.Door(Door.Closed)
    | _ -> Tile.Floor

let loadLevel mapStrs =
    {
        tiles = mapStrs 
            |> Seq.mapi (fun y s -> s |> Seq.mapi (fun x c -> ((x, y), (charToTile c))))
            |> Seq.collect (fun x -> x)
            |> Map.ofSeq
    }

let levels : Level list =
    Directory.EnumerateFiles("levels", "*.txt")
        |> Seq.map File.ReadAllLines
        |> Seq.map loadLevel
        |> List.ofSeq

let locateUpStairs level =
    level.tiles
        |> Map.toSeq 
        |> Seq.filter (fun (coords, tile) -> isUpStairs tile)
        |> Seq.map fst
        |> Seq.head

[<EntryPoint>]
let main argv = 
    let world = { 
        hero = List.head levels |> locateUpStairs;
        shouldExit = false
        maxX = window.width - 1
        maxY = window.height - 1
        levels = levels
        currentLevel = List.head levels
    }
    gameloop world
    0 // return an integer exit code
