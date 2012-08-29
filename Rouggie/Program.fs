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
                    match tryBumpAction world dir with
                    | Some(newWorld) -> newWorld
                    | None ->
                        match tryMove world.hero dir world.currentLevel with
                        | Some(newCoords) -> { world with hero = newCoords }
                        | None -> world
                | Action.Exit -> { world with shouldExit = true }

    if not world.shouldExit
        then gameloop world
        
let charToTile char =
    match char with
    | '#' -> Tile.Wall
    | '>' -> Tile.Stairs(Stair.Up)
    | '<' -> Tile.Stairs(Stair.Down)
    | '-' -> Tile.Door(Door.Open)
    | '+' -> Tile.Door(Door.Closed)
    | _ -> Tile.Floor

let loadLevel i mapStrs =
    {
        index = i;
        tiles = mapStrs 
            |> Seq.mapi (fun y s -> s |> Seq.mapi (fun x c -> ((x, y), (charToTile c))))
            |> Seq.collect (fun x -> x)
            |> Map.ofSeq
    }

let levels : Level list =
    Directory.EnumerateFiles("levels", "*.txt")
        |> Seq.sort
        |> Seq.map File.ReadAllLines
        |> Seq.mapi loadLevel
        |> List.ofSeq

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
