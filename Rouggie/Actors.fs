module Actors

open libtcod
open Shared
open Input

type IActor =
    abstract member GetWorldAction : unit -> Action

type HumanActor() =
    interface IActor with
        member this.GetWorldAction() =
            let rec readAction() =
                let rawKey = readKey()

                match rawKey.KeyCode with
                | TCODKeyCode.Up -> Action.Movement(Direction.Down)
                | TCODKeyCode.Down -> Action.Movement(Direction.Up)
                | TCODKeyCode.Left -> Action.Movement(Direction.Left)
                | TCODKeyCode.Right -> Action.Movement(Direction.Right)
                | TCODKeyCode.Escape -> Action.Exit
                | _ -> readAction()

            readAction()