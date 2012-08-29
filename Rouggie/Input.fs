module Input

open IOMonad.IOUtils
open libtcod
open Shared

let readKey() : TCODKey =
    io { 
        return TCODConsole.waitForKeypress(true) 
    } |> runIO
    
    