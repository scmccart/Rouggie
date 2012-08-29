module Display

open libtcod
open Shared

let tileToCharAndColor tile =
    match tile with 
    | Tile.Wall -> ('#', TCODColor.darkGrey)
    | Tile.Stairs(Stair.Up) -> ('>', TCODColor.lightBlue)
    | Tile.Stairs(Stair.Down) -> ('<', TCODColor.lightBlue)
    | Tile.Door(Door.Open) -> ('-', TCODColor.darkerGrey)
    | Tile.Door(Door.Closed) -> ('+', TCODColor.lightestGrey)
    | _ -> (' ', TCODColor.black)

type Window(title, width, height) =
    let mutable root : TCODConsole = null

    do
        TCODConsole.initRoot(width, height, title)
        TCODConsole.setWindowTitle(title)

        root <- TCODConsole.root
        root.setBackgroundColor(TCODColor.black)
        root.setForegroundColor(TCODColor.green)
        root.clear()
    
        TCODConsole.flush()

    member val width = width with get
    member val height = height with get

    member this.drawHero (hero:Coord) =
        match hero with
        | (x, y) -> 
            root.setChar(x, y, int '@')
            root.setCharForeground(x, y, TCODColor.azure)

        TCODConsole.flush()

    member this.drawLevel level =
        level.tiles
            |> Map.map (fun coord tile -> tileToCharAndColor tile)
            |> Map.iter (fun (x, y) (d, c) -> 
                root.setChar(x, y, int d)
                root.setCharForeground(x, y, c))
        
    member this.startFrame() =
        root.clear();

    member this.endFrame() = 
        TCODConsole.flush()

    member this.frame draw =
        this.startFrame()
        draw()
        this.endFrame()
    


