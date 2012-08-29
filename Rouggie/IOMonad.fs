namespace IOMonad

type IO<'a> = IO of 'a

type IOBuilder() =

    member this.Return value =
        IO value
                
    member this.Bind ((IO(value)), cexpr) =
        cexpr value
        
module IOUtils = 

    let io = IOBuilder()

    let runIO (IO(value)) = value