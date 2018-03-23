namespace Emulator

[<AutoOpen>]
module Common =

    type DummieEmulatorType =
        | A
        | B
        
    
    let addMeBy1 x = x + 1       
    let addMoreText x = x + "Added you yo!" 