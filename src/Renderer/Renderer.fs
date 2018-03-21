(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Main
    Description: Electron Renderer Process - Script to executed after `index.html` is loaded.
*)

module Renderer

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Electron
open Node.Exports
open Fable.PowerPack
open Fable.Import.Browser

// open DevTools to see the message
// Menu -> View -> Toggle Developer Tools
Browser.console.log "Hi from the renderer.js" |> ignore

open Ref
open Update
open Emulator

/// Access to `Emulator` project

/// Initialization after `index.html` is loaded
let init () =

(*    let canvas = document.querySelector(".pane") :?> HTMLCanvasElement
    let z = Regex ("a(.)a")
    let matches = z.Match "bbaxa"
    let success = matches.Success
    
    let matchStr = sprintf "output is %A _ success is %A" matches success
    let ctx = canvas.getContext_2d()
    ctx.fillText(matchStr, 100., 100.)*)
    
    Ref.fontSize.addEventListener_change(fun _ ->
        let size: int =
            // TODO: error-prone, hardcoded index
            // of word "Font Size: xx" to slice
            Ref.fontSize.value.[11..]
            |> int
        Browser.console.log "Font size updated" |> ignore
        Update.fontSize size
    )
    
    // TODO: Implement actions for the buttons
    
(*    Ref.explore.addEventListener_click(fun _ ->
        Browser.console.log "Code updated"
        Update.code("mov r7, #5")
    )*)
    
(*    Ref.save.addEventListener_click(fun _ ->
        Browser.window.alert (sprintf "%s" (Ref.code ()))
        //Pass it to Emulator
    )*)
    
    Ref.run.addEventListener_click(fun _ ->
        // Get instructions
        let inputString = Ref.code ()   
        // Get the result
        let regs, fls, mms = Emulator.Group.output (inputString.ToString())
        
        Browser.console.log (sprintf "input %s" (inputString.ToString())) 
        |> ignore
        
        Browser.console.log (sprintf "flags %A" fls) 
        |> ignore
        
        Browser.console.log (sprintf "Regs %A" regs)
        |> ignore
        
        Browser.console.log (sprintf "MM %A" mms)
        |> ignore
        
        Update.memory mms
        |> ignore 
        
        regs
        |> List.map (fun (x, y) -> Update.register x (uint32 y))
        |> ignore
        
        fls
        |> List.map (fun (x, y) -> Update.flag x y)
    )
   
    // just for fun!
    (Ref.register 0).addEventListener_click(fun _ ->
        Browser.console.log "register R0 changed!" |> ignore
        //Update.register 0 (System.Random().Next 1000u)
    )
    
    (Ref.flag "N").addEventListener_click(fun _ ->
        Browser.console.log "flag N changed!" |> ignore
        Update.flag "N" true
    )

init()