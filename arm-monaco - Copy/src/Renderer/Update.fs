(* 
    High Level Programming @ Imperial College London # Spring 2018
    Project: A user-friendly ARM emulator in F# and Web Technologies ( Github Electron & Fable Compliler )
    Contributors: Angelos Filos
    Module: Renderer.Update
    Description: Event helper functions for `HTML` elements in `index.html`.
*)

module Update

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.Browser

open Ref

let fontSize (size: int) =
    let options = createObj ["fontSize" ==> size]
    window?code?updateOptions options
    
let register (id: int) (value: uint32) =
    let el = Ref.register id
  
    el.setAttribute("style", "background: #c4e160")
    el.innerHTML <- sprintf "0x%X" value
    
let flag (id: string) (value: bool) =
    let el = Ref.flag id
    match value with
        | false ->
            el.setAttribute("style", "background: #fcfcfc")
            el.innerHTML <- sprintf "%i" 0
        | true ->
            el.setAttribute("style", "background: #e56464")
            el.innerHTML <- sprintf "%i" 1
            
let code (text: string) =
    window?code?setValue(text)
    
let memory (wl: (uint32 * uint32) list) = 
    let mem = Ref.mem
   
    let appendChild wa wv = 
        let newNode = document.createElement("tr")
        let b0 = wv &&& 255u
        let b1 = (wv &&& 65280u) >>> 8
        let b2 = (wv &&& 16711680u) >>> 16
        let b3 = (wv &&& uint32 -16777216) >>> 24
        newNode.innerHTML <- sprintf "<tr> <td> 0x%X </td> <td> 0x%X </td> <td> 0x%X </td> <td> 0x%X </td> <td> 0x%X </td> <td> 0x%X </td> <tr>" wa b3 b2 b1 b0 wv 
        mem.appendChild(newNode) 
    
    mem.innerHTML <- sprintf "<tr><th>Word Address</th><th>Byte 3</th><th>Byte 2</th><th>Byte 1</th><th>Byte 0</th><th>Word Value</th></tr>" 
    
    wl |> List.map (fun (wa, wv) -> appendChild wa wv) 

let sym (symlist: (string * uint32) list)
 = 
    let sym = Ref.sym
    
    let appendChild name v = 
        let newNode = document.createElement("tr")
        newNode.innerHTML <- sprintf "<tr> <td> %s </td> <td> 0x%X </td> <tr>" name v
        sym.appendChild(newNode) 
    
    sym.innerHTML <- sprintf "<tr><th> Name </th> <th> Value </th> </tr>"
    
    symlist |> List.map (fun (str, v) -> appendChild str v)    
    
    