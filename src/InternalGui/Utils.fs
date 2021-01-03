module InternalGui.Utils

open Avalonia.Input
open Avalonia.FuncUI.Types

let inline getPointerPosition (e: PointerEventArgs) =
    let x = (e.GetPosition null).X
    let y = (e.GetPosition null).Y
    (x, y)

let inline addAttrIf condition attr (attrs: list<IAttr<'a>>) = 
    if condition then attr :: attrs else attrs