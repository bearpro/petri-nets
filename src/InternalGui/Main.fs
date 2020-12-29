namespace InternalGui

open Core.Types
open Core.Utils

module Main =
    open Avalonia.Controls
    open Avalonia.Controls.Shapes
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    type EditMode =
    | Cursor
    | AddPlace
    | AddTransition

    type ConnectionState =
    | PlaceToTransitionBegin
    | TransitionToPlaceBegin
    
    type ItemsDisplacement = list<float * float * string>

    type State = 
        { Network: Network 
          Mode: EditMode
          ItemsDisplacement: ItemsDisplacement
          NextPlaceIndex: int
          NextTransitionIndex: int
          ConnectionState: ConnectionState option
          DraggingItem: string option }
    
    let init = 
        let places = [| { Name = "p1"; Tokens = 1 }
                        { Name = "p2"; Tokens = 0 } |]
        let transitions = [| { Name = "t1" } |]
        let arcs = array2D [ [ From 1; To 1 ] ] 
        let net = { Places = places; Transitions = transitions; Arcs = arcs }
        { Mode = Cursor
          Network = net
          NextPlaceIndex = 3
          NextTransitionIndex = 2
          ItemsDisplacement = [ (25., 25., "p1"); (125., 25., "p2"); (75., 25., "t1");  ]
          ConnectionState = Option.None
          DraggingItem = None }

    type Msg = 
    | AddNode of X: float * Y: float
    | ChangeMode of EditMode
    | ChangeDraggedItem of string option
    | ChangeDraggedItemPosition of float * float

    let update (msg: Msg) (state: State) : State =
        printfn "%A" msg
        match msg with
        | AddNode (x, y) -> 
            let newNode, pc, tc, name = 
                match state.Mode with
                | AddPlace -> 
                    let name = sprintf "p%i" state.NextPlaceIndex
                    ( Place { Tokens = 1; Name = name }, 
                      state.NextPlaceIndex + 1, 
                      state.NextTransitionIndex,
                      name )
                | AddTransition -> 
                    let name = sprintf "t%i" state.NextTransitionIndex
                    ( Transition { Name = name },
                      state.NextPlaceIndex, 
                      state.NextTransitionIndex + 1,
                      name )
                | Cursor -> failwith "Невозможно добавить узел с в этом режиме."
            { state with 
                Network = state.Network.AddNode newNode
                NextPlaceIndex = pc
                NextTransitionIndex = tc
                ItemsDisplacement = (x, y, name) :: state.ItemsDisplacement }
        | ChangeMode mode -> { state with Mode = mode }
        | ChangeDraggedItem (Some name) -> { state with DraggingItem = Some name }
        | ChangeDraggedItem None -> { state with DraggingItem = None }
        | ChangeDraggedItemPosition (x', y') when state.DraggingItem.IsSome -> 
            let displacement = 
                state.ItemsDisplacement 
                |> List.map ^ fun (x, y, name) -> 
                    if name = state.DraggingItem.Value then (x', y', name)
                                                        else (x, y, name)
            { state with ItemsDisplacement = displacement }
        | ChangeDraggedItemPosition (x, y) -> state

    let view (state: State) (dispatch) =
        let inline getPointerPosition (e: Avalonia.Input.PointerEventArgs) =
            let x = (e.GetPosition null).X
            let y = (e.GetPosition null).Y
            (x, y)
        
        let nodeView (x, y, name) =
            let node = state.Network.Node name
            let shape, x, y = 
                match node with
                | Place p ->
                    Border.create [
                        Border.borderBrush "black"
                        Border.borderThickness 1.0
                        Border.cornerRadius 12.5
                        Border.child (
                            Ellipse.create [
                                Ellipse.width 25.0
                                Ellipse.height 25.0
                                Ellipse.fill "white"
                            ] ) ] :> Avalonia.FuncUI.Types.IView,
                    (x - 12.5), (y - 12.5)
                | Transition t -> 
                    Border.create [
                         Border.borderBrush "black"
                         Border.borderThickness 1.0
                         Border.child (
                            Rectangle.create [
                                Ellipse.width 15.0
                                Ellipse.height 25.0
                                Ellipse.fill "white"
                            ] ) ] :> Avalonia.FuncUI.Types.IView,
                    (x - 7.5), (y - 12.5)
            let label = 
                TextBlock.create [
                    TextBlock.foreground "black"
                    TextBlock.text (
                        match node with
                        | Transition { Name = name } 
                        | Place { Name = name } -> name )
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                ]
            Grid.create [
                Canvas.left x
                Canvas.top y
                Grid.onPointerPressed ^ fun e ->
                    e.Handled <- true
                    if state.DraggingItem.IsNone then ChangeDraggedItem (Some name) |> dispatch
                Grid.onPointerReleased ^ fun e -> 
                    e.Handled <- true
                    ChangeDraggedItem None |> dispatch
                Grid.zIndex 1
                Grid.children [
                    shape
                    label
                ] ] :> Avalonia.FuncUI.Types.IView
        
        let arcsView net (arcs: Arc[,]) = seq {
            for t_i in 0..net.Arcs.GetLength(0)-1 do
                for p_i in 0..net.Arcs.GetLength(1)-1 do
                    let arc = net.Arcs.[t_i, p_i]
                    let pos = 
                        match arc with 
                        | NotExist -> None
                        | From _ -> 
                            let name1 = net.Places.[p_i].Name
                            let name2 = net.Transitions.[t_i].Name
                            let x1, y1, _ = state.ItemsDisplacement |> List.find ^ fun (_, _, name) -> name = name1
                            let x2, y2, _ = state.ItemsDisplacement |> List.find ^ fun (_, _, name) -> name = name2
                            Some (x1, y1, x2, y2)
                        | To _ -> 
                            let name1 = net.Transitions.[t_i].Name
                            let name2 = net.Places.[p_i].Name
                            let x1, y1, _ = state.ItemsDisplacement |> List.find ^ fun (_, _, name) -> name = name1
                            let x2, y2, _ = state.ItemsDisplacement |> List.find ^ fun (_, _, name) -> name = name2
                            Some (x1, y1, x2, y2)
                    match pos with
                    | Some (x1, y1, x2, y2) ->
                        let line = Line.create [
                            Line.startPoint (x1, y1)
                            Line.endPoint (x2, y2)
                            Line.strokeThickness 2.
                            Line.stroke "black"
                        ]
                        yield line :> Avalonia.FuncUI.Types.IView
                    | None -> ()
        }

        let arcsView = arcsView state.Network

        DockPanel.create [
            DockPanel.children [
                Grid.create [
                    DockPanel.dock Dock.Bottom
                    Grid.columnDefinitions "* * * *"
                    Grid.children [
                        Button.create [
                            Grid.column 0
                            Button.background (if state.Mode = Cursor then "blue" else "gray")
                            Button.content "Cursor"
                            Button.onClick (fun e -> dispatch ^ ChangeMode Cursor )
                        ]
                        Button.create [
                            Grid.column 1
                            Button.background (if state.Mode = EditMode.AddPlace then "blue" else "gray")
                            Button.content "Place"
                            Button.onClick (fun e -> dispatch ^ ChangeMode AddPlace )
                        ]
                        Button.create [
                            Grid.column 2
                            Button.background (if state.Mode = EditMode.AddTransition then "blue" else "gray")
                            Button.content "Transition"
                            Button.onClick (fun e -> dispatch ^ ChangeMode AddTransition )
                        ]
                        Button.create [
                            Grid.column 3
                            Button.background "gray"
                            Button.content "Connection"
                        ]
                    ]
                ]
                Canvas.create [
                    Canvas.background "white"
                    Canvas.onPointerMoved (
                        fun e -> 
                        e.Handled <- true
                        let x, y = getPointerPosition e
                        ChangeDraggedItemPosition (x, y) |> dispatch )
                    Canvas.onPointerReleased (
                        match state.Mode with
                        | AddPlace 
                        | AddTransition -> fun e -> 
                            e.Handled <- true 
                            getPointerPosition e |> AddNode |> dispatch
                        | _ -> ignore )
                    Canvas.children (
                        let nodes = List.map nodeView state.ItemsDisplacement
                        let arcs = arcsView state.Network.Arcs |> List.ofSeq
                        nodes @ arcs
                    )
                ]
            ]
        ]
