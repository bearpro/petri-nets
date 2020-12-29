module InternalGui.Main

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL
open Avalonia.Layout

open Core.Types
open Core.Utils
open InternalGui.Utils
open InternalGui.Types

type State = 
    { Network: Network 
      Mode: EditMode
      ItemsDisplacement: ItemsDisplacement
      Counters: Counters
      ConnectionState: ConnectionState option
      DraggingItem: string option
      PlaceShapeRadius: float }

let init = 
    let places = [| { Name = "p1"; Tokens = 50 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let arcs = array2D [ [ From 1; To 1 ] ] 
    let net = { Places = places; Transitions = transitions; Arcs = arcs }
    { Mode = Cursor
      Network = net
      Counters = { Places = 3; Transitions = 2 }
      ItemsDisplacement = [ (25., 25., "p1"); (125., 25., "p2"); (75., 25., "t1");  ]
      ConnectionState = None
      DraggingItem = None
      PlaceShapeRadius = 25.0 }

type Msg = 
| AddNode of X: float * Y: float
| ChangeMode of EditMode
| ChangeDraggedItem of string option
| ChangeDraggedItemPosition of float * float
| Fire

let newNode mode counters = 
    match mode with
    | AddPlace      -> let name = sprintf "p%i" counters.Places
                       Place { Tokens = 1; Name = name }, counters.AddPlace, name
    | AddTransition -> let name = sprintf "t%i" counters.Transitions
                       Transition { Name = name }, counters.AddTransition, name
    | Cursor -> failwith "Невозможно добавить узел с в этом режиме."

let addNode x y state =
    let newNode, counters, name = newNode state.Mode state.Counters
    { state with Network = state.Network.AddNode newNode
                 Counters = counters
                 ItemsDisplacement = (x, y, name) :: state.ItemsDisplacement }

let updatePosition x' y' state =
    let displacement = [ 
        for (x, y, name) in state.ItemsDisplacement ->
            if name = state.DraggingItem.Value 
            then (x', y', name)
            else (x, y, name) ]
    { state with ItemsDisplacement = displacement }
 
let update (msg: Msg) (state: State) : State =
    printfn "%A" msg
    match msg with
    | AddNode (x, y) -> addNode x y state
    | ChangeMode mode -> { state with Mode = mode }
    | ChangeDraggedItem (Some name) -> { state with DraggingItem = Some name }
    | ChangeDraggedItem None -> { state with DraggingItem = None }
    | ChangeDraggedItemPosition (x, y) when state.DraggingItem.IsSome -> updatePosition x y state
    | Fire -> { state with Network = Core.Processing.fire state.Network |> Option.get }
    | _ -> failwithf "Invalid msg %A for state %A" msg state

let nodeView state dispatch = [
    for (x, y, name) in state.ItemsDisplacement -> 
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
                    | Transition t -> t.Name
                    | Place p -> $"%s{p.Name}: %i{p.Tokens}" )
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.horizontalAlignment HorizontalAlignment.Center
            ]
        Grid.create ([
            Canvas.left x
            Canvas.top y
            Grid.onPointerReleased ^ fun e -> 
                e.Handled <- true
                ChangeDraggedItem None |> dispatch
            Grid.zIndex 1
            Grid.children [
                shape
                label
        ] ] 
        |> addAttrIf state.DraggingItem.IsNone (Grid.onPointerPressed ^ fun e ->
            e.Handled <- true
            ChangeDraggedItem (Some name) |> dispatch ))
        :> Avalonia.FuncUI.Types.IView
    ]

let private arcCoordinates (arc: Arc) t_i p_i (net: Network) (displacement: ItemsDisplacement) =
    let point = net.Transitions.[t_i].Name
    let trans = net.Places.[p_i].Name
    if arc.Exist then 
        let order = 
            match arc with
            | From _ -> fun (a, b) -> (a, b)
            | To _ -> fun (a, b) -> (b, a)
            | NotExist -> failwith "Critical error"
        let x1, y1, _ = List.find (fun (_, _, name) -> name = point) displacement
        let x2, y2, _ = List.find (fun (_, _, name) -> name = trans) displacement 
        Some (order ((x1, y1), (x2, y2)))
    else None

let private arcsView state _ = 
    seq { let displacement = state.ItemsDisplacement
          let net = state.Network
          let arcs = state.Network.Arcs
          for t_i in 0..arcs.GetLength(0)-1 do
              for p_i in 0..arcs.GetLength(1)-1 do
                  let arc = arcs.[t_i, p_i]
                  match arcCoordinates arc t_i p_i net displacement with
                  | Some ((x1, y1),( x2, y2)) ->
                      let line = Line.create [
                          Line.startPoint (x1, y1)
                          Line.endPoint (x2, y2)
                          Line.strokeThickness 2.
                          Line.stroke "black"
                      ]
                      yield line :> Avalonia.FuncUI.Types.IView
                  | None -> ()
          } |> List.ofSeq

let private workspace state dispatch =
    let arcs = arcsView state dispatch
    let nodes = nodeView state dispatch
    Canvas.create ([
        Canvas.background "white"
        Canvas.onPointerReleased (
            match state.Mode with
            | AddPlace 
            | AddTransition -> fun e -> 
                e.Handled <- true 
                getPointerPosition e |> AddNode |> dispatch
            | _ -> ignore )
        Canvas.children (nodes @ arcs)
    ] |> addAttrIf state.DraggingItem.IsSome
        (Canvas.onPointerMoved ^ fun e -> 
            e.Handled <- true
            let x, y = getPointerPosition e
            ChangeDraggedItemPosition (x, y) |> dispatch ))

let private toolPanel state dispatch =
    Grid.create [
        DockPanel.dock Dock.Bottom
        Grid.columnDefinitions "* * * * Auto *"
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
                Button.isEnabled false
                Button.background "gray"
                Button.content "Connection"
            ]
            Grid.create [ Grid.column 4; Grid.width 16.0 ]
            Button.create [
                Grid.column 5
                Button.background "#994444"
                Button.content "Fire"
                Button.onClick (fun _ -> dispatch ^ Fire )
            ]
        ]
    ]

let view (state: State) (dispatch) =
    DockPanel.create [
        DockPanel.children [
            toolPanel state dispatch
            workspace state dispatch
        ]
    ]
