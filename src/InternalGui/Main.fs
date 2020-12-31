module InternalGui.Main

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Core.Types
open Core.Utils
open Elmish
open InternalGui.Types
open InternalGui.Utils
open System.IO

type State = 
    { Network: Network 
      Mode: EditMode
      ItemsDisplacement: ItemsDisplacement
      Counters: Counters
      ConnectionState: ConnectionState option
      DraggingItem: string option
      BaseShapeRadius: float
      Window: Window
      CurrentFileName: string }

let init window = 
    let places = [| { Name = "p1"; Tokens = 50 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let arcs = array2D [ [ From 1; To 1 ] ] 
    let net = { Places = places; Transitions = transitions; Arcs = arcs }
    { Mode = Cursor
      Network = net
      Counters = { Places = 3; Transitions = 2 }
      ItemsDisplacement = Map.ofList [ "p1", (25., 25.); "p2", (125., 25.); "t1", (75., 25.) ]
      ConnectionState = None
      DraggingItem = None
      BaseShapeRadius = 40.0
      Window = window 
      CurrentFileName = "" }

type Msg = 
| AddNode of X: float * Y: float
| ChangeMode of EditMode
| ChangeDraggedItem of string option
| ChangeDraggedItemPosition of float * float
| Fire
| SelectFile
| OpenFile of string[]
| FileNameChanged of string

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
                 ItemsDisplacement = Map.add name (x, y) state.ItemsDisplacement }

let updatePosition x' y' state =
    let name = Option.get state.DraggingItem
    let displacement = state.ItemsDisplacement |> Map.change name ^ fun _ -> Some (x', y')
    { state with ItemsDisplacement = displacement }

let selectFile() = 
    let d = OpenFileDialog(AllowMultiple = false)
    let show () = d.ShowAsync(Window()) |> Async.AwaitTask
    Cmd.OfAsync.perform show () OpenFile

let openFile path = 
    let path = path |> Array.exactlyOne
    if File.Exists path then
        let content = File.ReadAllText path
        let network = QPNet.Data.parseNetwork content
        let displacement = QPNet.Data.parseDisplacement content
        Some(network, displacement)
    else None

let update (msg: Msg) (state: State) =
    printfn "%A" msg
    match msg with
    | AddNode (x, y) -> addNode x y state, Cmd.none
    | ChangeMode mode -> { state with Mode = mode }, Cmd.none
    | ChangeDraggedItem (Some name) -> { state with DraggingItem = Some name }, Cmd.none
    | ChangeDraggedItem None -> { state with DraggingItem = None }, Cmd.none
    | ChangeDraggedItemPosition (x, y) when state.DraggingItem.IsSome -> updatePosition x y state, Cmd.none
    | Fire -> { state with Network = Core.Processing.fire state.Network |> Option.get }, Cmd.none
    | SelectFile -> state, selectFile()
    | OpenFile path -> match openFile path with
                       | Some(n, d) -> { state with Network = n; ItemsDisplacement = d }, Cmd.none
                       | None -> state, Cmd.none
    | FileNameChanged path -> { state with CurrentFileName = path}, Cmd.ofMsg(OpenFile [|path|])
    | _ -> failwithf "Invalid msg %A for state %A" msg state, Cmd.none

let nodeView state dispatch = [
    let r = state.BaseShapeRadius
    for name, (x, y) in Map.toSeq state.ItemsDisplacement -> 
        let node = state.Network.Node name
        let shape, x, y = 
            match node with
            | Place p ->
                Border.create [
                    Border.borderBrush "black"
                    Border.borderThickness 1.0
                    Border.cornerRadius (r / 2.0)
                    Border.child (
                        Ellipse.create [
                            Ellipse.width r
                            Ellipse.height r
                            Ellipse.fill "white"
                        ] ) ] :> Avalonia.FuncUI.Types.IView,
                (x - r / 2.), (y - r / 2.)
            | Transition t -> 
                Border.create [
                     Border.borderBrush "black"
                     Border.borderThickness 1.0
                     Border.child (
                        Rectangle.create [
                            Ellipse.width (r / 2. + 3.)
                            Ellipse.height r
                            Ellipse.fill "white"
                        ] ) ] :> Avalonia.FuncUI.Types.IView,
                (x - (r / 2. + 3.) / 2.), (y - r / 2.)
        let label = 
            TextBlock.create [
                TextBlock.foreground "black"
                TextBlock.background "white"
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

let private arcCoordinates (arc: Arc) transitionIndex placeIndex (net: Network) (displacement: ItemsDisplacement) =
    let point = net.Transitions.[transitionIndex].Name
    let trans = net.Places.[placeIndex].Name
    if arc.Exist then 
        let order = 
            match arc with
            | From _ -> fun (a, b) -> (a, b)
            | To _ -> fun (a, b) -> (b, a)
            | NotExist -> failwith "Critical error"
        let x1, y1 = displacement.[point]
        let x2, y2 = displacement.[trans]
        Some (order ((x1, y1), (x2, y2)))
    else None

let private arcsView state _ = 
    seq { let displacement = state.ItemsDisplacement
          let net = state.Network
          let arcs = state.Network.Arcs
          for transitionIndex in 0..arcs.GetLength(0)-1 do
              for placeIndex in 0..arcs.GetLength(1)-1 do
                  let arc = arcs.[transitionIndex, placeIndex]
                  match arcCoordinates arc transitionIndex placeIndex net displacement with
                  | Some ((x1, y1),( x2, y2)) ->
                      let line = 
                          Grid.create [
                              Grid.children [
                                  Line.create [
                                      Line.startPoint (x1, y1)
                                      Line.endPoint (x2, y2)
                                      Line.strokeThickness 2.
                                      Line.stroke (if arc.IsFromPlace then "black" else "green")
                                  ]
                                  //TextBlock.create [
                                  //    TextBlock.text (string arc.Value)
                                  //    TextBlock.background "white"
                                  //]
                          ]
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

let private menuPanel state dispatch =
    StackPanel.create [
        DockPanel.dock Dock.Bottom
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children [
            TextBlock.create [
                TextBlock.text "Open file: "
            ]
            //Button.create [
            //    Button.onClick ^ fun e -> dispatch SelectFile
            //    Button.content "Open"
            //]
            TextBox.create [
                TextBox.width 200.
                TextBox.onTextChanged (FileNameChanged >> dispatch)
            ]
        ]
    ]

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
            menuPanel state dispatch
            toolPanel state dispatch
            workspace state dispatch
        ]
    ]
