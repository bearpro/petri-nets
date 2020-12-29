namespace Gui

open Lib

module Counter =
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

    type State = 
        { Network: Network 
          Mode: EditMode
          PlacesCreated: int
          TransitionsCreated: int
          ConnectionState: ConnectionState option }
    
    let init = 
        { Mode = Cursor
          Network = 
            { Connections = []
              Nodes = [] }
          PlacesCreated = 0
          TransitionsCreated = 0
          ConnectionState = None }

    type Msg = 
    | AddNode of X: float * Y: float
    | ChangeMode of EditMode

    let update (msg: Msg) (state: State) : State =
        match msg with
        | AddNode (X, Y) -> 
            let newNode, pc, tc = 
                match state.Mode with
                | AddPlace -> Place { Value = 1
                                      Position = (X, Y)
                                      Name = sprintf "p%i" state.PlacesCreated }, 
                              state.PlacesCreated + 1, 
                              state.TransitionsCreated
                | AddTransition -> Transition { Position = (X, Y)
                                                Name = sprintf "t%i" state.TransitionsCreated },
                                   state.PlacesCreated, 
                                   state.TransitionsCreated + 1
                | Cursor -> failwith "Невозможно добавить узел с в этом режиме."
            { state with 
                Network = { state.Network with Nodes = newNode :: state.Network.Nodes }
                PlacesCreated = pc
                TransitionsCreated = tc }
        | ChangeMode mode -> { state with Mode = mode }
    
    let view (state: State) (dispatch) =
        let getClick f (e: Avalonia.Input.PointerReleasedEventArgs) =
            let x = (e.GetPosition null).X
            let y = (e.GetPosition null).Y
            f (x, y)
        let node n = 
            let shape, x, y = 
                match n with 
                | Place { Position = (x, y) } ->
                    Ellipse.create [
                        Ellipse.width 25.0
                        Ellipse.height 25.0
                        Ellipse.fill "green"
                    ] :> Avalonia.FuncUI.Types.IView,
                    (x - 12.5), (y - 12.5)
                | Transition { Position = (x, y) } -> 
                    Rectangle.create [
                        Ellipse.width 15.0
                        Ellipse.height 25.0
                        Ellipse.fill "white"
                    ] :> Avalonia.FuncUI.Types.IView,
                    (x - 7.5), (y - 12.5)
            let label = 
                TextBlock.create [
                    TextBlock.foreground (
                        match n with 
                        | Transition _ -> "black"
                        | Place _ -> "white" )
                    TextBlock.text (
                        match n with
                        | Transition { Name = name } 
                        | Place { Name = name } -> name )
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                ]
            Grid.create [
                Canvas.left x
                Canvas.top y
                //Grid.onPointerReleased
                Grid.children [
                    shape
                    label
                ] 
            ] :> Avalonia.FuncUI.Types.IView
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
                    Canvas.background "black"
                    Canvas.onPointerReleased (
                        match state.Mode with
                        | AddPlace | AddTransition -> getClick ^ AddNode
                        | _ -> ignore )
                    Canvas.children ^ List.map node state.Network.Nodes 
                ]
            ]
        ]
