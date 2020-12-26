module Core.Types

open Utils

type Place = { Value: int; Name: string }

type Transition = { Name: string }

type Node =
    | Place of Place
    | Transition of Transition
    with member this.Name = match this with Place { Name = name} | Transition {Name = name} -> name

type Connection =
    | PlaceToTransition of string * string
    | TransitionToPlace of string * string

type Network =
    { Nodes: Node list
      Connections: Connection list }
    with 
    member this.Place name = 
        this.Nodes 
        |> List.find ^ fun node -> node.Name = name
        |> function Place p -> p | _ -> failwith $"Invalid place name %s{name}"
