module Core.Types

open Utils

type Place = { Tokens: int; Name: string }

type Transition = { Name: string }

type Node =
    | Place of Place
    | Transition of Transition
    with member this.Name = match this with Place { Name = name} | Transition {Name = name} -> name

type Arc = 
    | None
    | To of int
    | From of int
    with 
    static member IsLeft conn = match conn with From _ -> true | _ -> false


type Network =
  { Places: Place[]
    Transitions: Transition[]
    Arcs: Arc[,] }
    with member this.Place name = this.Places |> Array.find ^ fun p -> p.Name = name
