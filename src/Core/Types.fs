module Core.Types

open Utils

type Place = { Value: int; Name: string }

type Transition = { Name: string }

type Node =
    | Place of Place
    | Transition of Transition
    with member this.Name = match this with Place { Name = name} | Transition {Name = name} -> name

type Connection = 
    | None
    | To
    | From
    | Loop
    with 
      static member IsLeft conn = match conn with From -> true | _ -> false


type Network =
  { Places: Place[]
    Transitions: Transition[]
    Connections: Connection[,] }
    with member this.Place name = this.Places |> Array.find ^ fun p -> p.Name = name
