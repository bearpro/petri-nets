module Core.Processing

open System
open System.Collections.Generic
open Core.Types

let fire net = 
    let nodeMap = 
        net.Nodes
        |> List.map ^ fun node -> node.Name, node
        |> dict
        |> Dictionary

    let leftConnections, rightConnections = 
        net.Connections
        |> List.partition ^ function 
                          | PlaceToTransition _ -> true
                          | TransitionToPlace _ -> false 

    for lConnection in leftConnections do
        match lConnection with
        | PlaceToTransition(placeName, transitionName) -> 
            let placeA = 
                match nodeMap.[placeName] with 
                | Place p -> p
                | _ -> invalidOp $"Connection '%s{placeName}' -> '%s{transitionName}' invalid."
            let value = placeA.Value
            nodeMap.[placeName] <- Place { placeA with Value = value - 1}
            for rConnection in rightConnections do
                match rConnection with
                | TransitionToPlace (transitionName', placeName) 
                    when transitionName' = transitionName -> 
                        let placeB = 
                            match nodeMap.[placeName] with 
                            | Place p -> p
                            | _ -> invalidOp $"Connection '%s{placeName}' -> '%s{transitionName}' invalid."
                        nodeMap.[placeName] <- Place { placeB with Value = placeB.Value + 1}
                | TransitionToPlace _ -> ()
                | _ -> failwith $"Invalid behavior of %s{nameof(rightConnections)}."
                
        | _ -> failwith $"Invalid behavior of %s{nameof(leftConnections)}."
    { net with Nodes = nodeMap.Values |> List.ofSeq }


