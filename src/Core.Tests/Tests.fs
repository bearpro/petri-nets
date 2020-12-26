module Tests

open System
open Xunit
open Core.Types
open Core.Utils

[<Fact>]
let ``Network of 2 places fires correctly`` () =
    let nodes = [ Place { Name = "p1"; Value = 1}
                  Transition { Name = "t1" }
                  Place { Name = "p2"; Value = 0} ]
    let connection = [ PlaceToTransition("p1", "t1")
                       TransitionToPlace("t1", "p2") ]
    let net = { Nodes = nodes; Connections = connection }
    
    let net' = Core.Processing.fire net;
    let p1' = net'.Place("p1")
    let p2' = net'.Place("p2")
    
    Assert.Equal(0, p1'.Value)
    Assert.Equal(1, p2'.Value)
    Assert.True(true)

[<Fact>]
let ``Network of 3 places fires correctly`` () =
    let nodes = [ Place { Name = "p1"; Value = 1 }
                  Place { Name = "p2"; Value = 0 }
                  Place { Name = "p3"; Value = 0 } 
                  Transition { Name = "t1" }
                  Transition { Name = "t2" } ]
    let connection = [ PlaceToTransition("p1", "t1")
                       TransitionToPlace("t1", "p2")
                       PlaceToTransition("p2", "t2")
                       TransitionToPlace("t2", "p3")
                        ]
    let net = { Nodes = nodes; Connections = connection }
    
    
    let net' = Core.Processing.fire net;
    let values = 
        [ "p1"; "p2"; "p3" ] 
        |> List.map ^ fun name -> (net'.Place name).Value 
        |> Seq.ofList
    Assert.Equal([0; 1; 0], values)
    
    
    let net'' = Core.Processing.fire net';
    let values = 
        [ "p1"; "p2"; "p3" ] 
        |> List.map ^ fun name -> (net''.Place name).Value 
        |> Seq.ofList
    Assert.Equal([0; 0; 1], values)
