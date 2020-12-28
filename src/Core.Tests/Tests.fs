module Tests

open System
open Xunit
open Core.Types
open Core.Utils

[<Fact>]
let ``Network of 2 places fires correctly`` () =
    let places = [| { Name = "p1"; Value = 1 }
                    { Name = "p2"; Value = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let incidents = array2D [ [ From; To ] ] 
    let net = { Places = places; Transitions = transitions; Connections = incidents }
    
    let net' = Core.Processing.fire net;
    let p1' = net'.Place("p1")
    let p2' = net'.Place("p2")
    
    Assert.Equal(0, p1'.Value)
    Assert.Equal(1, p2'.Value)

[<Fact>]
let ``Network of 3 places fires correctly`` () =
    let places = [| { Name = "p1"; Value = 1 }
                    { Name = "p2"; Value = 0 }
                    { Name = "p3"; Value = 0 } |]
    let transitions = [| { Name = "t1" }
                         { Name = "t2" } |]
    let incidents = array2D [ [ From; To; None ]
                              [ None; From; To ] ]
    let net = { Places = places; Transitions = transitions; Connections = incidents }
    
        
    let net' = Core.Processing.fire net;
    let values = 
        net'.Places 
        |> Array.map (fun place -> place.Value)
        |> Seq.ofArray
    Assert.Equal([ 0; 1; 0 ], values)
    
    
    let net'' = Core.Processing.fire net';
    let values = 
        net''.Places 
        |> Array.map (fun place -> place.Value)
        |> Seq.ofArray
    Assert.Equal([ 0; 0; 1 ], values)

[<Fact>]
let ``Double transaction fires correctly`` () =
    let places = [| { Name = "p1"; Value = 1 }
                    { Name = "p2"; Value = 0 } |]
    let transitions = [| { Name = "t1" }
                         { Name = "t2" } |]
    let incidents = array2D [ [ From; To ]
                              [ From; To ] ] 
    let net = { Places = places; Transitions = transitions; Connections = incidents }
    
    let net' = Core.Processing.fire net;    
    let values = 
        net'.Places 
        |> Array.map (fun place -> place.Value)
        |> Seq.ofArray
    Assert.Equal([ 1; 0 ], values)
