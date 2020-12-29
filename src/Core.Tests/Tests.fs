module Tests

open System
open Xunit
open Core.Types
open Core.Utils

let makeNet places transitions incidents = 
    { Places = places; Transitions = transitions; Arcs = incidents }

let private test (result: int list Option) net =
    let net' = Core.Processing.fire net
    if result.IsSome then
        let values = net'.Value.Places |> Array.map (fun p -> p.Tokens) |> Seq.ofArray
        Assert.Equal(result.Value, values)
    else Assert.True(net'.IsNone)
    net'

let rec private testTimes times (result: int list Option) net =
    if times = 1 then test result net
    else
        Core.Processing.fire net
        |> Option.orElseWith ^ fun () -> 
            Assert.True(false, "Network can't fire specified number of times.")
            Option.None
        |> Option.bind ^ testTimes (times - 1) result


[<Fact>]
let ``Network of 2 places fires correctly`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let incidents = array2D [ [ From 1; To 1 ] ] 
    let net = makeNet places transitions incidents
    test (Some [ 0; 1]) net

[<Fact>]
let ``Chained transitions not fires same time`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 }
                    { Name = "p3"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" }
                         { Name = "t2" } |]
    let incidents = array2D [ [ From 1; To 1; NotExist ]
                              [ NotExist; From 1; To 1 ] ]
    let net = makeNet places transitions incidents
    
    Some net
    |> Option.bind ^ test (Some [ 0; 1; 0]) 
    |> Option.bind ^ test (Some [ 0; 0; 1])
    |> ignore

[<Fact>]
let ``Parallel transaction fires correctly`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" }
                         { Name = "t2" } |]
    let incidents = array2D [ [ From 1; To 1 ]
                              [ From 1; To 1 ] ] 
    let net = makeNet places transitions incidents
    
    test (Some [0; 1]) net |> ignore

[<Fact>]
let ``Multiple arcs to transition work`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let incidents = array2D [ [ From 2; To 1 ] ] 
    let net = makeNet places transitions incidents
    test Option.None net |> ignore
    
    let places = [| { Name = "p1"; Tokens = 2 }
                    { Name = "p2"; Tokens = 0 } |]
    let net = makeNet places transitions incidents
    test (Some [ 0; 1]) net |> ignore

[<Fact>]
let ``Multiple arcs from transition work`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 } |]
    let transitions = [| { Name = "t1" } |]
    let incidents = array2D [ [ From 1; To 2 ] ] 
    let net = makeNet places transitions incidents
    
    test (Some [ 0; 2]) net |> ignore
    
[<Fact>]
let ``Simple generator works`` () =
    let places = [| { Name = "p1"; Tokens = 1 }
                    { Name = "p2"; Tokens = 0 }
                    { Name = "p3"; Tokens = 0 }|]
    let transitions = [| { Name = "t1" }
                         { Name = "t2" } |]
    let incidents = array2D [ [ From 1; To   1; To 1 ]
                              [ To   1; From 1; NotExist ] ] 
    let net = makeNet places transitions incidents
    
    testTimes 6 (Some [1; 0; 3]) net |> ignore
