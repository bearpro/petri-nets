module Tests

open System
open Xunit
open System.Reflection
open System.IO
open Core.Types
open System.Collections.Generic

let loadResourceFile name = 
    let name = $"QPNet.Tests.Resources.%s{name}"
    let avaliableResources = Assembly.GetExecutingAssembly().GetManifestResourceNames();
    if not (Array.contains name avaliableResources) then 
        invalidArg (nameof name) $"No resource file with name %s{name}"
    
    let stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(name)
    use reader = new StreamReader(stream)
    reader.ReadToEnd()

[<Fact>]
let ``Basic sample parsed correctly`` () =
    let qpnetString = loadResourceFile "QPNet_Basic.xqp"
    let net = QPNet.Data.parse qpnetString
    
    let expectedPlaces = [| { Name = "p1"; Tokens = 1 }
                            { Name = "p2"; Tokens = 0 } |]
    let expectedTransitions = [| { Name = "t1" }; { Name = "t2" } |]
    let expectedArcs = array2D [ [ From 1; To 1] 
                                 [ From 1; To 1] ]

    Assert.Equal<Place>(expectedPlaces, net.Places)
    Assert.Equal<Transition>(expectedTransitions, net.Transitions)
    Assert.Equal<Arc>(Seq.cast<Arc> expectedArcs, Seq.cast<Arc> net.Arcs)

    
[<Fact>]
let ``Multi arc sample parsed correctly`` () =
    let qpnetString = loadResourceFile "QPNet_MultiArc.xqp"
    let net = QPNet.Data.parse qpnetString
    
    let expectedPlaces = [| { Name = "p1"; Tokens = 1 }
                            { Name = "p2"; Tokens = 0 } |]
    let expectedTransitions = [| { Name = "t1" } |]
    let expectedArcs = array2D [ [ From 2; To 1]  ]

    Assert.Equal<Place>(expectedPlaces, net.Places)
    Assert.Equal<Transition>(expectedTransitions, net.Transitions)
    Assert.Equal<Arc>(Seq.cast<Arc> expectedArcs, Seq.cast<Arc> net.Arcs)

