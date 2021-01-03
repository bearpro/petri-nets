module QPNet.Data

open FSharp.Data
open Core.Types
open Core.Utils

type private QPNetNetwork = XmlProvider<"""<root><objects>
<place x="125" y="225" n="0" value="5" name="p1"/>
<place x="350" y="200" n="3" value="0" name="p2"/>
<transition x="200" y="125" distr="0" priority="0" n="5" name="t1">
    <distr_options value="0"/>
</transition>
<arc-tp place="1" x="275" y="150" transition="0" distr="0" block="0" block_type="0">
    <distr_options value="1"/>
</arc-tp>
<arc-pt place="0" x="150" y="175" transition="5" distr="0" block="0" block_type="0">
    <distr_options value="1"/>
</arc-pt>
<transition x="225" y="250" distr="0" priority="0" n="1" name="t2">
    <distr_options value="0"/>
</transition>
<arc-tp place="1" x="300" y="225" transition="5" distr="0" block="0" block_type="0">
    <distr_options value="1"/>
</arc-tp>
<arc-pt place="0" x="175" y="225" transition="1" distr="0" block="0" block_type="0">
    <distr_options value="1"/>
</arc-pt>
</objects>
<format_version major="1" minor="2"/>
<global_options>
<transitions nonHolding="0"/>
</global_options>
</root>""">

let private addRoot qpnetFileContent = $"<root>%s{qpnetFileContent}</root>"

let private getCoreNetwork (root: QPNetNetwork.Root) =
    let places = 
        root.Objects.Places
        |> Array.map ^ fun x -> { Name = x.Name; Tokens = x.Value }, x.N
    let transitions = 
        root.Objects.Transitions
        |> Array.map ^ fun x -> { Name = x.Name }, x.N
    
    let arcsFrom = 
        root.Objects.ArcPts 
        |> Array.map ^ fun x -> 
            let place = places |> Array.find (fun (_, n) -> n = x.Place) |> fst
            let transition = transitions |> Array.find (fun (_, n) -> n = x.Transition) |> fst
            (place.Name, transition.Name)
    let arcsTo =
        root.Objects.ArcTps 
        |> Array.map ^ fun x -> 
            let place = places |> Array.find (fun (_, n) -> n = x.Place) |> fst
            let transition = transitions |> Array.find (fun (_, n) -> n = x.Transition) |> fst
            (transition.Name, place.Name)
    
    let arcs = Array2D.create transitions.Length places.Length Arc.NotExist
    
    for transitionIndex, (transition, _) in Array.indexed transitions do
        for placeIndex, (place, _) in Array.indexed places do
            let inline countEntries x = Array.length << Array.where ^ (=) x
            let countFrom = arcsFrom |> countEntries (place.Name, transition.Name)
            let countTo = arcsTo |> countEntries (transition.Name, place.Name)
            let arc = Arc.OfValue (countTo - countFrom)
            arcs.[transitionIndex, placeIndex] <- Arc.Sum arcs.[transitionIndex, placeIndex] arc
    { Places = Array.map fst places
      Transitions = Array.map fst transitions
      Arcs = arcs }

let private getItemsDisplacement (root: QPNetNetwork.Root) : ItemsDisplacement =
    let placesDisplacements = 
        root.Objects.Places
        |> Seq.map ^ fun place -> place.Name, (float place.X, float place.Y)

    let transitionsDisplacements =
        root.Objects.Transitions
        |> Seq.map ^ fun transition -> transition.Name, (float transition.X, float transition.Y)

    Map.ofSeq (Seq.concat [placesDisplacements; transitionsDisplacements])

/// <summary>
/// Принимает строку сериализованного содержимого файла модели QPNet (в формате XQP). 
/// Возвращает соответствующий экземпляр Network.
/// </summary>
/// <remarks> Может не поддерживать некоторые фишки QPNet </remarks>
let parseNetwork = addRoot >> QPNetNetwork.Parse >> getCoreNetwork
let parseDisplacement = addRoot >> QPNetNetwork.Parse >> getItemsDisplacement