module Core.Processing

open System
open System.Collections.Generic
open Core.Types

let fire net = 
    let places = net.Places
    let deltas = Array.create places.Length 0
    for t_i in 0..net.Connections.GetLength(0)-1 do
        let canFire = seq { 
            for p_i in 0..net.Connections.GetLength(1)-1 do
                match net.Connections.[t_i, p_i] with
                | From -> if places.[p_i].Value < 1 then yield () else ()
                | _ -> () } |> Seq.isEmpty
        if canFire then 
            for p_i in 0..net.Connections.GetLength(1)-1 do
                match net.Connections.[t_i, p_i] with
                | To -> deltas.[p_i] <- deltas.[p_i] + 1
                | From -> deltas.[p_i] <- deltas.[p_i] - 1
                | _ -> ()
    let places' = 
        Array.map2 
            (fun place delta -> { place with Value = place.Value + delta }) 
            places
            deltas
    { net with Places = places' }
