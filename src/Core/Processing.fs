module Core.Processing

open Core.Types

/// <summary>
/// Представляет разницу между количеством меток до и после выполнения шага.
/// </summary>
type private Delta = 
    { Add: int; Sub: int }
    with
    member this.Inc n = { this with Add = this.Add + n}
    member this.Dec n = { this with Sub = this.Sub + n}
    member this.Value = this.Add - this.Sub


/// <summary>
/// Выполняет шаг ("fire") сети петри, если это возможно. Возвращает сеть с обновлёнными значениями
/// количества меток в позициях. Если возможных переходов нет - возвращает None.
/// </summary>
/// <param name="net">Суть, над которой нужно совершить переход.</param>
let fire net = 
    let places = net.Places
    let deltas = Array.create places.Length { Add = 0; Sub = 0 }
    let mutable transitionFound = false
    for t_i in 0..net.Arcs.GetLength(0)-1 do
        let canFire = seq { 
            for p_i in 0..net.Arcs.GetLength(1)-1 do
                match net.Arcs.[t_i, p_i] with
                | From n -> if (places.[p_i].Tokens - deltas.[p_i].Sub) < n then yield () else ()
                | _ -> () } |> Seq.isEmpty
        if canFire then 
            transitionFound <- true
            for p_i in 0..net.Arcs.GetLength(1)-1 do
                let newDelta =
                    match net.Arcs.[t_i, p_i] with
                    | To n   -> deltas.[p_i].Inc n
                    | From n -> deltas.[p_i].Dec n
                    | None   -> deltas.[p_i]
                deltas.[p_i] <- newDelta
    if transitionFound then
        let applyDelta place (delta: Delta) = { place with Tokens = place.Tokens + delta.Value }
        let places' = Array.map2 applyDelta places deltas
        Some { net with Places = places' }
    else Option.None
