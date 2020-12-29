module Core.Types

open Utils

/// <summary>
/// Представляет узел сети "Позиция".
/// </summary>
type Place = { Tokens: int; Name: string }

/// <summary>
/// Представляет узел сети "Переход".
/// </summary>
type Transition = { Name: string }

type Node = Place of Place | Transition of Transition

/// <summary>
/// Представляет дугу между позицией и переходи, между переходом и позицией, или отсутствие дуги.
/// </summary>
type Arc = 
    | NotExist
    | To of int
    | From of int
    with 
    
    /// <summary>
    /// Возвращает значение, представляющее текущую дугу в матрице инцедентности.
    /// </summary>
    /// <returns></returns>
    member this.Value = match this with NotExist -> 0 | To n -> n | From n -> -n

    member this.Exist = match this with NotExist -> false | _ -> true

    /// <summary>
    /// Возвращает объект дуги, соответсвующий указанному значению в матрице инцедентности.
    /// </summary>
    /// <remarks>
    /// Отрицательные значения считаются дугами от позиции к переходу, положительные - от 
    /// перехода к позиции.
    /// </remarks>
    /// <param name="n">Значение в матрице инцедентности.</param>
    /// <returns></returns>
    static member OfValue n = 
        if n = 0 then NotExist
        else if n > 0 then To n
        else From -n
    
    /// <summary>
    /// Возвращает суммарную дугу.
    /// </summary>
    static member Sum (a: Arc) (b: Arc) = Arc.OfValue(a.Value + b.Value)

/// <summary>
/// Представляет сеть петри.
/// </summary>
type Network =
  { Places: Place[]
    Transitions: Transition[]
    Arcs: Arc[,] }
    /// <summary>
    /// Возвращает объект позиции с указанным именем.
    /// </summary>
    with 
        member this.Place name = this.Places |> Array.find ^ fun p -> p.Name = name
        member this.TryPlace name = this.Places |> Array.tryFind ^ fun p -> p.Name = name
        member this.TryTransition name = this.Transitions |> Array.tryFind ^ fun p -> p.Name = name
        member this.Node name = 
            match this.TryPlace name, this.TryTransition name with
            | Some place, Option.None -> Place place
            | None, Some transition -> Transition transition
            | Some _, Some _ ->
                 failwithf "Network contains both place and transition with label '%s'." name
            | None, None -> 
                failwithf "Network not contains place or transition with label '%s'." name
        member this.AddNode node =
            let net = 
                match node with
                | Place p -> { this with 
                                    Places = Array.append [| p |] this.Places
                                    Arcs = 
                                        let arcs = Array2D.create this.Transitions.Length 
                                                                  (this.Places.Length + 1) 
                                                                  NotExist
                                        for t_i in 0..this.Arcs.GetLength(0)-1 do
                                            for p_i in 0..this.Arcs.GetLength(1)-1 do
                                                arcs.[t_i, p_i + 1] <- this.Arcs.[t_i, p_i]
                                        arcs
                                    }
                | Transition t -> { this with Transitions = Array.append [| t |] this.Transitions
                                              Arcs = 
                                                  let arcs = Array2D.create (this.Transitions.Length + 1)
                                                                            this.Places.Length 
                                                                            NotExist
                                                  for t_i in 0..this.Arcs.GetLength(0)-1 do
                                                      for p_i in 0..this.Arcs.GetLength(1)-1 do
                                                          arcs.[t_i + 1, p_i] <- this.Arcs.[t_i, p_i]
                                                  arcs }
            net
