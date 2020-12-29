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

/// <summary>
/// Представляет дугу между позицией и переходи, между переходом и позицией, или отсутствие дуги.
/// </summary>
type Arc = 
    | None
    | To of int
    | From of int
    with 
    
    /// <summary>
    /// Возвращает значение, представляющее текущую дугу в матрице инцедентности.
    /// </summary>
    /// <returns></returns>
    member this.Value = match this with None -> 0 | To n -> n | From n -> -n

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
        if n = 0 then None
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
    with member this.Place name = this.Places |> Array.find ^ fun p -> p.Name = name
