namespace Core.Types

open Core.Utils

type ItemsDisplacement = Map<string, float * float> 

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

    /// <summary>
    /// Возвращает истину, если дугу следует отрисовать на двумерной диаграмме.
    /// </summary>
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
    
    member this.IsFromPlace = match this with From _ -> true | _ -> false

    member this.IsToPlace = match this with To _ -> true | _ -> false

    /// <summary>
    /// Возвращает суммарную дугу.
    /// </summary>
    static member Sum (a: Arc) (b: Arc) = Arc.OfValue(a.Value + b.Value)
