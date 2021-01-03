module InternalGui.Types

type EditMode =
| Cursor
| AddPlace
| AddTransition

type ConnectionState =
| PlaceToTransitionBegin
| TransitionToPlaceBegin

type Counters = { Places: int; Transitions: int}
    with member this.AddPlace = { this with Places = this.Places + 1}
         member this.AddTransition = { this with Transitions = this.Transitions + 1}