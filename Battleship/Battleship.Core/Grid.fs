namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let elementAt (g : 'a Grid) (x : int) (y : int) : 'a option =
        let rec loopColumn g y = 
            match g with
            | Empty -> None
            | Row(l, n) when y = 0 -> 
                let rec loopRow l i =
                    match l with
                    | [] -> None
                    | h::t when i = 0 -> Some h
                    | h::t -> loopRow t (i-1)
                loopRow l x
            | Row(l, n) -> loopColumn n (y-1)
        loopColumn g y