namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let elementAt (g : 'a Grid) (x : int) (y : int) : 'a option =
        let rec loopColumn g x = 
            match g with
            | Empty -> None
            | Row(l, n) when x = 0 -> 
                let rec loopRow l j =
                    match l with
                    | [] -> None
                    | h::t when j = 0 -> Some h
                    | h::t -> loopRow t (j-1)
                loopRow l y
            | Row(l, n) -> loopColumn n (x-1)
        loopColumn g x
    
    let verifInGrid (dim_grid: Dims) (coord: Coord) : bool =
         let (a,b) = dim_grid
         let (x,y) = coord
         let bool_first =
            match (x > a-1) with
            | true -> false
            | false -> not (y > b-1)
         match bool_first with
         | true -> (x >= 0) && (y >= 0)
         | false -> false

    let getGridDims (grid: 'a Grid) : Dims =
        let rec loopX l x =
            match l with
            | [] -> x
            | h::t -> loopX t (x+1)

        let rec loopY g x y =
            match g with
            | Empty -> (x, y)
            | Row(l, Empty) -> (loopX l 0, y+1)
            | Row(l, n) -> loopY n (x) (y+1)
        loopY grid (0) (0)