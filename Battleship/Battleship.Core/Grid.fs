namespace Battleship.Core

module Grid =

    type Dims = int * int

    type Coord = int * int

    type 'a Grid = Empty | Row of 'a list * 'a Grid


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

    let rec getGridDims (grid: 'a Grid) : Dims =
        match grid with
        | Empty -> (0, 0)
        | Row(ligne, reste) ->
            let (hauteur_reste,_) = getGridDims reste
            let hauteur = 1 + hauteur_reste
            let largeur = List.length ligne
            (hauteur, largeur)


    let parcoursGridAvecIndex (lambda : 'a -> 'b -> Coord -> 'b) (acc: 'b) (grid: 'a Grid) : 'b =
        let rec loopColumn acc columnIndex grid =
            match grid with
            | Empty -> acc
            | Row(ligne, reste) ->
                let rec loopRow acc rowIndex liste =
                    match liste with
                    | [] -> acc
                    | h::t -> loopRow (lambda h acc (columnIndex, rowIndex)) (rowIndex + 1) t
                loopColumn (loopRow acc 0 ligne) (columnIndex + 1) reste

        loopColumn acc 0 grid
        
    let parcoursGrid (lambda : 'a -> 'b -> 'b) (acc: 'b) (grid: 'a Grid) : 'b =
        parcoursGridAvecIndex (fun a b c -> lambda a b) acc grid

        //let rec loopX r l x y = 
        //    match r with
        //    | [] -> l
        //    | Active(name, i)::t -> loopX t (l@[((y, x), Active(name, i))]) (x+1) y
        //    | _::t -> loopX t l (x+1) y

        //let rec loopY g l y =
        //    match g with
        //    | Empty -> l
        //    | Row(r, n) -> loopY n (l@(loopX r [] 0 y)) (y+1)

    // utiliser dans la fonction addShip. Juliette
    let makeCoordIndexMap (coords: 'a list)(f: int -> 'a -> 'b * int) =
        coords
        |> List.mapi f
        |> Map.ofList
