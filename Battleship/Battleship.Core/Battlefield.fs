namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let initClearGrid (dims: Dims) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty

    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        match elementAt grid (fst coord) (snd coord) with
        | None -> None
        | Some Clear -> None
        | Some (Active (name,_)) -> Some name

    let extractData (grid: Sector Grid) : Data =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Dims = (0, 0); Ships = [] }

    let loadData (data: Data) : Sector Grid =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        Empty