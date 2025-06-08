namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }
    
    //pour grouper les morceaux d'un ship
    type ShipPieces = (Name * (Coord * int) list)

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
        let shipsToExtract = Spy::PatrolBoat::Destroyer::Submarine::Cruiser::AircraftCarrier::[]

        //Retourne les secteurs actifs avec leurs coords
        let getActiveSectors (grid : Sector Grid) : (Coord * Sector) list = 
            let rec loopX r l x y = 
                match r with
                | [] -> l
                | Active(name, i)::t -> loopX t (l@[((x, y), Active(name, i))]) (x+1) y
                | _::t -> loopX t l (x+1) y

            let rec loopY g l y =
                match g with
                | Empty -> l
                | Row(r, n) -> loopY n (l@(loopX r [] 0 y)) (y+1)

            loopY grid [] 0
        
        //Groupe les secteurs actifs par nom de ship
        let groupByName (sectors : (Coord * Sector) list) (name : Name) : ShipPieces =
            let group = (name, [])  
            let rec loop s g =
                match s with
                | [] -> g
                | (c, Active(n, i))::t when n = name -> loop t (n, (snd g)@[(c, i)])
                | h::t -> loop t g
            loop sectors group

        //Regroupe les morceaux de chaque type de ship
        let groupShips (sectors : (Coord * Sector) list) : ShipPieces list =
            let rec loop stg l =
                match stg with
                | [] -> l
                | name::t -> loop t (l@[(groupByName sectors name)])
            loop shipsToExtract []
        
        //Trouver direction du ship
        let directionFromPieces (pieces : ShipPieces) : Direction = 
            let rec findPiece p id =
                match p with
                | (c, i)::_ when i = id -> c
                | (c, i)::t -> findPiece t id
                | [] -> (-1, -1)
    
            let head = findPiece (snd pieces) 0
            let second = findPiece (snd pieces) 1

            match ((fst head) - (fst second), (snd head) - (snd second)) with
            | (-1,0) -> West
            | (0,-1) -> North
            | (1, 0) -> East
            | (0, 1) -> South
            | _ -> South

        //Trouver centre du ship
        let findShipCenter (pieces : ShipPieces) : Coord = 
            let centerId =
                match pieces with
                | (Spy, _) -> 0
                | (PatrolBoat, _) -> 0
                | (Destroyer, _) -> 1
                | (Submarine, _) -> 1
                | (Cruiser, _) -> 1
                | (AircraftCarrier, _) -> 2
    
            let rec loop p id =
                match p with
                | [] -> (-1,-1)
                | (c, i)::_ when i = id -> c
                | _::t -> loop t id

            loop (snd pieces) centerId

        //On parcours la grille et recreate les bateaux qui ont des morceaux
        let createShips (grid : Sector Grid) : Ship list = 
            let shipPieces = groupShips (getActiveSectors (grid))
            let rec loop sp l = 
                match sp with
                | [] -> l
                | (n, [])::t -> loop t l
                | (n, p)::t -> loop t (l@[createShip (findShipCenter (n,p)) (directionFromPieces (n, p)) n]) 
            loop shipPieces []

        { Dims = getGridDims grid; Ships = createShips grid }

    let loadData (data: Data) : Sector Grid =
        let grid = initClearGrid data.Dims
        let rec addShips s g =
            match s with
            | [] -> g
            | ship::t -> addShips t (addShip ship g)
        addShips data.Ships grid
