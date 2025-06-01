namespace Battleship.Core

module Ship =
    open Grid

    type Name =
        | Spy
        | PatrolBoat
        | Destroyer
        | Submarine
        | Cruiser
        | AircraftCarrier

    type Direction =
        | North
        | South
        | East
        | West

    type Ship = {Coords: Coord list; Center: Coord; Facing: Direction; Name: Name}

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let (acc,k) =
            match name with
            | Spy -> (2,0) 
            | PatrolBoat -> (2,0) 
            | Destroyer -> (3,1) 
            | Submarine -> (3,1) 
            | Cruiser -> (4,1) 
            | AircraftCarrier -> (5,2) 
        
        let rec creationE (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a,b-kP)::(creationE (a,b) (accP-1,kP-1))
        let rec creationN (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a-kP,b)::(creationN (a,b) (accP-1,kP-1))
        let rec creationW (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a,b+kP)::(creationW (a,b) (accP-1,kP-1))
        let rec creationS (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a+kP,b)::(creationS (a,b) (accP-1,kP-1))
        
        let listeC =
            match facing with
            | North -> creationN (center) (acc,k)
            | South -> creationS (center) (acc,k)
            | East -> creationE (center) (acc,k)
            | West -> creationW (center) (acc,k)
        
        {Coords = listeC; Center = center; Facing = facing; Name = name}

    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        []