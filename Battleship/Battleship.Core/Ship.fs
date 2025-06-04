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
    
    let getDimShip (ship: Ship) : int =
        match ship.Name with
            | Spy -> 2 
            | PatrolBoat -> 2
            | Destroyer -> 3 
            | Submarine -> 3
            | Cruiser -> 4
            | AircraftCarrier -> 5
    
    //À mettre dans Grid?
    // let verifInGrid (dim_grid: Dims) (coord: Coord) : bool =
    //     let (a,b) = dim_grid
    //     let (x,y) = coord
    //     let infBool first sec =
    //         match first with
    //         | 
        
        
        
    
    //Ajouter getDimShip dedans pour réduire?
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

    //Reste à retirer coords à l'extérieur de la grille
    let getPerimeter (ship: Ship) (dims: Dims) : Coord list =
        let coord_list = ship.Coords
        let getCoordFirst listeF =
            match listeF with
            | [] -> failwith "La liste est vide"
            | (a,b)::reste -> (a,b)
        let rec getCoordLast listeL =
            match listeL with
            | [] -> failwith "La liste est vide"
            | [(a,b)] -> (a,b)
            | (a,b)::reste -> getCoordLast reste
        let ((x,y), sens) =
            match ship.Facing with
            | North -> (getCoordFirst coord_list, North)
            | South -> (getCoordLast coord_list, South)
            | East -> (getCoordFirst coord_list, East)
            | West -> (getCoordLast coord_list, West)
        let dimShip = getDimShip ship
        let liste_repV = [(x-1,y); (x+dimShip,y)]
        let liste_repH = [(x,y+1); (x,y-dimShip)]
        let rec prepListeV liste_rep acc dim =
            match dim with
            | 0 -> liste_rep
            | _ -> (x+acc,y+1)::(x+acc,y-1)::(prepListeV liste_rep (acc+1) (dim-1))
        let rec prepListeH liste_rep acc dim =
            match dim with
            | 0 -> liste_rep
            | _ -> (x+1,y-acc)::(x-1,y-acc)::(prepListeH liste_rep (acc+1) (dim-1))
        match sens with
            | North | South -> prepListeV liste_repV 0 dimShip
            | East | West -> prepListeH liste_repH 0 dimShip