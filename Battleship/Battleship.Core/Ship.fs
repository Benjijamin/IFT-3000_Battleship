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
    
    let getDimKShip (name: Name) : int*int =
        match name with
        | Spy -> (2,0) 
        | PatrolBoat -> (2,0) 
        | Destroyer -> (3,1) 
        | Submarine -> (3,1) 
        | Cruiser -> (4,1) 
        | AircraftCarrier -> (5,2)
     
    let aInterieur (dim_grid: Dims) (x, y): bool = 
        let (hauteur, largeur) = dim_grid
        x >= 0 && y >= 0 && x < hauteur && y < largeur
        
    let createShip (center: Coord) (facing: Direction) (name: Name) : Ship =
        let (acc,k) = getDimKShip name
        let rec creationE (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a,b+kP)::(creationE (a,b) (accP-1,kP-1))
        let rec creationN (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a-kP,b)::(creationN (a,b) (accP-1,kP-1))
        let rec creationW (a,b) (accP, kP) =
            match accP with
            | 0 -> []
            | _ -> (a,b-kP)::(creationW (a,b) (accP-1,kP-1))
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
        let coord_list = ship.Coords
        let ((x,y), sens) =
            match ship.Facing with
            | North -> (List.head coord_list, North)
            | South -> (List.last coord_list, South)
            | East -> (List.head coord_list, East)
            | West -> (List.last coord_list, West)
        let (dimShip,_) = getDimKShip ship.Name
        let liste_repV = [(x-1,y); (x+dimShip,y)]
        let liste_repH = [(x,y+1); (x,y-dimShip)]
        let rec prepListeV liste_rep acc dim =
            match (dim+2) with
            | 0 -> liste_rep
            | _ -> (x+acc,y+1)::(x+acc,y-1)::(prepListeV liste_rep (acc+1) (dim-1))
        let rec prepListeH liste_rep acc dim =
            match (dim+2) with
            | 0 -> liste_rep
            | _ -> (x+1,y-acc)::(x-1,y-acc)::(prepListeH liste_rep (acc+1) (dim-1))
        let listeA =
            match sens with
            | North | South -> prepListeV liste_repV -1 dimShip
            | East | West -> prepListeH liste_repH -1 dimShip
        filtrage aInterieur dims listeA []