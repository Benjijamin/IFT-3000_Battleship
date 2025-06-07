namespace Battleship.Core

module Navigation =
    open Grid
    open Ship

    type Sector = Clear | Active of Name * int

    type Rotation =
        | Clockwise
        | Counterclockwise

    let getDegrees (direction: Direction) : int =
        match direction with
        | South -> 0
        | West -> 90
        | North -> 180
        | East -> 270

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let rec verifListeCoordDispo (listeCoord: Coord list) (grille: Sector Grid) (boat: Ship)=
            let nomShip = boat.Name
            match listeCoord with
            | [] -> true
            | (b,c)::reste -> (match (elementAt grille b c) with
                               | Some (Active (nom, _)) when (nom= nomShip) -> verifListeCoordDispo reste grille boat
                               | Some Clear -> verifListeCoordDispo reste grille boat
                               | _ -> false)
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        
        false

    let move (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }
    
    //Pour fonctions rotate et move : mettre à jour la grille?
    //Comment vérifier si possible et MAJ si pas accès à la grille?
    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let nomShip = ship.Name
        let boat = createShip ship.Center direction nomShip
        verifListeCoordDispo boat.Coords grid ship

    let rotate (ship: Ship) (direction: Direction) : Ship =
        let (dim, k) = getDimKShip ship.Name
        let nbrFront = k
        let nbrBack = dim - (nbrFront+1)
        let (x,y) = ship.Center
        let rec creationListeN liste accF accB accB2 accC =
            match (accF, accB, accC) with
            | (0,_,1) -> (ship.Center)::(creationListeN liste accF accB accB2 (accC-1))
            | (0,b,0) when b <> 0 -> ((x+accB2),y)::(creationListeN liste accF (accB-1) (accB2+1) accC)
            | (_,_,1) -> ((x-accF),y)::(creationListeN liste (accF-1) accB accB2 accC)
            | (0,0,0) -> []
        let rec creationListeS liste accF accB accB2 accC =
            match (accF, accB, accC) with
            | (0,_,1) -> (ship.Center)::(creationListeS liste accF accB accB2 (accC-1))
            | (0,b,0) when b <> 0 -> ((x-accB2),y)::(creationListeS liste accF (accB-1) (accB2+1) accC)
            | (_,_,1) -> ((x+accF),y)::(creationListeS liste (accF-1) accB accB2 accC)
            | (0,0,0) -> []
        let rec creationListeW liste accF accB accB2 accC =
            match (accF, accB, accC) with
            | (0,_,1) -> (ship.Center)::(creationListeW liste accF accB accB2 (accC-1))
            | (0,b,0) when b <> 0 -> (x,(y+accB2))::(creationListeW liste accF (accB-1) (accB2+1) accC)
            | (_,_,1) -> (x,(y-accF))::(creationListeW liste (accF-1) accB accB2 accC)
            | (0,0,0) -> []
        let rec creationListeE liste accF accB accB2 accC =
            match (accF, accB, accC) with
            | (0,_,1) -> (ship.Center)::(creationListeE liste accF accB accB2 (accC-1))
            | (0,b,0) when b <> 0 -> (x,(y-accB2))::(creationListeE liste accF (accB-1) (accB2+1) accC)
            | (_,_,1) -> (x,(y+accF))::(creationListeE liste (accF-1) accB accB2 accC)
            | (0,0,0) -> []
        let listeC =
            match direction with
            | North -> creationListeN [] nbrFront nbrBack 1 1
            | South -> creationListeS [] nbrFront nbrBack 1 1
            | West -> creationListeW [] nbrFront nbrBack 1 1
            | East -> creationListeE [] nbrFront nbrBack 1 1
        { Coords = listeC; Center = (x, y); Facing = direction; Name = ship.Name }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let (xa, ya) = ship.Center
        let (xb,yb) =
            match ship.Facing with
            | North -> (xa-1, ya)
            | South -> (xa+1, ya)
            | East -> (xa, ya+1)
            | West -> (xa, ya-1)
        let boat = createShip (xb,yb) ship.Facing ship.Name
        verifListeCoordDispo boat.Coords grid ship

    let moveForward (ship: Ship) : Ship =
        // let (dim, k) = getDimKShip ship.Name
        // let nbrFront = k
        // let (xa, ya) = ship.Center
        // let rec creationListeN liste acc =
        //     match (liste, acc) with
        //     | (_, 0) -> []
        //     | ()
        // let rec creationListeS liste accF accB accB2 accC =
        //     
        // let rec creationListeW liste accF accB accB2 accC =
        //     
        // let rec creationListeE liste accF accB accB2 accC =
        //     
        // let (listeC, centre) =
        //     match direction with
        //     | North -> (creationListeN [] dim, (xa-1,ya))
        //     | South -> (creationListeS [] dim, (xa+1,ya))
        //     | West -> (creationListeW [] dim, (xa,ya-1))
        //     | East -> (creationListeE [] dim, (xa,ya+1))
        // { Coords = listeC; Center = centre; Facing = ship.Facing; Name = ship.Name }
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }
    let getNextDirection (current: Direction) (rotation: Rotation) : Direction =
        match (current, rotation) with
        | (North, Clockwise) -> East
        | (East, Clockwise) -> South
        | (South, Clockwise) -> West
        | (West, Clockwise) -> North
        | (North, Counterclockwise) -> West
        | (East, Counterclockwise) -> North
        | (South, Counterclockwise) -> East
        | (West, Counterclockwise) -> South

    let canRotateForward (ship: Ship) (rotation: Rotation) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }