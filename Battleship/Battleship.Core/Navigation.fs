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

    let getcenter (center: Coord) (facing: Direction) : Coord =
        match facing with
        | North -> (fst center - 1, snd center)
        | South -> (fst center + 1, snd center)
        | East -> (fst center, snd center + 1)
        | West -> (fst center, snd center - 1)
    

    let rec verifListeCoordDispo (listeCoord: Coord list) (grille: Sector Grid) (boat: Ship) : bool =
            let nomShip = boat.Name
            match listeCoord with
            | [] -> true
            | (b,c)::reste -> (match (elementAt grille b c) with
                               | Some (Active (nom, _)) when (nom= nomShip) -> verifListeCoordDispo reste grille boat
                               | Some Clear -> verifListeCoordDispo reste grille boat
                               | None -> verifListeCoordDispo reste grille boat 
                               | _ -> false)
    
    let canPlaceSansPerimeter (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let theShip = createShip center direction name 
        let dims = getGridDims grid  
        let shipDansGrille = List.forall (aInterieur dims) theShip.Coords
        let theShipDispo = verifListeCoordDispo theShip.Coords grid theShip 
        theShipDispo && shipDansGrille
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        let theShip = createShip center direction name 
        let (hauteur, largeur) = getGridDims grid
        let theShipDispo = canPlaceSansPerimeter center direction name grid
        let theShipParamater = getPerimeter theShip (hauteur, largeur)
        let perimeterDispo = verifListeCoordDispo theShipParamater grid theShip 
        theShipDispo && perimeterDispo 
        
    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let (x,y) = ship.Center
        let newCenter = getcenter (x, y) direction
        canPlace newCenter ship.Facing ship.Name grid 
        
    let move (ship: Ship) (direction: Direction) : Ship =
        let (x,y) = ship.Center
        let newCenter = getcenter (x, y) direction
        createShip newCenter ship.Facing ship.Name

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let boat = createShip ship.Center direction ship.Name
        canPlace boat.Center boat.Facing boat.Name grid 

    let rotate (ship: Ship) (direction: Direction) : Ship =
        createShip ship.Center direction ship.Name

    // Faire une fonction pour trouver nouveau centre (utilisé aussi pour move et moveforward)
    //Match with direction mouvement, puis match facing
    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let (xa, ya) = ship.Center
        let (xb,yb) = getcenter (xa, ya) ship.Facing
        let boat = createShip (xb,yb) ship.Facing ship.Name
        canPlaceSansPerimeter boat.Center boat.Facing boat.Name grid

    let moveForward (ship: Ship) : Ship =
        let (xa, ya) = ship.Center
        let newCentre = getcenter (xa, ya) ship.Facing
        createShip newCentre ship.Facing ship.Name
        
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
        let direction = getNextDirection ship.Facing rotation
        canMoveForward (rotate ship direction) grid

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let direction = getNextDirection ship.Facing rotation
        moveForward (rotate ship direction)