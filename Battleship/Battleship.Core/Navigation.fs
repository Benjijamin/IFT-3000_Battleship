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

    //À mettre dans canPlace si pas utilisée ailleurs
    let rec verifListeCoordDispo (listeCoord: Coord list) (grille: Sector Grid) (boat: Ship) : bool =
            let nomShip = boat.Name
            match listeCoord with
            | [] -> true
            | (b,c)::reste -> (match (elementAt grille b c) with
                               | Some (Active (nom, _)) when (nom= nomShip) -> verifListeCoordDispo reste grille boat
                               | Some Clear -> verifListeCoordDispo reste grille boat
                               | None -> verifListeCoordDispo reste grille boat //ignore les coord en dehors de la grille 
                               | _ -> false)
    
    // Aucune coordonnees ne peut partager une cellule avec le parametre d'un autre bateau
    let canPlaceSansPerimeter (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        // Creer le bateau
        let theShip = createShip center direction name 
        let (hauteur, largeur) = getGridDims grid
        
        // Verifier si le bateau est dans la grille
        let aInterieur (x, y) = 
            x >= 0 && y >= 0 && x < hauteur && y < largeur
        let shipDansGrille = List.forall aInterieur theShip.Coords

        // Aucune coordonnees ne peut partager une cellule avec un autre bateau + verifier si coordonnees dans la grille
        let theShipDispo = verifListeCoordDispo theShip.Coords grid theShip 

        // Retourner vrai si tout est OK
        theShipDispo && shipDansGrille
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        
        // Creer le bateau
        let theShip = createShip center direction name 
        let (hauteur, largeur) = getGridDims grid
        
        let theShipDispo = canPlaceSansPerimeter center direction name grid
       
        //Verifier le paramètre
        let theShipParamater = getPerimeter theShip (hauteur, largeur)
        let perimeterDispo = verifListeCoordDispo theShipParamater grid theShip 
        
        // Retourner vrai si tout est OK
        theShipDispo && perimeterDispo 
        

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        
        // Calculer la nouvelle position
        let (x,y) = ship.Center
        let newCenter =
            match direction with
            | North -> (x - 1, y)
            | South -> (x + 1, y)
            | East -> (x, y + 1)
            | West -> (x, y - 1)
        
        // Apeller canPlace avec la nouvelle position : center, direction, name, grid
        canPlace newCenter ship.Facing ship.Name grid 
        

    let move (ship: Ship) (direction: Direction) : Ship =
        //calculer la nouvelle position
        let (x,y) = ship.Center
        let newCenter =
            match direction with
            | North -> (x - 1, y)
            | South -> (x + 1, y)
            | East -> (x, y + 1)
            | West -> (x, y - 1)

        //Créer le nouveau bateau avec la nouvelle position

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
        let (xb,yb) =
            match ship.Facing with
            | North -> (xa-1, ya)
            | South -> (xa+1, ya)
            | East -> (xa, ya+1)
            | West -> (xa, ya-1)
        let boat = createShip (xb,yb) ship.Facing ship.Name
        canPlaceSansPerimeter boat.Center boat.Facing boat.Name grid

    let moveForward (ship: Ship) : Ship =
        let (xa, ya) = ship.Center
        let newCentre =
            match ship.Facing with
            | North -> (xa-1,ya)
            | South -> (xa+1,ya)
            | West -> (xa,ya-1)
            | East -> (xa,ya+1)
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