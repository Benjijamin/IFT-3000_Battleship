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
    
    let rec getGridDim (grid: Sector Grid) : Coord =
         match grid with
            | Empty -> (0, 0) 
            | Row(line, reste) ->
                let hauteur_reste, _ = getGridDims reste
                let hauteur = 1 + hauteur_reste
                let largeur = List.length line
                (hauteur, largeur)

    //À mettre dans canPlace si pas utilisée ailleurs
    let rec verifListeCoordDispo (listeCoord: Coord list) (grille: Sector Grid) (boat: Ship) : bool =
            let nomShip = boat.Name
            match listeCoord with
            | [] -> true
            | (b,c)::reste -> (match (elementAt grille b c) with
                               | Some (Active (nom, _)) when (nom= nomShip) -> verifListeCoordDispo reste grille boat
                               | Some Clear -> verifListeCoordDispo reste grille boat
                               | _ -> false)
    
    let canPlace (center: Coord) (direction: Direction) (name: Name) (grid: Sector Grid) : bool =
        
        // // Creer le bateau
        // let theShip = createShip center direction name 
        // let (theDim_hauteur, theDim_largeur) = getGridDim grid
        //
        // // Aucune coordonnees ne peut partager une cellule avec un autre bateau + verifier si coordonnees dans la grille
        // let theShipDispo = verifListeCoordDispo theShip.Coords grid theShip //bool
        //
        // // Aucune coordonnees ne peut partager une cellule avec le parametre d'un autre bateau
        // let theShipParamater = getPerimeter theShip (theDim_hauteur, theDim_largeur) //Parametre
        // let perimeterDispo = verifListeCoordDispo theShipParamater grid theShip //bool
        //
        //  // Retourner vrai si tout est OK
        // dansDim && theShipDispo && perimeterDispo
        true

    let canMove (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        



        false

    let move (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }
    
    //Pour fonctions rotate et moveforward : mettre à jour la grille?
    //Pour rotate et moveforward, comment vérifier si possible et MAJ si pas accès à la grille?
    //À optimiser, mêmes bouts de code pour canrotate/rotate et canmoveforward/moveforward
    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let boat = createShip ship.Center direction ship.Name
        canPlace boat.Center boat.Facing boat.Name grid

    let rotate (ship: Ship) (direction: Direction) : Ship =
        // let (dim, k) = getDimKShip ship.Name
        // let nbrFront = k
        // let nbrBack = dim - (nbrFront+1)
        // let (x,y) = ship.Center
        createShip ship.Center direction ship.Name
        // let rec creationListeN liste accF accB accB2 accC =
        //     match (accF, accB, accC) with
        //     | (0,_,1) -> (ship.Center)::(creationListeN liste accF accB accB2 (accC-1))
        //     | (0,b,0) when b <> 0 -> ((x+accB2),y)::(creationListeN liste accF (accB-1) (accB2+1) accC)
        //     | (_,_,1) -> ((x-accF),y)::(creationListeN liste (accF-1) accB accB2 accC)
        //     | (0,0,0) -> []
        // let rec creationListeS liste accF accB accB2 accC =
        //     match (accF, accB, accC) with
        //     | (0,_,1) -> (ship.Center)::(creationListeS liste accF accB accB2 (accC-1))
        //     | (0,b,0) when b <> 0 -> ((x-accB2),y)::(creationListeS liste accF (accB-1) (accB2+1) accC)
        //     | (_,_,1) -> ((x+accF),y)::(creationListeS liste (accF-1) accB accB2 accC)
        //     | (0,0,0) -> []
        // let rec creationListeW liste accF accB accB2 accC =
        //     match (accF, accB, accC) with
        //     | (0,_,1) -> (ship.Center)::(creationListeW liste accF accB accB2 (accC-1))
        //     | (0,b,0) when b <> 0 -> (x,(y+accB2))::(creationListeW liste accF (accB-1) (accB2+1) accC)
        //     | (_,_,1) -> (x,(y-accF))::(creationListeW liste (accF-1) accB accB2 accC)
        //     | (0,0,0) -> []
        // let rec creationListeE liste accF accB accB2 accC =
        //     match (accF, accB, accC) with
        //     | (0,_,1) -> (ship.Center)::(creationListeE liste accF accB accB2 (accC-1))
        //     | (0,b,0) when b <> 0 -> (x,(y-accB2))::(creationListeE liste accF (accB-1) (accB2+1) accC)
        //     | (_,_,1) -> (x,(y+accF))::(creationListeE liste (accF-1) accB accB2 accC)
        //     | (0,0,0) -> []
        // let listeC =
        //     match direction with
        //     | North -> creationListeN [] nbrFront nbrBack 1 1
        //     | South -> creationListeS [] nbrFront nbrBack 1 1
        //     | West -> creationListeW [] nbrFront nbrBack 1 1
        //     | East -> creationListeE [] nbrFront nbrBack 1 1
        // { Coords = listeC; Center = (x, y); Facing = direction; Name = ship.Name }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        let (xa, ya) = ship.Center
        let (xb,yb) =
            match ship.Facing with
            | North -> (xa-1, ya)
            | South -> (xa+1, ya)
            | East -> (xa, ya+1)
            | West -> (xa, ya-1)
        let boat = createShip (xb,yb) ship.Facing ship.Name
        canPlace boat.Center boat.Facing boat.Name grid

    let moveForward (ship: Ship) : Ship =
        let (xa, ya) = ship.Center
        let newCentre =
            match ship.Facing with
            | North -> (xa-1,ya)
            | South -> (xa+1,ya)
            | West -> (xa,ya-1)
            | East -> (xa,ya+1)
        createShip newCentre ship.Facing ship.Name
        
        // let (dim, k) = getDimKShip ship.Name
        // let nbrFront = k
        // let (xa, ya) = ship.Center
        // let rec creationListeN liste acc =
        //     match (liste, acc) with
        //     | (_, 0) -> liste::
        //     | ()
        // let rec creationListeS liste accF accB accB2 accC =
        //     
        // let rec creationListeW liste accF accB accB2 accC =
        //     
        // let rec creationListeE liste accF accB accB2 accC =
        //     
        // let (listeC, centre) =
        //     match direction with
        //     | North -> (creationListeN ship.Coords dim, (xa-1,ya))
        //     | South -> (creationListeS ship.Coords dim, (xa+1,ya))
        //     | West -> (creationListeW ship.Coords dim, (xa,ya-1))
        //     | East -> (creationListeE ship.Coords dim, (xa,ya+1))
        // { Coords = listeC; Center = centre; Facing = ship.Facing; Name = ship.Name }
        
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
        if canRotate ship direction grid then
            canMoveForward (rotate ship direction) grid
        else
            false

    let rotateForward (ship: Ship) (rotation: Rotation) : Ship =
        let direction = getNextDirection ship.Facing rotation
        moveForward (rotate ship direction)