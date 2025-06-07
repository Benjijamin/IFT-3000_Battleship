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

    let canRotate (ship: Ship) (direction: Direction) (grid: Sector Grid) : bool =
        let nomShip = ship.Name
        let boat = createShip ship.Center direction nomShip
        let rec verifListeCoordDispo listeCoord =
            match listeCoord with
            | [] -> true
            | (b,c)::reste -> (match (elementAt grid b c) with
                               | Some (Active (nom, _)) when (nom=nomShip) -> verifListeCoordDispo reste
                               | Some Clear -> verifListeCoordDispo reste
                               | _ -> false)
        verifListeCoordDispo boat.Coords

    let rotate (ship: Ship) (direction: Direction) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        { Coords = []; Center = (0, 0); Facing = North; Name = Spy }

    let canMoveForward (ship: Ship) (grid: Sector Grid) : bool =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
        false

    let moveForward (ship: Ship) : Ship =
        (* ------- À COMPLÉTER ------- *)
        (* ----- Implémentation ------ *)
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