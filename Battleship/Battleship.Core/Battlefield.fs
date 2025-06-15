namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }
    
    type ShipPieces = (Name * (Coord * int) list)

    let initClearGrid (dims: Dims) : Sector Grid =
        let (hauteur, largeur) = dims
        let ligneVide = List.init largeur (fun _ -> Clear)
        let rec construireGrille h =
            if h <= 0 then Empty
            else Row (ligneVide, construireGrille (h - 1))
        construireGrille hauteur
        
    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        
        let coordIndexMap =  makeCoordIndexMap ship.Coords (fun i coord -> (coord, i))
        
        let updateSector sector (x, y) =
            match Map.tryFind (x, y) coordIndexMap with
            | Some i -> Active (ship.Name, i)  
            | None -> sector                    

        mapGridWithIndex updateSector grid
      

    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        let cleanedGrid =
            mapGridWithIndex (fun sector _ ->
                match sector with
                | Active (name, _) when name = ship.Name -> Clear
                | _ -> sector
            ) grid
        addShip ship cleanedGrid
        

    let getSelectedName (coord: Coord) (grid: Sector Grid) : Name option =
        match elementAt grid (fst coord) (snd coord) with
        | None -> None
        | Some Clear -> None
        | Some (Active (name,_)) -> Some name

    let extractData (grid: Sector Grid) : Data =
        let shipsToExtract = Spy::PatrolBoat::Destroyer::Submarine::Cruiser::AircraftCarrier::[]

        let getActiveSectors (grid : Sector Grid) : (Coord * Sector) list =
            let lambda sector acc coords =
                match sector with
                | Active(name, i) -> acc@[(coords, sector)]
                | Clear -> acc

            parcoursGridAvecIndex (lambda) [] grid

        let groupShips (sectors : (Coord * Sector) list) : ShipPieces list =
            //let groupByName (sectors : (Coord * Sector) list) (name : Name) : ShipPieces =
            //    let group = (name, [])  
            //    let rec loop s g =
            //        match s with
            //        | [] -> g
            //        | (c, Active(n, i))::t when n = name -> loop t (n, (snd g)@[(c, i)])
            //        | h::t -> loop t g
            //    loop sectors group
            let groupByName (sectors : (Coord * Sector) list) (name : Name) : ShipPieces =
                let filtre (targetName: Name) (coord, sector) =
                    match sector with
                    | Active(n, _) -> n = targetName
                    | _ -> false

                let resultat = filtrage filtre name sectors []
                let pieces = List.map (fun (c, Active(_,i)) -> (c, i)) resultat
                (name, pieces)

            let rec loop stg l =
                match stg with
                | [] -> l
                | name::t -> loop t (l@[(groupByName sectors name)])
            loop shipsToExtract []
        
        let directionFromPieces (pieces : ShipPieces) : Direction = 
            let rec findPiece p id =
                match p with
                | (c, i)::_ when i = id -> c
                | (c, i)::t -> findPiece t id
                | [] -> (-1, -1)
    
            let head = findPiece (snd pieces) 0
            let second = findPiece (snd pieces) 1

            match ((fst head) - (fst second), (snd head) - (snd second)) with
            | (0,-1) -> West
            | (-1,0) -> North
            | (0, 1) -> East
            | (1, 0) -> South
            | _ -> South

        let findShipCenter (pieces : ShipPieces) : Coord = 
            let centerId = snd (getDimKShip (fst pieces))
    
            let rec loop p id =
                match p with
                | [] -> (-1,-1)
                | (c, i)::_ when i = id -> c
                | _::t -> loop t id

            loop (snd pieces) centerId

        let createShips (grid : Sector Grid) : Ship list = 
            let shipPieces = groupShips (getActiveSectors (grid))
            let rec loop sp l = 
                match sp with
                | [] -> l
                | (n, [])::t -> loop t l
                | (n, p)::t -> loop t (l@[createShip (findShipCenter (n,p)) (directionFromPieces (n, p)) n]) 
            loop shipPieces []

        { Dims = Grid.getGridDims grid; Ships = createShips grid }

    let loadData (data: Data) : Sector Grid =
        let grid = initClearGrid data.Dims
        let rec addShips s g =
            match s with
            | [] -> g
            | ship::t -> addShips t (addShip ship g)
        addShips data.Ships grid
