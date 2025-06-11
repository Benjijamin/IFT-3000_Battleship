namespace Battleship.Core

module Battlefield =
    open Grid
    open Ship
    open Navigation

    type Data = { Dims: Dims; Ships: Ship list }
    
    //pour grouper les morceaux d'un ship
    type ShipPieces = (Name * (Coord * int) list)

    // Initialiser une grille vide avec des Clear
    let initClearGrid (dims: Dims) : Sector Grid =
        //serparer les dimensions en hauteur et largeur
        let (hauteur, largeur) = dims
        // creer une ligne vide avec la largeur
        let ligneVide = List.init largeur (fun _ -> Clear)
        // creer une grille vide avec la hauteur
        let rec construireGrille h =
            if h <= 0 then Empty
            else Row (ligneVide, construireGrille (h - 1))
        construireGrille hauteur
        
    // Ajouter un bateau à la grille
    let addShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        // Ajouter l'index de chaque position du bateau (0 au debut et n-1 la fin)
        let coordIndexMap =
            ship.Coords
            |> List.mapi (fun i coord -> (coord, i))
            |> Map.ofList
        
        // Mettre Active et le nom du bateau dans les coordonnées du bateau
        // Appel recursif pour mettre à jour la grille ligne par ligne
        let rec updateGrid hauteur grid =
            match grid with
            | Empty -> Empty
            | Row (ligne, reste) ->
                // pour chaque ligne, recherche les coordonnées du bateau et met a jour
                let newLigne = List.mapi (fun largeur sector ->
                    match Map.tryFind (hauteur, largeur) coordIndexMap with
                    | Some i -> Active (ship.Name, i)
                    | None -> sector ) ligne
                Row (newLigne, updateGrid (hauteur + 1) reste)

        // Commencer a la ligne 0 et mettre à jour la grille
        updateGrid 0 grid


    // Remplacer un bateau dans la grille
    let replaceShip (ship: Ship) (grid: Sector Grid) : Sector Grid =
        // Supprimer le bateau de la grille (bateau unique)
        let rec removeShip grid =
            match grid with
            | Empty -> Empty
            | Row (ligne, reste) ->
                let newLigne = List.map (fun sector ->
                    match sector with
                    | Active (name, _) when name = ship.Name -> Clear
                    | _ -> sector) ligne
                Row (newLigne, removeShip reste)

        // Ajouter le bateau à la grille avec les nouvelles coordonnées
        let cleanedGrid = removeShip grid
        addShip ship cleanedGrid
        

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
                | Active(name, i)::t -> loopX t (l@[((y, x), Active(name, i))]) (x+1) y
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
            | (0,-1) -> West
            | (-1,0) -> North
            | (0, 1) -> East
            | (1, 0) -> South
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

        { Dims = Grid.getGridDims grid; Ships = createShips grid }

    let loadData (data: Data) : Sector Grid =
        let grid = initClearGrid data.Dims
        let rec addShips s g =
            match s with
            | [] -> g
            | ship::t -> addShips t (addShip ship g)
        addShips data.Ships grid
