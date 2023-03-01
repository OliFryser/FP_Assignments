module Dictionary

type Dict = 
    | Node of Map<char, Dict> * bool

let empty () = Node (Map.empty, false)

let rec insertYellow s = 
    function
    | Node (m, b)   when s = "" -> Node (m, true)
    
    | Node (m, b)               -> 
        match Map.tryFind s.[0] m with
        | None -> Node (Map.add s.[0] (insertYellow s.[1..] (Node (Map.empty, false))) m,  b)
        | Some d -> Node (Map.add s.[0] (insertYellow s.[1..] d) m, b)

let rec nextInsert (s : string) d i =
    match i with
    | _ when i = s.Length-1 -> d
    | i' -> 
        let s' = (string s.[i'+1] + s.[0..i'] + s.[i'+2..])
        nextInsert s' (insertYellow s' d) (i'+1)

let rec insert (s : string) d = 
    let s' = (string s.[0] + (string ('#')) + s.[1..]) //TODO: change '#' to char 0
    nextInsert s' (insertYellow s' d) 1

let step c (Node (m, _)) = 
    match Map.tryFind c m with
    | None -> None
    | Some (Node (m, b)) -> Some (b, Node (m, b))

let reverse (Node (m, _)) = 
    match Map.tryFind '#' m with
    | None -> None
    | Some (Node (m, b)) -> Some (b, Node (m, b))

let rec lookupYellow s = 
    function
    | Node (m, b) when s = "" -> b
    | Node (m, b) ->
        match Map.tryFind s.[0] m with
            | None -> false
            | Some d -> lookupYellow s.[1..] d

let lookup (s : string) d =
    let s' = (string s.[0] + (string ('#')) + s.[1..])
    lookupYellow s' d