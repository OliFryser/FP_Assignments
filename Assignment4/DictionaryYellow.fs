module DictionaryYellow

type Dict = 
    | Node of Map<char, Dict> * bool

let empty () = Node (Map.empty, false)

let rec insert s = 
    function
    | Node (m, b)   when s = "" -> Node (m, true)
    
    | Node (m, b)               -> 
        match Map.tryFind s.[0] m with
        | None -> Node (Map.add s.[0] (insert s.[1..] (Node (Map.empty, false))) m,  b)
        | Some d -> Node (Map.add s.[0] (insert s.[1..] d) m, b)

let rec lookup s = 
    function
    | Node (m, b) when s = "" -> b
    | Node (m, b) ->
        match Map.tryFind s.[0] m with
            | None -> false
            | Some d -> lookup s.[1..] d

let step c (Node (m, b)) = 
    match Map.tryFind c m with
    | None -> None
    | Some (Node (m, b)) -> Some (b, Node (m, b))
