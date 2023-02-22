module Dict

type Dict = 
    | Node of Map<char, Dict> * bool
    | Leaf of bool

let empty = Leaf false

let insert s = 
    function
    | Leaf _        when s = "" -> Leaf true
    | Node (m, b)   when s[0] > 