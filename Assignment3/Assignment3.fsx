type BinTree<'a when 'a : comparison> =
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>;;



let fromList = List.fold (fun acc elem -> insert elem acc) Leaf lst;;
let fromList2 lst = List.foldBack insert lst Leaf
