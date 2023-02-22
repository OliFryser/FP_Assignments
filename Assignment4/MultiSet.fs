module MultiSet

type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32> * uint32

let empty = M (Map.empty<'a, uint32>, 0u)

let isEmpty (M (m,l)) = 
    match l with
    | 0u -> true
    | _ -> false

let size (M (_,l)) = l

let contains a (M (m,_)) = 
    match Map.tryFind a m with
    | None -> false
    | _ -> true

let numItems a (M (m,_)) = Map.tryFind a m |> Option.defaultValue 0u

let add a n (M (m,l)) = 
    let current = numItems a (M (m,l))
    M ((Map.add a (n + current) m), l+n)

let addSingle a (M (m,l)) =
    let current = numItems a (M (m,l))
    M (Map.add a (current+1u) m, l+1u)

let remove a n (M (m,l)) =
    if (not (contains a (M (m,l)))) then (M (m,l)) else
    let current = numItems a (M (m,l))
    if (current > n) 
        then M (Map.add a (current-n) m, (l-n))
    else 
        M (Map.remove a m,(l-current))

let removeSingle a (M (m,l)) = 
    if (not (contains a (M (m,l)))) then (M (m,l)) else
    let current = numItems a (M (m,l))
    if (current-1u > 0u)
        then M (Map.add a (current-1u) m, l-1u)
    else M (Map.remove a m, l-1u)

let fold f acc (M (m,_)) =
    Map.fold f acc m

let foldBack f (M (m,_)) acc =
    Map.foldBack f m acc

let rec ofList = 
    function
    | [] -> empty
    | x::xs -> (addSingle x (ofList xs))

// ofListN is used as a helper function, since in the map function, we already know the number of items, for each item.
// We could use the ofList function instead, but this would be a lot less effective, if we have many of the same element.
let rec ofListN = 
    function
    | [] -> empty
    | (x,n)::xs -> (add x n (ofListN xs))

let rec toList (M (m,l))=
    match l with
    | 0u -> []
    | _ -> 
        let (k,_) = Map.toList m |> Seq.head
        k::toList (removeSingle k (M (m,l)))

let map f (M (m,_)) = 
    Map.toList m 
    |> List.map (fun (a,b) -> (f a, b)) 
    |> ofListN

//Not implmented
let rec union (M ((m1 : Map<'a,uint32>), l1)) (M ((m2 : Map<'a,uint32>), l2)) = 
    Map.fold (fun acc a n -> 
        if (contains a acc) 
        then 
            match numItems a acc with
            | n' when n > n' -> add a n acc 
            | _ -> acc
        else add a n acc) (M (m2,l2)) m1

let sum (M ((m1 : Map<'a,uint32>), l1)) (M ((m2 : Map<'a,uint32>), l2)) = 
    Map.fold (fun acc a n -> 
        if (contains a acc) 
        then add a n acc
        else add a n acc) (M (m2,l2)) m1

let subtract (M ((m1 : Map<'a,uint32>), l1)) (M ((m2 : Map<'a,uint32>), l2)) =
    Map.fold (fun acc a n -> 
        if (contains a acc) 
        then remove a n acc
        else acc) (M (m1,l1)) m2

let intersection (M ((m1 : Map<'a,uint32>), l1)) (M ((m2 : Map<'a,uint32>), l2)) =
    Map.fold (fun acc a n -> 
        if (contains a (M (m1, l1)))
        then 
            match numItems a (M (m1, l1)) with
            | n' when n > n' -> add a n' acc
            | _ -> add a n acc
        else acc) empty m2