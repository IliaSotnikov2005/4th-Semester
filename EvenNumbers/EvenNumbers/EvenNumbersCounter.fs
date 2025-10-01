/// A module containing three ways to count even numbers in a list.
module EvenNumbersCounter

    /// Counts even numbers in a list.
    let mapRealisation list =
        list |> List.map (fun element -> if element % 2 = 0 then 1 else 0) |> List.sum

    /// Counts even numbers in a list.
    let filterRealisation list =
        list |> List.filter (fun element -> element % 2 = 0) |> List.length
    
    /// Counts even numbers in a list.
    let foldRealisation list =
        list |> List.fold (fun acc element -> if element % 2 = 0 then acc + 1 else acc) 0