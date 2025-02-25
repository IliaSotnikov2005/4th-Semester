module EvenNumbersCounter
    let mapRealisation list =
        list |> List.map (fun element -> if element % 2 = 0 then 1 else 0) |> List.sum

    let filterRealisation list =
        list |> List.filter (fun element -> element % 2 = 0) |> List.length
    
    let foldRealisation list =
        list |> List.fold (fun acc element -> if element % 2 = 0 then acc + 1 else acc) 0