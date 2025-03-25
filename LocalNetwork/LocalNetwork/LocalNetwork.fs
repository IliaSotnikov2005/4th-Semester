namespace LocalNetwork

type Connection = Computer * Computer

type LocalNetwork(computers: Computer list, connections: Connection Set) =
    member this.Computers = computers
    member this.Connenctions = connections

    member this.getComputersAtRick () =
        connections
        |> Set.fold (
            fun atRick (c1, c2) ->
            match c1.IsInfected, c2.IsInfected with
            | true, false -> Set.add c2 atRick
            | false, true -> Set.add c1 atRick
            | _ -> atRick
        ) Set.empty

    member this.Step () =
        let computersAtRisk = this.getComputersAtRick()
        
        match computersAtRisk.IsEmpty with
        | true -> false
        | false ->
            let infectionResults =
                computersAtRisk
                |> Set.map (fun computer -> 
                    let infected = computer.TryInfect()
                    computer.Name, infected)
            
            infectionResults
            |> Set.filter(fun (name, infected) -> infected)
            |> Set.iter (fun (name, result) ->
                printfn $"{name} - INFECTED")
            true