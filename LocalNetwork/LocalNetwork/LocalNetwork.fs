namespace LocalNetwork

type Connection = Computer * Computer

type LocalNetwork(computers: Computer list, connections: Connection Set) =
    member this.Computers = computers
    member this.Connections = connections

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
                computersAtRisk |> Set.iter (fun c -> c.TryInfect() |> ignore)
                true
        
    member this.Print() =
        let maxLeftLength = 
            this.Connections |> Set.map(fun (c1, _) -> c1.Name.Length + if c1.IsInfected then " (infected)".Length else 0) |> Set.maxElement

        this.Connections
        |> Set.iter (fun (c1, c2) ->
            let left = sprintf "%s%s" c1.Name (if c1.IsInfected then " (infected)" else "")
            let right = sprintf "%s%s" c2.Name (if c2.IsInfected then " (infected)" else "")
            printfn "%*s <-> %s" maxLeftLength left right)