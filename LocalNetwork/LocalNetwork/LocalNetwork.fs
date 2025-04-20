namespace LocalNetwork

/// A type representing connection between two computers.
type Connection = Computer * Computer

/// A type representing local network.
type LocalNetwork(computers: Computer list, connections: Connection Set) =
    
    /// Computers in the local network.
    member this.Computers = computers
   
    /// Connections in the local network.
    member this.Connections = connections

    /// Gets computers at infection risk.
    member this.getComputersAtRick () =
        connections
        |> Set.fold (
            fun atRick (c1, c2) ->
            match c1.IsInfected, c2.IsInfected with
            | true, false -> if c2.Os.InfectionChance <> 0 then Set.add c2 atRick else atRick
            | false, true -> if c1.Os.InfectionChance <> 0 then Set.add c1 atRick else atRick
            | _ -> atRick
        ) Set.empty

    /// Step of infection.
    member this.Step () =
        let computersAtRisk = this.getComputersAtRick()
        
        match computersAtRisk.IsEmpty with
        | true -> false
        | false ->
                computersAtRisk |> Set.iter (fun c -> c.TryInfect() |> ignore)
                true
        
    /// Prints network status.
    member this.Print() =
        let maxLeftLength = 
            this.Connections |> Set.map(fun (c1, _) -> c1.Name.Length + if c1.IsInfected then " (infected)".Length else 0) |> Set.maxElement

        this.Connections
        |> Set.iter (fun (c1, c2) ->
            let left = sprintf "%s%s" c1.Name (if c1.IsInfected then " (infected)" else "")
            let right = sprintf "%s%s" c2.Name (if c2.IsInfected then " (infected)" else "")
            printfn "%*s <-> %s" maxLeftLength left right)