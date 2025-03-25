namespace LocalNetwork

type Computer(name: string, os: IOperatingSystem) =
    let random = System.Random()

    member val Name = name with get
    member val Os = os with get
    member val IsInfected = false with get, set
    
    member this.TryInfect() =
        if not this.IsInfected && random.NextDouble() < this.Os.InfectionChance then
            this.IsInfected <- true
            true
        else
            false

    interface System.IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Computer as c -> this.Name.CompareTo c.Name
            | _ -> -1