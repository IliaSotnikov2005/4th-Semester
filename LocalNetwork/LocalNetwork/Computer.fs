namespace LocalNetwork

/// A type representing computer.
type Computer(name: string, os: IOperatingSystem) =
    let random = System.Random()

    /// Gets name of the computer.
    member val Name = name with get

    /// Gets Os of the computer.
    member val Os = os with get

    /// Gets whether computer is infected.
    member val IsInfected = false with get, set
    
    /// Tries to infect computer.
    member this.TryInfect() =
        if not this.IsInfected && random.NextDouble() < this.Os.InfectionChance then
            this.IsInfected <- true
            true
        else
            false

    interface System.IComparable with
        /// Compares computer with another by name.
        member this.CompareTo(other) =
            match other with
            | :? Computer as c -> this.Name.CompareTo c.Name
            | _ -> -1