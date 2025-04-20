namespace LocalNetwork

/// A type representing MacOS.
type MacOS(?infectionChance: float) =
    interface IOperatingSystem with
        member val Name = "MacOS" with get
        member val InfectionChance = defaultArg infectionChance 0.2 with get