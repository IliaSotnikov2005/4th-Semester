namespace LocalNetwork

/// A type representing Windows.
type Windows(?infectionChance: float) =
    interface IOperatingSystem with
        member val Name = "Windows" with get
        member val InfectionChance = defaultArg infectionChance 0.8 with get