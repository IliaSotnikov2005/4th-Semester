namespace LocalNetwork

/// A type representing Linux OS.
type Linux(?infectionChance: float) =
    interface IOperatingSystem with
        member val Name = "Linux" with get
        member val InfectionChance = defaultArg infectionChance 0.4 with get