namespace LocalNetwork

type NavOS(?infectionChance: float) =
    interface IOperatingSystem with
        member val Name = "NavOS" with get
        member val InfectionChance = defaultArg infectionChance 1.0 with get