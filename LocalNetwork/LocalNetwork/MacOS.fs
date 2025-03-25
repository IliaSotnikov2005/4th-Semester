namespace LocalNetwork

type MacOS(?infectionChance: float) =
    interface IOperatingSystem with
        member val Name = "MacOS" with get
        member val InfectionChance = defaultArg infectionChance 0.2 with get