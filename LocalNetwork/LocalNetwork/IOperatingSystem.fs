namespace LocalNetwork

type IOperatingSystem =
    abstract member Name: string with get
    abstract member InfectionChance: float with get
