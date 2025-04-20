namespace LocalNetwork

/// An interface representing operating system.
type IOperatingSystem =
    /// Gets a name of the OS.
    abstract member Name: string with get

    /// Gets an infection chanse of the OS,
    abstract member InfectionChance: float with get
