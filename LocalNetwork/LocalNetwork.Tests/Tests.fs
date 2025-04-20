module Tests

open Xunit
open FsUnit.Xunit
open Moq
open LocalNetwork

[<Fact>]
let ``With a probability of 0, computers are not infected`` () =
    let osMock = Mock<IOperatingSystem>()
    osMock.Setup(fun x -> x.InfectionChance).Returns 0.0 |> ignore

    let c1 = Computer ("PC1", osMock.Object)
    let c2 = Computer ("PC2", osMock.Object)
    let c3 = Computer ("PC3", osMock.Object)
    let c4 = Computer ("PC4", osMock.Object)

    c1.IsInfected <- true

    let connections = Set [
        c1, c2
        c1, c3
        c1, c4
    ]

    let network = LocalNetwork ([c1;c2;c3;c4], connections)

    let result = network.Step()

    result |> should be False

[<Fact>]
let ``With a probability of 1, the infection spreads as BFS`` () =
    let osMock = Mock<IOperatingSystem>()
    osMock.Setup(fun x -> x.InfectionChance).Returns 1.0 |> ignore

    let c1 = Computer ("PC1", osMock.Object)
    let c2 = Computer ("PC2", osMock.Object)
    let c3 = Computer ("PC3", osMock.Object)
    let c4 = Computer ("PC4", osMock.Object)

    c1.IsInfected <- true

    let connections = Set [
        c1, c2
        c1, c3
        c2, c4
    ]

    let network = LocalNetwork ([c1;c2;c3;c4], connections)

    network.Step() |> should be True
    c2.IsInfected |> should be True
    c3.IsInfected |> should be True
    c4.IsInfected |> should be False

    network.Step() |> should be True
    c4.IsInfected |> should be True

    network.Step() |> should be False


[<Fact>]
let ``Step reurns false if the state cannot change`` () =
    let osMock = Mock<IOperatingSystem>()
    osMock.Setup(fun x -> x.InfectionChance).Returns 1.0 |> ignore
    let strongOsMock = Mock<IOperatingSystem>()
    osMock.Setup(fun x -> x.InfectionChance).Returns 0.0 |> ignore
    
    let c1 = Computer("PC1", osMock.Object)
    let c2 = Computer("PC2", osMock.Object)

    // reachable but not infectable
    let c3 = Computer("PC3", strongOsMock.Object)

    // infectable but not reachable
    let c4 = Computer("PC4", osMock.Object)
    c1.IsInfected <- true
    c2.IsInfected <- true
    
    let connections = Set [
        c1, c2
        c2, c3
    ]
    let network = LocalNetwork([c1; c2; c3; c4], connections)
    
    let result = network.Step()
    
    result |> should be False