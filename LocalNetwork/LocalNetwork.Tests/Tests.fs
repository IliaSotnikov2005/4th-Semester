module Tests

open Xunit
open FsUnit.Xunit
open Moq
open LocalNetwork

[<Fact>]
let ``With a probability of 0, computers are not infected`` () =
    let osMock = Mock<IOperatingSystem>()
    osMock.Setup(fun x -> x.InfectionChance).Returns 0.0 |> ignore

    let c1 = Computer ("PC1", Windows ())
    let c2 = Computer ("PC2", osMock.Object)
    let c3 = Computer ("PC3", osMock.Object)
    let c4 = Computer ("PC4", osMock.Object)

    c1.IsInfected <- true

    let connections = Set [
        c1, c2
        c1, c3
        c1, c4
    ]

    let network = LocalNetwork (computers, connections)

    let result = network.Step()

    result |> should be False

