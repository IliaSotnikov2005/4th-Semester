namespace LocalNetwork

module Test =
    let windowsOS = Windows()
    let computer1 = Computer("PC1", windowsOS)
    let computer2 = Computer("PC2", windowsOS)
    let computer3 = Computer("PC3", windowsOS)
    let computer4 = Computer("PC4", windowsOS)

    let connections = 
        Set [
            computer1, computer2
            computer2, computer3
            computer3, computer4
        ]

    let network = 
        LocalNetwork(
            computers = [computer1; computer2; computer3; computer4],
            connections = connections
        )

    printfn "Initial network state:"
    network.Print()

    computer3.IsInfected <- true
    printfn "\nAfter infecting PC1:"
    network.Print()

    printfn "\nRunning infection step..."
    network.Step() |> ignore
    network.Print()

    printfn "\nRunning infection step..."
    network.Step() |> ignore
    network.Print()