module Tests

open NUnit.Framework
open Lazy
open FsUnit

[<Test>]
let ``Single thread - simple supplier`` () =
    let supplierCalls = ref 0
    let supplier () = 
        supplierCalls.Value <- supplierCalls.Value + 1
        42
       
    let lazyInstance = SingleThreadLazy supplier :> ILazy<_>
        
    lazyInstance.Get() |> should equal 42
    supplierCalls.Value |> should equal 1
    
    lazyInstance.Get() |> should equal 42
    lazyInstance.Get() |> should equal 42
    supplierCalls.Value |> should equal 1