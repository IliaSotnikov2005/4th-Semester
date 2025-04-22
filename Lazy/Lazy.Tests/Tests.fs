module Tests

open NUnit.Framework
open Lazy
open FsUnit
open System.Threading
open System.Threading.Tasks

type LazyImplementation = 
    | SingleThread
    | ThreadSafe
    | LockFree

let createLazy (impl: LazyImplementation) (supplier: unit -> 'a) : ILazy<'a> =
    match impl with
    | SingleThread -> SingleThreadLazy(supplier) :> ILazy<'a>
    | ThreadSafe -> ThreadSafeLazy(supplier) :> ILazy<'a>
    | LockFree -> LockFreeLazy(supplier) :> ILazy<'a>

let singleThreadLazyImplementations = 
    [ SingleThread; ThreadSafe; LockFree ]
    |> List.map (fun x -> TestCaseData(x).SetName(sprintf "%A" x))

let multiThreadLazyImplementations = 
    [ ThreadSafe; LockFree ]
    |> List.map (fun x -> TestCaseData(x).SetName(sprintf "%A" x))

[<TestCaseSource("singleThreadLazyImplementations")>]
let ``Get should compute value only once`` (impl: LazyImplementation) =
    let mutable computations = 0
    let supplier () = 
        computations <- computations + 1
        42
        
    let lazyInstance = createLazy impl supplier
    computations |> should equal 0
    
    let first = lazyInstance.Get()
    first |> should equal 42
    computations |> should equal 1
    
    let second = lazyInstance.Get()
    second |> should equal 42
    computations |> should equal 1

[<TestCaseSource("singleThreadLazyImplementations")>]
let ``Get should return same value on subsequent calls`` (impl: LazyImplementation) =
    let random = System.Random()
    let value = random.Next()
    
    let lazyInstance = createLazy impl (fun () -> value)
    let first = lazyInstance.Get()
    let second = lazyInstance.Get()
    let third = lazyInstance.Get()
    
    first |> should equal second
    second |> should equal third
    third |> should equal value

[<TestCaseSource("singleThreadLazyImplementations")>]
let ``Get should handle null values`` (impl: LazyImplementation) =
    let lazyInstance = createLazy impl (fun () -> null)
    lazyInstance.Get() |> should be null

[<TestCaseSource("multiThreadLazyImplementations")>]
let ``ThreadSafe implementations should compute value once in multi-threaded environment`` (impl: LazyImplementation) =
    let mutable computations = 0
    let supplier () =
        Interlocked.Increment(&computations) |> ignore
        Thread.Sleep 50
        100
        
    let lazyInstance = createLazy impl supplier
    
    let results = Array.zeroCreate<int> 1000
    let threads = Array.zeroCreate<Thread> 1000
    
    for i in 0..999 do
        threads.[i] <- Thread(fun () -> results.[i] <- lazyInstance.Get())
        threads.[i].Start()
    
    for thread in threads do
        thread.Join()
    
    computations |> should equal 1
    
    results |> Array.forall (fun x -> x = 100) |> should be True

[<TestCaseSource("singleThreadLazyImplementations")>]
let ``Get should propagate exceptions from supplier`` (impl: LazyImplementation) =
    let exceptionSupplier () = raise (System.InvalidOperationException "Test exception")
    
    let lazyInstance = createLazy impl exceptionSupplier
    (fun () -> lazyInstance.Get() |> ignore) |> should throw typeof<System.InvalidOperationException>
    
    (fun () -> lazyInstance.Get() |> ignore) |> should throw typeof<System.InvalidOperationException>

[<Test>]
let ``LockFreeLazy should work correctly under high contention`` () =
    let mutable computations = 0
    let supplier () =
        Interlocked.Increment(&computations) |> ignore
        Thread.SpinWait 1000
        42
        
    let lazyInstance = createLazy LockFree supplier
    let count = 100
    let tasks = Array.zeroCreate<Task<int>> count
    
    for i in 0..count-1 do
        tasks.[i] <- Task.Run(fun () -> lazyInstance.Get())
    
    Task.WaitAll(tasks |> Array.map (fun t -> t :> Task))
    
    computations |> should equal 1
    tasks |> Array.iter (fun t -> t.Result |> should equal 42)