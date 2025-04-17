module Tests

open NUnit.Framework
open FsUnit
open System.Net
open System.Net.Http
open System.Threading.Tasks
open MiniCrawler

[<Test>]
let ``downloadHtmlAsync should fail on non-existent URL`` () =
    async {
        use httpClient = new HttpClient()
        let! result = downloadHtmlAsync httpClient "http://this.url.does.not.exist.test"
        
        match result with
        | Ok _ -> Assert.Fail("Expected error for non-existent URL")
        | Error msg -> 
            printfn "Got expected error for non-existent URL: %s" msg
    } |> Async.RunSynchronously

[<Test>]
let ``getLinks should find links on example.com`` () =
    async {
        use httpClient = new HttpClient()
        let! result = downloadHtmlAsync httpClient "https://example.com"
        
        match result with
        | Ok html ->
            let links = getLinks html
            links |> should contain "https://www.iana.org/domains/example"
        | Error msg -> Assert.Fail($"Failed to download example.com: {msg}")
    } |> Async.RunSynchronously