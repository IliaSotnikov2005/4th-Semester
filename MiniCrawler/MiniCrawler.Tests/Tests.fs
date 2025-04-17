module Tests

open NUnit.Framework
open FsUnit
open System.Net
open System.Net.Http
open System.Threading.Tasks
open MiniCrawler

type MockHttpMessageHandler() =
    inherit HttpMessageHandler()
    let mutable responses = Map.empty<string, HttpResponseMessage>
    
    member _.AddResponse(url, content, statusCode) =
        let response = new HttpResponseMessage(statusCode)
        response.Content <- new StringContent(content)
        responses <- responses.Add(url, response)
    
    override _.SendAsync(request, _) =
        match responses.TryFind(request.RequestUri.AbsoluteUri) with
        | Some response -> Task.FromResult(response)
        | None -> Task.FromResult(new HttpResponseMessage(HttpStatusCode.NotFound))

[<Test>]
let ``downloadHtmlAsync should return Ok with content for successful request`` () =
    async {
        let handler = new MockHttpMessageHandler()
        handler.AddResponse("http://example.com", "<html>test</html>", HttpStatusCode.OK)
        let httpClient = new HttpClient(handler)
        
        let! result = downloadHtmlAsync httpClient "http://example.com"
        
        match result with
        | Ok content -> content |> should equal "<html>test</html>"
        | Error _ -> Assert.Fail("Expected Ok result")
    } |> Async.RunSynchronously

[<Test>]
let ``downloadHtmlAsync should return Error for failed request`` () =
    async {
        let handler = new MockHttpMessageHandler()
        handler.AddResponse("http://bad-url.com", "Error", HttpStatusCode.NotFound)
        let httpClient = new HttpClient(handler)
        
        let! result = downloadHtmlAsync httpClient "http://bad-url.com"
        
        match result with
        | Ok _ -> Assert.Fail("Expected Error result")
        | Error msg -> msg |> should contain "404"
    } |> Async.RunSynchronously

[<Test>]
let ``crawl should return single error when main URL fails`` () =
    async {
        let handler = new MockHttpMessageHandler()
        handler.AddResponse("http://failing-url.com", "Error", HttpStatusCode.NotFound)
        let httpClient = new HttpClient(handler)
        
        let! results = crawl httpClient "http://failing-url.com"
        
        results |> should haveLength 1
        match results.[0] with
        | Error (u, msg) -> 
            u |> should equal "http://failing-url.com"
            msg |> should contain "404"
        | _ -> Assert.Fail("Expected Error result")
    } |> Async.RunSynchronously

[<Test>]
let ``crawl should process all links when main URL succeeds`` () =
    async {
        let handler = new MockHttpMessageHandler()
        handler.AddResponse("http://main.com", 
            """<a href="http://link1.com">Link1</a><a href="http://link2.com">Link2</a>""", 
            HttpStatusCode.OK)
        handler.AddResponse("http://link1.com", "link1 content", HttpStatusCode.OK)
        handler.AddResponse("http://link2.com", "Error", HttpStatusCode.NotFound)
        let httpClient = new HttpClient(handler)
        
        let! results = crawl httpClient "http://main.com"
        
        results |> should haveLength 2
        results |> Array.exists (function Ok ("http://link1.com", 13) -> true | _ -> false) |> should be True
        results |> Array.exists (function Error ("http://link2.com", msg) when msg.Contains("404") -> true | _ -> false) |> should be True
    } |> Async.RunSynchronously