/// A module containing mini crawler.
module MiniCrawler

open System.Text.RegularExpressions
open System.Net.Http

/// Downloads html async.
let downloadHtmlAsync (httpClient: HttpClient) (url: string) =
    async {
        try
            let! response = httpClient.GetAsync(url) |> Async.AwaitTask
            response.EnsureSuccessStatusCode() |> ignore
            let! html = response.Content.ReadAsStringAsync() |> Async.AwaitTask
            return Ok html
        with ex ->
            return Error ex.Message
    }

/// Gets links from html.
let getLinks (html: string) =
    let pattern = @"<a\s+href\s*=\s*""(https?://[^""]+)""[^>]*>[^<]*<\/a>"
    Regex.Matches(html, pattern, RegexOptions.IgnoreCase)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.distinct
    |> List.ofSeq

/// Crawls through the html page by its url.
let crawl (httpClient: HttpClient) (url: string) =
    async {
        let! html = downloadHtmlAsync httpClient url
        match html with
        | Ok html ->
            let links = getLinks html
            let! result =
                links
                |> List.map (fun link ->
                    async {
                        let! content = downloadHtmlAsync httpClient link
                        match content with
                        | Ok content ->
                            return Ok (link, content.Length)
                        | Error message ->
                            return Error (link, message)
                    })
                |> Async.Parallel
            
            return result
        | Error message-> return [| Error (url, message) |]
    }

/// Prints results.
let printResults (results: Result<string * int, string * string>[]) =
    results |> Array.iter (function
        | Ok (url, length) ->
            printfn "%s - %d chars" url length
        | Error (url, errorMsg) ->
            printfn "%s - Error when executing request: %s" url errorMsg
    )
