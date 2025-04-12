module MiniCrawler

open System.Text.RegularExpressions
open System.Net.Http

let downloadHtmlAsync (url: string) =
    async {
        use httpClient = new HttpClient()
        let! response = httpClient.GetStringAsync(url) |> Async.AwaitTask
        return response
    }

let getLinks (html: string) =
    let pattern = @"<a\s+[^>]*href\s*=\s*[""'](https?://[^""']+)[""'][^>]*>"
    Regex.Matches(html, pattern, RegexOptions.IgnoreCase)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Seq.distinct
    |> List.ofSeq

let crawl (url: string) =
    async {
        let! html = downloadHtmlAsync url
        let links = getLinks html
        let! result =
            links
            |> List.map (fun link ->
                async {
                    let! content = downloadHtmlAsync link
                    return link, content.Length
                })
            |> Async.Parallel
        let res = result |> Set.ofArray
        return res
    }

let printResult (result: Set<string * int>) =
    result
    |> Set.iter (fun (url, length) -> printfn "%s - %d chars" url length)

let main() =
    async {
        let! result = crawl "http://localhost:8000/test.html"
        printResult result
    }

main() |> Async.RunSynchronously // python -m http.server 8000
