module PhoneBook

type Entry = {Name: string; PhoneNumber: string}

type PhoneBook = Entry list

type Response =
| Message of string
| Entry of Entry
| AllEntries of PhoneBook

let add name phoneNumber phoneBook =
    let newEntry = { Name = name; PhoneNumber = phoneNumber }
    if List.exists (fun entry -> entry.PhoneNumber = phoneNumber) phoneBook then
        Message "This phone number is already exists in the phone book.", phoneBook
    else
        Message "OK", newEntry :: phoneBook

let findNumberByName name phoneBook =
    let result = phoneBook |> List.tryFind(fun entry -> entry.Name = name)
    match result with
    | None ->
        Message "Name is not in the phone book.", phoneBook
    | Some entry ->
        Entry entry, phoneBook

let findNameByNumber phoneNumber phoneBook =
    let result = phoneBook |> List.tryFind(fun entry -> entry.PhoneNumber = phoneNumber)
    match result with
    | None ->
        Message "Phone number is not in the phone book.", phoneBook
    | Some entry ->
        Entry entry, phoneBook

let listEntries phoneBook =
    AllEntries phoneBook, phoneBook

let save path phoneBook =
    let defaultPath = "./phoneBook.txt"
    let filePath = if path = "" then defaultPath else path
    let lines = phoneBook |> List.map(fun entry -> $"{entry.Name} - {entry.PhoneNumber}")
    System.IO.File.WriteAllLines(filePath, lines)
    Message "OK", phoneBook

let read path phoneBook =
    let defaultPath = "./phoneBook.txt"
    let filePath = if path = "" then defaultPath else path

    if System.IO.File.Exists(filePath) then
        let lines = System.IO.File.ReadAllLines(filePath)
        let entries =
            lines |> Array.map (fun line ->
                let parts = line.Split " - "
                { Name = parts.[0]; PhoneNumber = parts.[1]})
                |> Array.toList
        Message "OK", entries
    else
        Message "File not found", phoneBook