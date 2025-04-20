/// A phone book module.
module PhoneBook

/// A type representing an entry in the phone book.
type Entry = {Name: string; PhoneNumber: string}

/// A type representing phone book.
type PhoneBook = Entry list

/// A type representing response from the phone book.
type Response =
| Message of string
| Entry of Entry
| AllEntries of PhoneBook

/// Adds name and phone number to the phone book.
let add name phoneNumber phoneBook =
    let newEntry = { Name = name; PhoneNumber = phoneNumber }
    if List.exists (fun entry -> entry.PhoneNumber = phoneNumber) phoneBook then
        Message "This phone number is already exists in the phone book.", phoneBook
    else
        Message "OK", newEntry :: phoneBook

/// Finds number by name.
let findNumberByName name phoneBook =
    let result = phoneBook |> List.tryFind(fun entry -> entry.Name = name)
    match result with
    | None ->
        Message "Name is not in the phone book.", phoneBook
    | Some entry ->
        Entry entry, phoneBook

/// Finds name by number.
let findNameByNumber phoneNumber phoneBook =
    let result = phoneBook |> List.tryFind(fun entry -> entry.PhoneNumber = phoneNumber)
    match result with
    | None ->
        Message "Phone number is not in the phone book.", phoneBook
    | Some entry ->
        Entry entry, phoneBook

/// Gets all entries.
let listEntries phoneBook =
    AllEntries phoneBook, phoneBook

/// Saves phone book to the file.
let save path phoneBook =
    let lines = phoneBook |> List.map(fun entry -> $"{entry.Name} - {entry.PhoneNumber}")
    System.IO.File.WriteAllLines(path, lines)
    Message "OK", phoneBook

/// Reads phone book from the file.
let read path phoneBook =
    if System.IO.File.Exists path then
        let lines = System.IO.File.ReadAllLines(path)
        let entries =
            lines |> Array.map (fun line ->
                let parts = line.Split " - "
                { Name = parts.[0]; PhoneNumber = parts.[1]})
                |> Array.toList
        Message "OK", entries
    else
        Message "File not found", phoneBook