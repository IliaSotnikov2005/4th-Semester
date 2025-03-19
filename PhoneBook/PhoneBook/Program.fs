module PhoneBookIntereface

open PhoneBook
open System

let printCommands () =
    printf "
    Phone book
    ==========
    Commands:
    help - print commands
    exit - exit without saving
    add <name> <phoneNumber> - add entry
    find phone <name> - find entry by name
    find name <phoneNumber> - find entry by phone number
    list - list all entries
    save <filepath> - save current data to file, default path is ./phoneBook.txt
    read <filepath> - read data from file, default is ./phoneBook.txt
    "

let handleResponse response phoneBook =
    match response with
    | Message message ->
        printfn "%s" message
        phoneBook
    | Entry {Name = name; PhoneNumber = phoneNumber} ->
        printfn "%s - %s" name phoneNumber
        phoneBook
    | AllEntries entries ->
        for entry in entries do
            printfn "%s - %s" entry.Name entry.PhoneNumber
        phoneBook

let handleCommand command phoneBook =
    match command with
        | [| "help" |] ->
            printCommands ()
            phoneBook
        | [| "exit" |] ->
            Environment.Exit 0
            phoneBook
        | [| "add"; name; phoneNumber |] ->
            let response, phoneBook =  add name phoneNumber phoneBook
            handleResponse response phoneBook
        | [| "find"; "phone"; name |] ->
            let response, phoneBook = findNumberByName name phoneBook
            handleResponse response phoneBook
        | [| "find"; "name"; phoneNumber |] ->
            let response, phoneBook = findNameByNumber phoneNumber phoneBook
            handleResponse response phoneBook
        | [| "list" |] ->
            let response, phoneBook = listEntries phoneBook
            handleResponse response phoneBook
        | [| "save"; path |] ->
            let response, phoneBook = save path phoneBook
            handleResponse response phoneBook
        | [| "read"; path |] ->
            let response, phoneBook = read path phoneBook
            handleResponse response phoneBook
        | _ ->
            printf "Invalid command. Use help for help."
            phoneBook

let start =
    let rec mainLoop phoneBook =
        printf "\n> "
        let command =
            Console.ReadLine().Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries)

        let updatedPhoneBook = handleCommand command phoneBook
        mainLoop updatedPhoneBook

    printCommands ()
    mainLoop []

start()