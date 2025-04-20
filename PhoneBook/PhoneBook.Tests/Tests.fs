module Tests

open Xunit
open FsUnit.Xunit
open PhoneBook
open System.IO

let testEntry1 = { Name = "Alice"; PhoneNumber = "1234567890" }
let testEntry2 = { Name = "Bob"; PhoneNumber = "0987654321" }
let testPhoneBook = [testEntry1; testEntry2]

[<Fact>]
let ``add should add a new entry to the phone book`` () =
    let resultMessage, updatedPhoneBook = add "Charlie" "5555555555" testPhoneBook
    resultMessage |> should equal (Message "OK")
    updatedPhoneBook |> should contain { Name = "Charlie"; PhoneNumber = "5555555555" }

[<Fact>]
let ``add should not add an entry with a duplicate phone number`` () =
    let resultMessage, updatedPhoneBook = add "Alice" "1234567890" testPhoneBook
    resultMessage |> should equal (Message "This phone number is already exists in the phone book.")
    updatedPhoneBook |> should equal testPhoneBook

[<Fact>]
let ``findNumberByName should return the correct entry`` () =
    let result, _ = findNumberByName "Alice" testPhoneBook
    match result with
    | Entry entry -> entry |> should equal testEntry1
    | _ -> Assert.True(false, "Expected Entry but got something else")

[<Fact>]
let ``findNumberByName should return a message if name is not found`` () =
    let result, _ = findNumberByName "Charlie" testPhoneBook
    match result with
    | Message msg -> msg |> should equal "Name is not in the phone book."
    | _ -> Assert.True(false, "Expected Message but got something else")

[<Fact>]
let ``findNameByNumber should return the correct entry`` () =
    let result, _ = findNameByNumber "0987654321" testPhoneBook
    match result with
    | Entry entry -> entry |> should equal testEntry2
    | _ -> Assert.True(false, "Expected Entry but got something else")

[<Fact>]
let ``findNameByNumber should return a message if phone number is not found`` () =
    let result, _ = findNameByNumber "5555555555" testPhoneBook
    match result with
    | Message msg -> msg |> should equal "Phone number is not in the phone book."
    | _ -> Assert.True(false, "Expected Message but got something else")

[<Fact>]
let ``listEntries should return all entries`` () =
    let result, _ = listEntries testPhoneBook
    match result with
    | AllEntries entries -> entries |> should equal testPhoneBook
    | _ -> Assert.True(false, "Expected AllEntries but got something else")

[<Fact>]
let ``save should save the phone book to a file`` () =
    let path = "testPhoneBook.txt"
    let _, updatedPhoneBook = save path testPhoneBook
    File.Exists(path) |> should be True
    let lines = File.ReadAllLines path
    lines.Length |> should equal 2
    File.Delete path

[<Fact>]
let ``read should read the phone book from a file`` () =
    let path = "testPhoneBook.txt"
    File.WriteAllLines(path, [|"Alice - 1234567890"; "Bob - 0987654321"|])
    let result, readPhoneBook = read path []
    match result with
    | Message msg -> msg |> should equal "OK"
    | _ -> Assert.True(false, "Expected Message but got something else")
    readPhoneBook |> should equal testPhoneBook
    File.Delete path

[<Fact>]
let ``read should return a message if file is not found`` () =
    let path = "nonexistent.txt"
    let result, _ = read path []
    match result with
    | Message msg -> msg |> should equal "File not found"
    | _ -> Assert.True(false, "Expected Message but got something else")