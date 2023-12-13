module tests

open Parser
open Evaluator
open System
open NUnit.Framework

[<TestFixture>]
type TestClass () =
    let testProjectPath = IO.Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName

    [<Test>]
    member this.TestParser() =
        let path = IO.Path.Combine(testProjectPath, "TestData", "sam", "smallHomage.sam")
        let text = IO.File.ReadAllText path
        let expected = IO.File.ReadAllText (IO.Path.Combine(testProjectPath, "TestData", "parser", "smallHomage.txt"))
        let ast = parse text
        Assert.AreEqual(expected, (string ast) + "\n")

    [<Test>]
    member this.TestEval() =
        let path = IO.Path.Combine(testProjectPath, "TestData", "sam", "smallHomage.sam")
        let text = IO.File.ReadAllText path
        let expected = IO.File.ReadAllText (IO.Path.Combine(testProjectPath, "TestData", "svg", "smallHomage.svg"))

        match parse text with
            | Some ast -> Assert.AreEqual(expected, (eval ast) + "\n") 
            | None -> Assert.True(false)
