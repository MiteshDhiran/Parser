
open System
type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> Result<'a * string>)

let pchar charToMatch =
    let innerFn input = 
        if String.IsNullOrEmpty(input) then
            Failure "No More Input"
        else
           let first = input.[0]
           if first = charToMatch then
              let remaining = input.[1..]
              Success(first,remaining)
           else
               let msg = sprintf($"Expecting {charToMatch}. Got {first}")
               Failure msg
    Parser innerFn           

let run parser input =
    //unwrap the inner function
    let (Parser innerFunc) = parser
    //call inner function
    innerFunc input    

let pCharA = pchar 'A'    
let inputABC = "ABC"
run pCharA inputABC

//Parser andThen Parser
//Parser orElse Parser
//Parser map (transformer) => Parser

let andThen firstParser secondParser =  
    let innerFn input =  
         let firstParserResult = run firstParser input
         match firstParserResult with
            | Failure err -> 
                Failure err 
            | Success(value1,remaining1) ->
                let secondResult = run secondParser remaining1
                match secondResult with
                | Failure errs ->
                    Failure errs
                | Success(value2, remaining2) ->
                    let combineResult = (value1,value2)
                    Success (combineResult,remaining2)
    Parser innerFn

//andthen
let (.>>.) firstParser secondParser =  
    let innerFn input =  
         let firstParserResult = run firstParser input
         match firstParserResult with
            | Failure err -> 
                Failure err 
            | Success(value1,remaining1) ->
                let secondResult = run secondParser remaining1
                match secondResult with
                | Failure errs ->
                    Failure errs
                | Success(value2, remaining2) ->
                    let combineResult = (value1,value2)
                    Success (combineResult,remaining2)
    Parser innerFn    

let orElse firstParser secondParser =  
    let innerFn input =  
         let firstParserResult = run firstParser input
         match firstParserResult with
            | Failure err -> 
                 let secondParserResult = run secondParser input
                 match secondParserResult with
                 | Success (value2, remaining2) -> 
                        Success(value2,remaining2)
                 | Failure err2 ->
                        Failure err2       
            | Success(value1,remaining1) ->
                Success(value1,remaining1)
    Parser innerFn

//orThen
let (<|>) = orElse
 
let mapP f p = 
    let innerFunc input = 
        let result = run p input
        match result with
        | Failure err ->
            Failure err
        | Success (value1,remaining1) ->
            let funResult = f(value1)
            Success(funResult,remaining1)
    Parser innerFunc
let (|>>) p f = mapP f p 

let choice listOfParser =
    listOfParser |> List.reduce (<|>)

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice    

let parserAAndB = pchar 'A' .>>. pchar 'B'
let res = run parserAAndB "AZC"
let parseAnyLowerCharacters = anyOf ['a'..'z']

let sequence listOfParsers = 
    let concatResult p1 p2=
        p1 .>>. p2 
        |>> (fun(list1,list2) -> list1 @ list2)   
    listOfParsers
    |> Seq.map (fun p -> p |>> List.singleton)
    |> Seq.reduce concatResult

let pString str =
    str
    |> Seq.map pchar
    |> sequence
    |>> List.toArray
    |>> String    

let rec parseZeroOrMore parser input =
    //run the parser with input
    let firstResult = run parser input
    match firstResult with
    | Success (firstValue,remaining1) -> 
            let (subsequentValues,remainingInput) 
                = parseZeroOrMore parser remaining1
            let values = firstValue :: subsequentValues
            (values,remainingInput)     
    | Failure err ->
        ([],input)

(* let rec parseZeroOrMoreNew parser input =
    //run the parser with input
    let firstResult = run parser input
    match firstResult with
    | Success (firstValue,remaining1) -> 
            let (subsequentValues,remainingInput) 
                = parseZeroOrMoreNew parser remaining1
            let values = firstValue :: subsequentValues
            (values,remainingInput)     
    | Failure err ->
        ([],input) *)
//matches zero or more occurence of a given parser
let many parser =
    let rec innerFn input =
        //parse the input - wrap in success as its always successfull
        Success (parseZeroOrMore parser input)
    Parser innerFn

(* let many1New parser =
    let rec innerFn input =
        let firstResult = run parser input
        match firstResult with
        | Failure err -> 
            Failure err
        | Success (firstValue1,remainingValue1) ->
                let (subsequentValues,remainingInput) 
                    = parseZeroOrMoreNew parser remainingValue1
                let values =  firstValue1 :: subsequentValues
                Success(values,remainingInput)
    Parser innerFn *)

let many1 parser =
    let rec innerFn input =
        let firstResult = run parser input
        match firstResult with
        | Failure err -> 
            Failure err
        | Success (firstValue1,remainingValue1) ->
                let (subsequentValues,remainingInput) 
                    = parseZeroOrMore parser remainingValue1
                let values =  firstValue1 :: subsequentValues
                Success(values,remainingInput)
    Parser innerFn

(* let many1Custom parser =
    let rec innerFn input =
          let (valueArray, remaining) =  parseZeroOrMore parser input
          if valueArray.Length > 0 then
             Success (valueArray, remaining)
          else
             Failure "Not a single match found"
    Parser innerFn            

//let many p = 
let manyOld p =
    let innerFn input =
        let rec innermostFun inputValue agg =
                let result1 = run p inputValue
                match result1 with
                        | Success (value1,remaining1) -> innermostFun remaining1 (value1 @ agg) 
                        | Failure err ->
                                    Failure err
                                            
        innermostFun input []
        
    Parser innerFn *)

let whitespaceCharP = anyOf [' ';'\t';'\n']    
let whitespace = many1 whitespaceCharP
let inputWithWhiteSpace = "        Hello"
//let res = run whitespace "    123"
let res1 = run whitespace inputWithWhiteSpace
(* let whitespaceNew = many1New whitespaceCharP
let resNew = run  whitespaceNew inputWithWhiteSpace
 *)
// let rec parseZeroOrMoreNew parser input =
//     //run the parser with input
//     let firstResult = run parser input
//     match firstResult with
//     | Success (firstValue,remaining1) -> 
//             let (subsequentValues,remainingInput) 
//                 = parseZeroOrMoreNew parser remaining1
//             let values = firstValue :: subsequentValues
//             (values,remainingInput)     
//     | Failure err ->
//         ([],input)

// let many11 parser =
//     let rec innerFn input =
//         let firstResult = run parser input
//         match firstResult with
//         | Failure err -> 
//             Failure err
//         | Success (firstValue1,remainingValue1) ->
//                 let (subsequentValues,remainingInput) 
//                     = parseZeroOrMoreNew parser remainingValue1
//                 let values =  firstValue1 :: subsequentValues
//                 Success(values,remainingInput)
//     Parser innerFn

// let inputWithWhiteSpace = "        Hello"        
// let whitespace11 = many11 whitespaceCharP
// let res2 = parseZeroOrMoreNew whitespace11 inputWithWhiteSpace
// let res3 = parseZeroOrMoreNew whitespace inputWithWhiteSpace