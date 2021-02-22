open System
type Position = {
        Line : int
        Column : int
    }
let initialPos = {Line=0; Column=0}

let incrCol pos = 
        {pos with Column=pos.Column + 1}

    /// increment the line number and set the column to 0
let incrLine pos = 
    {Line=pos.Line + 1; Column=0}

type InputState = {
        Lines : string[]
        Position : Position 
    }

let currentLine (inputState:InputState) = 
        let linePos = inputState.Position.Line
        if linePos < inputState.Lines.Length then
            inputState.Lines.[linePos]
        else
            "end of file"

let fromStr str = 
        if String.IsNullOrEmpty(str) then
            {Lines=[||]; Position=initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = str.Split(separators, StringSplitOptions.None)
            {Lines=lines; Position=initialPos}

/// Get the next character from the input, if any
    /// else return None. Also return the updated InputState
    /// Signature: InputState -> InputState * char option 
let nextChar (input:InputState)  =
    let linePos = input.Position.Line
    let colPos = input.Position.Column
    // three cases
    // 1) if line >= maxLine -> 
    //       return EOF
    // 2) if col less than line length -> 
    //       return char at colPos, increment colPos
    // 3) if col at line length -> 
    //       return NewLine, increment linePos

    if linePos >= input.Lines.Length then
        input, None
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.Position 
            let newState = {input with Position=newPos}
            newState, Some char
        else 
            // end of line, so return LF and move to next line
            let char = '\n'
            let newPos = incrLine input.Position 
            let newState = {input with Position=newPos}
            newState, Some char

type ParserLabel = string
type ParserError = string
type ParserPosition = {
    CurrentLine : string
    Line : int
    Column : int
    }

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition 

type Parser<'a> = 
    {
        ParseFn : (InputState -> Result<'a * InputState>)
        Label:  ParserLabel 
    }


/// Run the parser on a InputState
let runOnInput parser input = 
    // call inner function with input
    parser.ParseFn input

/// Run the parser on a string
let run parser inputStr = 
    // call inner function with input
    runOnInput parser (fromStr inputStr)


// =============================================
// Error messages
// =============================================

let parserPositionFromInputState (inputState:InputState) = {
    CurrentLine = currentLine inputState
    Line = inputState.Position.Line
    Column = inputState.Position.Column
    }
        
let printResult result =
    match result with
    | Success (value,input) -> 
        printfn "%A" value
    | Failure (label,error,parserPos) -> 
        let errorLine = parserPos.CurrentLine
        let colPos = parserPos.Column
        let linePos = parserPos.Line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        // examples of formatting
        //   sprintf "%*s^%s" 0 "" "test"
        //   sprintf "%*s^%s" 10 "" "test"
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret 


// =============================================
// Label related
// =============================================

/// get the label from a parser
let getLabel parser = 
    // get label
    parser.Label

/// update the label in the parser
let setLabel parser newLabel = 
    // change the inner function to use the new label
    let newInnerFn input = 
        let result = parser.ParseFn input
        match result with
        | Success s ->
            // if Success, do nothing
            Success s 
        | Failure (oldLabel,err,pos) -> 
            // if Failure, return new label
            Failure (newLabel,err,pos) 
    // return the Parser
    {ParseFn=newInnerFn; Label=newLabel}

/// infix version of setLabel
let ( <?> ) = setLabel


let satisfy predicate label=
    let innerFn (input:InputState) =
        let (remainingInput:InputState),charOpt = nextChar input
        match charOpt with
        | None -> 
            let (err:ParserError) = "No more input"
            let (pos:ParserPosition) = parserPositionFromInputState input
            Failure (label,err,pos)
        | Some first -> 
            if predicate first then
                Success (first,remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input
                Failure (label,err,pos)               
    {ParseFn=innerFn; Label=label}

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let label = "unknown"
    let innerFn input =
        let result1 = runOnInput p input 
        match result1 with
        | Failure (label,err,pos) -> 
            // return error from parser1
            Failure (label,err,pos)  
        | Success (value1,remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    {ParseFn=innerFn; Label=label}    

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP x = 
    let label = sprintf "%A" x
    let innerFn input =
        // ignore the input and return x
        Success (x,input)
    // return the inner function
    {ParseFn=innerFn; Label=label}

/// apply a function to the value inside a parser
let mapP f = 
    bindP (f >> returnP)


/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
//Parser<('a -> 'b)> -> Parser<'a> -> Parser<'b>
let applyP fP xP =         
    fP >>= (fun f -> 
    xP >>= (fun x ->  (f x) |> returnP ))

//let applyPNewNewA {ParseFn f; Label l1} {ParseFn f2; Label l2} = {ParseFn=f;Label=l1}
/// infix version of apply
let ( <*> ) = applyP

let applyP1 fP xP =         
    bindP (fun f -> 
    xP >>= (fun x ->  (f x) |> returnP )) fP

 let applyP11 fP xP =         
    bindP (fun f -> bindP (fun x ->  (f x) |> returnP ) xP) fP    

/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP    

let lift2N f xP yP =
     (returnP f) <*> xP <*> yP

        
/// Combine two parsers as "A andThen B"
let andThen p1 p2 =         
    let label = sprintf "%s andThen %s" (getLabel p1) (getLabel p2)
    p1 >>= (fun p1Result -> 
    p2 >>= (fun p2Result -> 
        returnP (p1Result,p2Result) ))
    <?> label

/// Infix version of andThen
let ( .>>. ) = andThen
/// Combine two parsers as "A orElse B"
let orElse p1 p2 =
    let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
    let innerFn input =
        // run parser1 with the input
        let result1 = runOnInput p1 input

        // test the result for Failure/Success
        match result1 with
        | Success result -> 
            // if success, return the original result
            result1

        | Failure _ -> 
            // if failed, run parser2 with the input
            let result2 = runOnInput p2 input

            // return parser2's result
            result2 

    // return the inner function
    {ParseFn=innerFn; Label=label}

let (<|>) = orElse        

let (>>%) p x =
    p |>> (fun _ -> x)

let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers    

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> 
        returnP []
    | head::tail ->
        consP head (sequence tail)    

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input 
    // test the result for Failure/Success
    match firstResult with
    | Failure (_,_,_) -> 
        // if parse fails, return empty list
        ([],input)  
    | Success (firstValue,inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)  

/// matches zero or more occurences of the specified parser
let many parser = 
    let label = sprintf "many %s" (getLabel parser)
    let rec innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser input)
    {ParseFn=innerFn; Label=label}

/// matches one or more occurences of the specified parser
let many1 p =         
    let label = sprintf "many1 %s" (getLabel p)

    p      >>= (fun head -> 
    many p >>= (fun tail -> 
        returnP (head::tail) ))
    <?> label        

/// Parses an optional occurrence of p and returns an option value.
let opt p = 
    let label = sprintf "opt %s" (getLabel p)
    let some = p |>> Some
    let none = returnP None
    (some <|> none) <?> label    


let (.>>) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the first value
    |> mapP (fun (a,b) -> a)     

/// Keep only the result of the right side parser
let (>>.) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the second value
    |> mapP (fun (a,b) -> b) 

/// Keep only the result of the middle parser
let between p1 p2 p3 = 
    p1 >>. p2 .>> p3     

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p            
    p .>>. many sepThenP 
    |>> fun (p,pList) -> p::pList    

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []


// =============================================
// Standard parsers 
// =============================================


// ------------------------------
// char and string parsing
// ------------------------------
            
/// parse a char 
let pchar charToMatch = 
    // label is just the character
    let label = sprintf "%c" charToMatch 

    let predicate ch = (ch = charToMatch) 
    satisfy predicate label 

/// Choose any of a list of characters
let anyOf listOfChars = 
    let label = sprintf "anyOf %A" listOfChars 
    listOfChars
    |> List.map pchar // convert into parsers
    |> choice
    <?> label

/// Convert a list of chars to a string
let charListToStr charList =
    String(List.toArray charList) 

/// Parses a sequence of zero or more chars with the char parser cp. 
/// It returns the parsed chars as a string.
let manyChars cp =
    many cp
    |>> charListToStr

/// Parses a sequence of one or more chars with the char parser cp. 
/// It returns the parsed chars as a string.
let manyChars1 cp =
    many1 cp
    |>> charListToStr

/// parse a specific string
let pstring str = 
    // label is just the string
    let label = str 

    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar 
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr 
    <?> label

// ------------------------------
// whitespace parsing
// ------------------------------

/// parse a whitespace char
let whitespaceChar = 
    let predicate = Char.IsWhiteSpace 
    let label = "whitespace"
    satisfy predicate label 

/// parse zero or more whitespace char
let spaces = many whitespaceChar

/// parse one or more whitespace char
let spaces1 = many1 whitespaceChar



// ------------------------------
// number parsing
// ------------------------------

/// parse a digit
let digitChar = 
    let predicate = Char.IsDigit 
    let label = "digit"
    satisfy predicate label 


// parse an integer
let pint = 
    let label = "integer" 

    // helper
    let resultToInt (sign,digits) = 
        let i = digits |> int  // ignore int overflow for now
        match sign with
        | Some ch -> -i  // negate the int
        | None -> i
            
    // define parser for one or more digits
    let digits = manyChars1 digitChar 

    // an "int" is optional sign + one or more digits
    opt (pchar '-') .>>. digits 
    |> mapP resultToInt
    <?> label

// parse a float
let pfloat = 
    let label = "float" 

    // helper
    let resultToFloat (((sign,digits1),point),digits2) = 
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match sign with
        | Some ch -> -fl  // negate the float
        | None -> fl
            
    // define parser for one or more digits 
    let digits = manyChars1 digitChar 

    // a float is sign, digits, point, digits (ignore exponents for now)
    opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits 
    |> mapP resultToFloat
    <?> label
