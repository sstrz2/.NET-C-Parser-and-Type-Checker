//
// Analyzer for SimpleC programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]

namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //
    let private matchToken expected_token tokens =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
        let next_token = List.head tokens

        if expected_token = next_token then  
            List.tail tokens
        else
            failwith ("expecting " + expected_token + ", but found " + next_token)

    //testing function to print out list
    let rec printList tokens = 
        match tokens with
        | [] -> printfn "listDone"
        | head::tail -> 
            printfn "%A" head
            printList tail

    let rec searchTable name (table : (string * string) list)= 
        match table with
        | [] -> false
        | head::tail ->
            let (called,realOrInt) = head
            if called = name then
                true
            else
                searchTable name tail
    
    let getStringAfterColon (x: string) =
        let parts = x.Split([|':'|])
        match parts with
        | [| _; secondPart |] -> secondPart
        | _ -> ""

    let beginswith (pattern: string) (literal: string) =
        literal.StartsWith (pattern)


    let rec solver tokens (table : (string * string) list) = 
        match List.head tokens with
        | id when beginswith "identifier" id -> //variable being used
            let name = getStringAfterColon id
            if searchTable name table then
                let t1 = List.tail tokens
                solver t1 table
            else
                failwith ("variable '" + name + "' undefined")
        | "real" -> //real being declared
            let t1 = List.tail tokens
            let identifier = List.head t1
            let name = getStringAfterColon identifier
            if searchTable name table then
                failwith ("redefinition of variable '" + name + "'")
            else
                let t2 = List.tail t1
                let tup = (name, "real")
                let newTable = tup :: table
                solver t2 newTable
        | "int" -> //int being delcared
            let t1 = List.tail tokens
            let identifier = List.head t1
            let name = getStringAfterColon identifier
            if searchTable name table then
                failwith ("redefinition of variable '" + name + "'")
            else
                let t2 = List.tail t1
                let tup = (name,"int")
                let newTable = tup :: table
                solver t2 newTable
            //int declared 
        | "$" -> ("success",table)
        | _ -> solver (List.tail tokens) table //not relevant token




    let simpleC tokens = 
        let emptyTable : (string * string) list = []
        solver tokens emptyTable



  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
    let build_symboltable tokens = 
        try
            let (T2, symboltable) = simpleC tokens
            (T2, symboltable)
        with 
        | ex -> ("semantic_error: " + ex.Message, [])
