
// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid SimpleC program.

namespace compiler

module checker =
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
  let private matchToken expected_token tokens =
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

  //checks if a string literal begins with a string pattern
  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern)

  //searches the given table and returns the type of name variable, NONE if not found
  let rec searchType name (table : (string * string) list) =
    match table with
    | [] -> "NONE"
    |head::tail->
      let (called,realOrInt) = head
      if called = name then
        realOrInt
      else
        searchType name tail

  //extracts the name of the variable from the identifier:x format
  let getStringAfterColon (x: string) =
    let parts = x.Split([|':'|])
    match parts with
    | [| _; secondPart |] -> secondPart
    | _ -> ""

  // Extracts the value type of a tokenized expression.
  //<expr-value> part of BNF
  let private expr_value tokens =
    match List.head tokens with
    | id when beginswith "identifier:" id -> (List.tail tokens,"var")
    | il when beginswith "int_literal:" il -> (List.tail tokens,"int")
    | rl when beginswith "real_literal:" rl -> (List.tail tokens,"real")
    | sl when beginswith "str_literal:" sl -> (List.tail tokens,"str")
    | tf when tf = "true" || tf = "false" -> (List.tail tokens,"bool")
    | _ -> failwith ("expecting identifier or literal, but found " + (List.head tokens))

  // Analyzes expressions, performs type checking, and handles errors for arithmetic and logical operations.
  //<expr> part of BNF 
  let private expr tokens table =
    let firstOperand = getStringAfterColon (List.head tokens)
    let (t1,kind1) = expr_value tokens
    let head = List.head t1
    let t2 = List.tail t1
    //arithmetic operators
    if head = "+" || head = "-" || head = "*" || head = "/" || head = "^" then
      let secondOperand = getStringAfterColon (List.head t2)
      let (t3,kind2) = expr_value t2
      match kind1,kind2 with
      | "int","int" -> (t3,"int")
      | "str","str" -> failwith("operator " + head + " must involve 'int' or 'real'")
      | "real","real" -> (t3,"real")
      | "bool","bool" -> failwith("operator " + head + " must involve 'int' or 'real'")
      | "var","var" -> 
        let firstType = searchType firstOperand table
        let secondType = searchType secondOperand table
        if firstType = "bool" || firstType = "str" || secondType = "bool" || secondType = "str" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif  firstType = secondType then //MAYBE DIFFERENT ERROR MESSAGE HERE
          (t3,firstType)
        else
          failwith("operator " + head + " must involve 'int' or 'real'")
          //failwith("type mismatch '" + firstType + "' " + head + " '" + secondType + "'")
      | "var",x -> 
        let firstType = searchType firstOperand table
        if firstType = "str" || firstType = "bool" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif firstType = x then
          (t3,firstType)
        else
          failwith("operator " + head + " must involve 'int' or 'real'")
          //failwith("type mismatch '" + firstType + "' " + head + " '" + x + "'")
      | x,"var" -> 
        let secondType = searchType secondOperand table
        if secondType = "str" || secondType = "bool" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif secondType = x then
          (t3,secondType)
        else 
          failwith("operator " + head + " must involve 'int' or 'real'")
          //failwith("type mismatch '" + x + "' " + head + " '" + secondType + "'")
      | x,y -> 
        if(x = y) then
          (t3,x)
        else
          failwith("operator " + head + " must involve 'int' or 'real'")
          //failwith("type mismatch '" + x + "' " + head + " '" + y + "'")
    //logical operators
    elif head = "<" || head = "<=" || head = ">" || head = ">=" || head = "==" || head = "!=" then
      let secondOperand = getStringAfterColon (List.head t2)
      let (t3,kind2) = expr_value t2
      match kind1,kind2 with
      | "var","var"->
        let firstType = searchType firstOperand table
        let secondType = searchType secondOperand table
        if firstType = secondType then
          if head = "==" && firstType = "real" then
            printfn "warning: comparing real numbers with == may never be true"
            (t3,"bool")
          else
            (t3,"bool")
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + secondType + "'")
      | "var",x ->
        let firstType = searchType firstOperand table
        if firstType = x then
          if head = "==" && firstType = "real" then
            printfn "warning: comparing real numbers with == may never be true"
            (t3,"bool")
          else
            (t3,"bool")
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + x + "'")
      | x,"var" -> 
        let secondType = searchType secondOperand table
        if secondType = x then
          if head = "==" && secondType = "real" then
            printfn "warning: comparing real numbers with == may never be true"
            (t3,"bool")
          else
            (t3,"bool")
        else
          failwith("type mismatch '" + x + "' " + head + " '" + secondType + "'")
      | x,y -> 
        if(x = y) then
          if head = "==" && x = "real" then
            printfn "warning: comparing real numbers with == may never be true"
            (t3,"bool")
          else
            (t3,"bool")
        else
          failwith("type mismatch '" + x + "' " + head + " '" + y + "'")
    else
      (t1,kind1)
    
    //<output-value> part of BNF
  let private output_value tokens =
    if(List.head tokens = "endl") then
        (List.tail tokens,"NONE")
    else
        expr_value tokens
  //sorts the stmt part of the BNF into the correct categories, sending it to the if part, declaration part etc      
  //<stmt> part of BNF
  let rec private stmt tokens table= 
    match tokens with
    | [] -> failwith("empty")
    | ";"::tail -> tail
    | "int"::tail ->
        if beginswith "identifier:" (List.head tail) then
            matchToken ";" (List.tail tail)
        else
            failwith ("expecting identifier or literal, but found " + (List.head tail))
    | "real"::tail ->
        if beginswith "identifier:" (List.head tail) then
            matchToken ";" (List.tail tail)
        else
            failwith ("expecting identifier or literal, but found " + (List.head tail))
    | "cin"::tail ->
        let t1= matchToken ">>" tail
        if(beginswith "identifier:" (List.head t1)) then
            matchToken ";" (List.tail t1)
        else
            failwith ("expecting identifier, but found " + (List.head t1))
    | "cout"::tail ->
        let t2 = matchToken "<<" tail
        let (t3,DONTUSE) = output_value t2
        matchToken ";" t3
    | head::tail when beginswith "identifier:" head ->
        let name = getStringAfterColon head
        let idType = searchType name table
        let t3 = matchToken "=" tail
        let (t4,typeExpr) = expr t3 table
        if typeExpr = idType then
          matchToken ";" t4
        elif idType = "real" && typeExpr = "int" then
          matchToken ";" t4
        else
          failwith("cannot assign '" + typeExpr + "' to variable of type '" + idType + "'")
    | head::tail when head = "if" ->
        let t6 = matchToken "(" tail
        let (t7,typeExpr) = expr t6 table
        if typeExpr = "bool" then
          let t8 = matchToken ")" t7
          let t9 = stmt t8 table
          if(List.head t9 = "else") then
            let t10 = matchToken "else" t9
            stmt t10 table
          else
            t9
        else
          failwith("if condition must be 'bool', but found '" + typeExpr + "'")
    | _ -> failwith ("expecting statement, but found " + List.head tokens)

    //<morestmt> part of BNF
  let rec private morestmts tokens table=
    if List.head tokens = "}" then
        tokens
    else
        let t1 = stmt tokens table
        morestmts t1 table

    //<stmts> part of BNF
  let rec private stmts tokens table=
    let t1 = stmt tokens table
    morestmts t1 table

  //<simpleC> part of BNF
  let private simpleC tokens table = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6 table
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
    "success"

  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message



let block tokens= 
  let t1 = matchToken "{" tokens
  let t2 = stmts t1
  let t3 = matchToken "}" t2
  t3
