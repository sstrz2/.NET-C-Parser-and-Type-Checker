
// Analyzer for SimpleC programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid SimpleC program.
//
// Modified by:
//  Seth Strzechowski>
//
// Original author:
//   Prof. Joe Hummel
//   University of Illinois Chicago
//   CS 341, Spring 2022
// Modified by:
//   Ellen Kidane
//   University of Illinois Chicago
//   CS 341, Spring 2024
//

namespace compile

module checker =
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

  let beginswith (pattern: string) (literal: string) =
    literal.StartsWith (pattern)
  
  let rec searchTable name (table : (string * string) list )=
     match table with
     | [] -> false
     | head::tail -> let (called,realOrInt) = head
                     if called = name then
                      true
                     else
                      searchTable name tail
  
  let rec searchType name (table : (string * string) list) =
    match table with
    | [] -> "NONE"
    |head::tail->
      let (called,realOrInt) = head
      if called = name then
        realOrInt
      else
        searchType name tail
    
  let getStringAfterColon (x: string) =
    let parts = x.Split([|':'|])
    match parts with
    | [| _; secondPart |] -> secondPart
    | _ -> ""

    //<expr-op> part of BNF
  let private expr_op tokens = 
    match List.head tokens with
    | "+" -> List.tail tokens
    | "-" -> List.tail tokens
    | "*" -> List.tail tokens
    | "/" -> List.tail tokens
    | "^" -> List.tail tokens
    | "<" -> List.tail tokens
    | "<=" -> List.tail tokens
    | ">" -> List.tail tokens
    | ">=" -> List.tail tokens
    | "==" -> List.tail tokens
    | "!=" -> List.tail tokens
    | _ -> failwith ("expecting expression operator, but found " + List.head tokens)

    //<expr-value> part of BNF
  let private expr_value tokens =
    match List.head tokens with
    | id when beginswith "identifier:" id -> (List.tail tokens,"var")
    | il when beginswith "int_literal:" il -> (List.tail tokens,"int")
    | rl when beginswith "real_literal:" rl -> (List.tail tokens,"real")
    | sl when beginswith "str_literal:" sl -> (List.tail tokens,"str")
    | tf when tf = "true" || tf = "false" -> (List.tail tokens,"bool")
    | _ -> failwith ("expecting identifier or literal, but found " + (List.head tokens))

    //<expr> part of BNF 
  let private expr tokens table =
    let firstOperand = getStringAfterColon (List.head tokens)
    let (t1,kind1) = expr_value tokens
    let head = List.head t1
    let t2 = List.tail t1
    let secondOperand = getStringAfterColon (List.head t2)
    let (t3,kind2) = expr_value t2
    if head = "+" || head = "-" || head = "*" || head = "/" || head = "^" then
      match kind1,kind2 with
      | "int","int" -> t3
      | "str","str" -> failwith("operator " + head + " must involve 'int' or 'real'")
      | "real","real" -> t3
      | "bool","bool" -> failwith("operator " + head + " must involve 'int' or 'real'")
      | "var","var" -> 
        let firstType = searchType firstOperand table
        let secondType = searchType secondOperand table
        // if firstType = "NONE" || secondType = "NONE" then
        if firstType = "bool" || firstType = "str" || secondType = "bool" || secondType = "str" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif  firstType = secondType then //MAYBE DIFFERENT ERROR MESSAGE HERE
          t3
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + secondType + "'")
      | "var",x -> 
        let firstType = searchType firstOperand table
        if firstType = "str" || firstType = "bool" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif firstType = x then
          t3
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + x + "'")
      | x,"var" -> 
        let secondType = searchType secondOperand table
        if secondType = "str" || secondType = "bool" then
          failwith("operator " + head + " must involve 'int' or 'real'")
        elif secondType = x then
          t3
        else
          failwith("type mismatch '" + x + "' " + head + " '" + secondType + "'")
      | x,y -> 
        if(x = y) then
          t3
        else
          failwith("type mismatch '" + x + "' " + head + " '" + y + "'")
    elif head = "<" || head = "<=" || head = ">" || head = ">=" || head = "==" || head = "!=" then
      match kind1,kind2 with
      | "var","var"->
        let firstType = searchType firstOperand table
        let secondType = searchType secondOperand table
        if firstType = secondType then
          if head = "==" then
            printfn "warning: comparing real numbers with == may never be true"
            t3
          else
            t3
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + secondType + "'")
      | "var",x ->
        let firstType = searchType firstOperand table
        if firstType = x then
          t3
        else
          failwith("type mismatch '" + firstType + "' " + head + " '" + x + "'")
      | x,"var" -> 
        let secondType = searchType secondOperand table
        if secondType = x then
          t3
        else
          failwith("type mismatch '" + x + "' " + head + " '" + secondType + "'")
      | x,y -> 
        if(x = y) then
          t3
        else
          failwith("type mismatch '" + x + "' " + head + " '" + y + "'")
    else
        t1
    
    //<output-value> part of BNF
  let private output_value tokens = 
    if(List.head tokens = "endl") then
        (List.tail tokens,"NONE")
    else
        expr_value tokens
        
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
        let t3 = matchToken "=" tail
        let t4 = expr t3 table
        matchToken ";" t4
    | head::tail when head = "if" ->
        let t6 = matchToken "(" tail
        let t7 = expr t6 table
        let t8 = matchToken ")" t7
        let t9 = stmt t8 table
        if(List.head t9 = "else") then
            let t10 = matchToken "else" t9
            stmt t10 table
        else
            t9
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

