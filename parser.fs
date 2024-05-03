//
// Parser for SimpleC programs.  This component checks 
// the input program to see if it meets the syntax rules
// of SimpleC.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid SimpleC program.
//
// Seth Strzechowski
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
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


    let beginswith (pattern: string) (literal: string) =
        literal.StartsWith (pattern)

    
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
            | id when beginswith "identifier:" id -> List.tail tokens
            | il when beginswith "int_literal:" il -> List.tail tokens
            | rl when beginswith "real_literal:" rl -> List.tail tokens
            | sl when beginswith "str_literal:" sl -> List.tail tokens
            | tf when tf = "true" || tf = "false" -> List.tail tokens
            | _ -> failwith ("expecting identifier or literal, but found " + (List.head tokens))

    //<expr> part of BNF 
    let private expr tokens =
        let t1 = expr_value tokens
        let head = List.head t1
        if (head = "+" || head = "-" || head = "*" || head = "/" || head = "^" || head = "<" || head = "<=" || head = ">" || head = ">=" || head = "==" || head = "!=") then
            let t2 = expr_op t1
            expr_value t2
        else
            t1
        // if(head = ";" || head = ")" || beginswith "identifier:" head || beginswith "int_literal:" head || beginswith "real_literal" head || beginswith "str_literal:" head) then
        //     t1
        // else
        //     let t2 = expr_op t1
        //     expr_value t2
    
    //<output-value> part of BNF
    let private output_value tokens = 
        if(List.head tokens = "endl") then
            List.tail tokens
        else
            expr_value tokens
        
    //<stmt> part of BNF
    let rec private stmt tokens = 
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
            let t3 = output_value t2
            matchToken ";" t3
        | head::tail when beginswith "identifier:" head ->
            let t3 = matchToken "=" tail
            let t4 = expr t3
            matchToken ";" t4
        | head::tail when head = "if" ->
            let t6 = matchToken "(" tail
            let t7 = expr t6
            let t8 = matchToken ")" t7
            let t9 = stmt t8
            if(List.head t9 = "else") then
                let t10 = matchToken "else" t9
                stmt t10
            else
                t9
        | _ -> failwith ("expecting statement, but found " + List.head tokens)

    //<morestmt> part of BNF
    let rec private morestmts tokens =
        if List.head tokens = "}" then
            tokens
        else
            let t1 = stmt tokens
            morestmts t1

    //<stmts> part of BNF
    let rec private stmts tokens =
        let t1 = stmt tokens
        morestmts t1

    //<simpleC> part of BNF
    let private simpleC tokens = 
        let T2 = matchToken "void" tokens
        let T3 = matchToken "main" T2
        let T4 = matchToken "(" T3
        let T5 = matchToken ")" T4
        let T6 = matchToken "{" T5
        let T7 = stmts T6
        let T8 = matchToken "}" T7
        let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
        T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid SimpleC program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
    let parse tokens = 
        try
            let result = simpleC tokens
            "Success!"
        with 
            | ex -> "syntax_error: " + ex.Message
