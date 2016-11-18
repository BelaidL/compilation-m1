%{

  open HopixAST
  open Position

(*TODO : solve error with located expressions?????*)
(*TODO : find how to set optional and list parameters *)

%}
%token VAL
%token EQUAL EXTERN FUN
%token TYPE TYPECON TYPEVAR VARID CONSTRID
%token COMMA COLON SEMICOLON LRARROW RLARROW LPAREN RPAREN RBRACKET LBRACKET PIPE
%token EOF
%token<Int32.t> INT
%token<string> ID

%start<HopixAST.t> program

%%

program: def=definition * EOF
{
   def
}

definition:
| TYPE tc=located(TYPECON) tp=loption(separated_list(COMMA, located(TYPEVAR))) td=option(preceded(EQUAL,located(tdefinition)))
{
	DefineType(tc,tp,td)
}
| EXTERN vd=located(VARID) COLON ttype
{

}
| vd=located(vdefinition)
{
  vd
}



tdefinition:
option(PIPE) constr_id=located(CONSTRID) loption(delimited(LPAREN, separated_nonempty_list(COMMA, ttype), RPAREN)) option(tdefinition)
{

}

vdefinition:
VAL vid=located(VARID) option(preceded(COLON, ttype)) (*EQUAL e=located(expression)*)
{

}
| FUN vid=located(VARID) ltv=loption(delimited(LBRACKET, separated_nonempty_list(COMMA, located(TYPEVAR)), RBRACKET)) 
	LPAREN p=separated_nonempty_list(COMMA, located(pattern)) (*TOBECONTINUED*) 
{
	
}




ttype:
| tc=located(TYPECON) loption(separated_list(COMMA,ttype))
{

}
| ttype LRARROW ttype
{

}
| tv=located(TYPEVAR)
{

}
| LPAREN ttype RPAREN
{

}

(* | var_id  *)
(* | constr_id  *)
(* | (expression:type) *)
(* | expression{; expression } *)
(* | vdefinition; expression *)
(* | expression \[[type_variable {, type_variable }]\] (pattern {, pattern}) => expression *)
(* | expression binop expression *)
(* | expression ? branches *)
(* | if expression then expression {elif expression} [else expression] *)
(* | ref expression *)
(* | expression := expression *)
(* | ! expression *)
(* | while expression {expression} *)
(* | (expression) *)

pattern: 
| x=located(CONSTRID)
{

}

%inline located(X): x=X{
	Position.with_poss $startpos $endpos x
}
