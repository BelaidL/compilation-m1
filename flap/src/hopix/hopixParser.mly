%{
  open HopixAST
  open Position


%}
%token VAL
%token EQUAL EXTERN FUN
%token<string> TYPE TYPECON TYPEVAR VARID CONSTRID
%token COMMA COLON SEMICOLON LRARROW RLARROW LPAREN RPAREN RBRACKET LBRACKET PIPE
%token EOF
%token<Int32.t> INT
%token<string> ID

%start<HopixAST.t> program

%%

program: def=located(definition) * EOF
{
   def
}

definition:
| TYPE tc=located(type_constructeur) tp=loption(separated_list(COMMA, located(type_var))) td=option(preceded(EQUAL,located(tdefinition)))
{
	DefineType(tc,tp,td)
}(**
| EXTERN vd=located(VARID) COLON ttype
{

}
|TYPE t=located(ID) EQUAL td=located(tdefinition)
{

}
| vd=located(vdefinition)
{
  vd
}
*)

type_constructeur:
| tc = TYPECON
{
TCon tc
}

type_var : 
| tv = TYPEVAR
{
TId tv
}

ttdef: 
| option(PIPE) constr_id=located(CONSTRID) ttp = loption(delimited(LPAREN, separated_nonempty_list(COMMA, ttype), RPAREN)) td=option(preceded(PIPE,ttdef))
{
	[(constr_id,ttp)] @ td
}

tdefinition: 
	td=ttdef
{
 
 DefineSumType  td 
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
| tv = type_var
{
 	TyVar (tv)
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
