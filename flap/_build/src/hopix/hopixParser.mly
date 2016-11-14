%{
  open Position
  open HopixAST

(*TODO : solve error with located expressions?????*)
(*TODO : find how to set optional and list parameters *)

%}
%token VAL
%token EQUAL
%token TYPE
%token EOF
%token<Int32.t> INT
%token<char> CHAR
%token<string> STRING
%token<string> ID

%start<HopixAST.t> program

%%

program: def=definition * EOF
{
   def
}

definition:
| TYPE t=ID
{
	
}
|TYPE t=ID EQUAL td=tdefinition
{
	
}
| vd=vdefinition
{
	vd
}

tdefinition:
constr_id=ID
{
	
}


vdefinition:
VAL x=ID EQUAL e=expression
{
  
}


expression:
x=INT
{

}
|x=CHAR
{

}
|x=STRING
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
