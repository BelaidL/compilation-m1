%{
  open HopixAST
  open Position
  (* version 1.2 *)

%}

%token VAL EQUAL CEQUAL EQUALRARROW UNDERSCORE
%token TYPE EXTERN FUN REF
%token COMMA COLON SEMICOLON LRARROW RLARROW
%token STAR PLUS MINUS SLASH AND OR LOWEREQUAL GREATEREQUAL LOWERTHAN  GREATERTHAN
%token LPAREN RPAREN  RBRACKET LBRACKET PIPE EXCLPOINT QUESTIONMARK
%token WHILE
%token EOF
%token<Int32.t> INT
%token<char> CHAR
%token<bool> BOOL
%token<string> STRING
%token<string> ID TYPECON TYPEVAR VARID CONSTRID

%start<HopixAST.t> program

%%

program: def=located(definition) * EOF
{
   def
}

definition:
| TYPE tc=located(type_constructeur) tp=loption(delimited(LBRACKET , separated_nonempty_list(COMMA, located(type_var)), RBRACKET)) EQUAL ? td =tdefinition
{
	DefineType(tc,tp,td)
}
| EXTERN id=located(identifier) COLON t=located(ttype)
{
	DeclareExtern(id,t)
}
| vdef=vdefinition
{
	vdef
}


tdefinition:
| s=sum_types+ 
{
	DefineSumType(s)
}


sum_types:
| x=located(constructor) s=sum_def?
{
	let extract x = match x with
	| Some x -> x
	| None -> []
		in x, (extract s)
}
| PIPE x=located(constructor) s=sum_def?
{
	let extract x = match x with 
	| Some x -> x
	| None -> []
	in x, (extract s)
}

sum_def:
| t=stype s=stp *
{
	t :: s
}
stype:
| LPAREN t=located(ttype) 
{
	t
}
stp:
| COMMA t=located(ttype) RPAREN
{
	t
}


vdefinition:
| VAL x=located(identifier) COLON t=located(ttype) EQUAL e=located(expression)
{
	DefineValue(x, e)
}
| FUN x = separated_list(AND, mdle_vdefinition)
{
	DefineRecFuns (x)
}


mdle_vdefinition : 
| var_id = located(identifier) fun_def = located(function_definition)
{
	(var_id,fun_def)	
}

function_definition : 
| ltp_var = loption(delimited(LBRACKET,separated_nonempty_list(COMMA, located(type_var)), RBRACKET))
   l_tp = delimited(LPAREN,separated_nonempty_list(COMMA, located(pattern)), RPAREN) tp = option(preceded(COLON, located(ttype)))
  EQUAL exp = located(expression)
{
	FunctionDefinition (ltp_var, l_tp, exp)
}

ttype:
| t=type_constructeur LPAREN s=located(ttype)* RPAREN
{
	TyCon(t,s)
}	
| LPAREN t=ttype RPAREN
{
	t
}
| s=ttype RLARROW ttype
{
	s
}
| e=type_var
{
	TyVar e
}


expression:
(** Literals *)
| e=located(literal)
{
	Literal e
}
(** Variables *)
| v=located(identifier)
{
	Variable v
}
(** Construction d'une donnee etiquete **)
| c=located(constructor) LBRACKET t=located(ttype)* RBRACKET LPAREN e=located(expression)* RPAREN
{
	Tagged(c,t,e)
}
(** Type Annotation *)
| LPAREN e=located(expression) COLON t=located(ttype) RPAREN 
{
	TypeAnnotation(e,t)
}
(** Sequence *)
| e1=expression SEMICOLON e2=expression
{
	e1; e2
}
| REF e=located(expression)
{
	Ref e
}
(** *)
| e=located(expression) CEQUAL ee=located(expression)
{
	Write (e,ee)
}
(** *)
| EXCLPOINT e=located(expression)
{
	Read e
}
(** *)
| WHILE e1=located(expression) LBRACKET e2=located(expression) RBRACKET
{
	While (e1,e2)
}
(** *)
| LPAREN e=expression RPAREN
{
	e
}
(** *)
| e=located(expression) QUESTIONMARK b=	branches
{
	Case (e,b)
}


branches:
| option(PIPE) l=separated_nonempty_list(PIPE, located(branch))
{
	l
}
| option(PIPE) l=separated_nonempty_list(PIPE, located(branch)) RBRACKET
{
	l
}

branch:
| p=located(pattern) EQUALRARROW e=located(expression)
{
	Branch(p,e)
}

pattern:
(** Etiquette *)
| c=located(constructor)
{
	PTaggedValue (c, [])
}
(**Variable id*)
| i=located(identifier)
{
	PVariable i
}
| c=located(constructor) LPAREN l=separated_nonempty_list(COMMA,located(pattern)) RPAREN 
{
  PTaggedValue (c,l)
}

| UNDERSCORE
{
	PWildcard
}
(** Parenthesis *)
| LPAREN p=pattern RPAREN
{
	p
}
(** Pattern with type*)
| p=located(pattern) COLON t=located(ttype)
{
	PTypeAnnotation (p,t)
}
(**Literal *)
| l=located(literal)
{
	PLiteral l
}
(**Valeur etiquette *)
| c=located(constructor) LPAREN l=separated_nonempty_list(COMMA,located(pattern)) RPAREN
{
	PTaggedValue (c,l)
}
(** Pattern OR *)
(*| plist = separated_nonempty_list(PIPE, located(pattern))
{
	POr plist
}*)
(** Pattern AND*)
(*| plist = separated_nonempty_list(AND, located(pattern))
{
	PAnd plist
}*)


%inline literal:
| i=INT
{
	LInt i
}
| c=CHAR
{
	LChar c
}
| s=STRING
{
	LString s
}	
| b=BOOL
{
	LBool b
}

%inline type_constructeur: ty=TYPECON
{
	TCon ty
}

%inline identifier: i=VARID
{
	Id i
}

%inline type_var: t=TYPEVAR
{
	TId t
}

%inline constructor: c=CONSTRID
{
	KId c
}

%inline located(X): x=X{
	Position.with_poss $startpos $endpos x
}
