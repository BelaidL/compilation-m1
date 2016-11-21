%{
  open HopixAST
  open Position
  (* version 1.2 *)

%}

%token VAL EQUAL CEQUAL EQUALRARROW UNDERSCORE
%token TYPE EXTERN FUN REF
%token COMMA COLON SEMICOLON LRARROW RLARROW
%token STAR PLUS MINUS SLASH AND OR LOWEREQUAL GREATEREQUAL LOWERTHAN  GREATERTHAN
%token LPAREN RPAREN  RBRACKET LBRACKET PIPE EXCLPOINT
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
| LPAREN s=sum_types+ RPAREN
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
(*TODO definition de fonction *)

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
| e=located(literal)
{
	Literal e
}
| v=located(identifier)
{
	Variable v
}
| c=located(constructor) LBRACKET t=located(ttype)* RBRACKET LPAREN e=located(expression)* RPAREN
{
	Tagged(c,t,e)
}
| LPAREN e=located(expression) COLON t=located(ttype) RPAREN 
{
	TypeAnnotation(e,t)
}
| REF e=located(expression)
{
	Ref e
}
| e=located(expression) CEQUAL ee=located(expression)
{
	Write (e,ee)
}
| EXCLPOINT e=located(expression)
{
	Read e
}
| WHILE e1=located(expression) LBRACKET e2=located(expression) RBRACKET
{
	While (e1,e2)
}
| LPAREN e=expression RPAREN
{
	e
}


branches:
| option(PIPE) l=separated_nonempty_list(PIPE, branch)
{
	l
}
| LBRACKET option(PIPE) l=separated_nonempty_list(PIPE, branch) RBRACKET
{
	l
}

branch:
| p=located(pattern) EQUALRARROW e=located(expression)
{
	Branch(p,e)
}

pattern:
| l=located(literal)
{
	PLiteral l
}
| i=located(identifier)
{
	PVariable i
}
| c=located(constructor)
{
	c
}
| c=located(constructor) LPAREN l=separated_nonempty_list(COMMA,located(pattern)) RPAREN
{
	PTaggedValue (c,l)
}
| LPAREN p=located(pattern) RPAREN
{
	p
}
| p=located(pattern) COLON t=located(ttype)
{
	PTypeAnnotation (p,t)
}
| UNDERSCORE
{
	PWildcard
}


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
