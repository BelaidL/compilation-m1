%{
  open HopixAST
  open Position

  (* version 1.7 *)

%}

%token VAL EQUAL CEQUAL EQUALRARROW UNDERSCORE
%token TYPE EXTERN FUN REF
%token COMMA COLON SEMICOLON LRARROW RLARROW 
%token STAR PLUS MINUS SLASH AND OR LOWEREQUAL GREATEREQUAL LOWERTHAN  GREATERTHAN ANTISLASH
%token LPAREN RPAREN  RBRACKET LBRACKET PIPE EXCLPOINT QUESTIONMARK
%token WHILE
%token EOF
%token<Int32.t> INT
%token<char> CHAR
%token<bool> BOOL
%token<string> STRING
%token<string> ID TYPECON TYPEVAR VARID CONSTRID INFIXID


%right SEMICOLON
%left THEN
%left ELIF
%left ELSE
%left LRARROW 

%left PIPE
%left ESPER
%nonassoc DOUBLEDOTEQUAL
%left DOUBLEOR
%left DOUBLEAND
%nonassoc EQUAL
%nonassoc LOWEREQUAL GREATEREQUAL LOWERTHAN GREATERTHAN
%left ALIENINFIXID
%left DOUBLEDOT
%right RIGHTARROW

%left PLUS MINUS
%left MULTI DIVIDE
%right REF
%nonassoc EXCLPOINT
%nonassoc QUESTIONMARK

%start<HopixAST.t> program

%%

program: def=located(definition) * EOF
{
   def
}

definition:
| TYPE x=located(type_constructeur)
{
	DefineType( x, [], HopixAST.Abstract )
}
| TYPE tc=located(type_constructeur) ltv=delimited(LPAREN,separated_nonempty_list(COMMA,located(type_var)), RPAREN)
{
	DefineType( tc, ltv, HopixAST.Abstract )
}
| TYPE tc=located(type_constructeur) EQUAL td=tdefinition
{
	DefineType(tc, [], td)
}
| TYPE tc=located(type_constructeur) ltv=delimited(LPAREN,separated_nonempty_list(COMMA,located(type_var)), RPAREN) EQUAL td=tdefinition
{
	DefineType(tc, ltv, td)
}
| EXTERN id=located(identifier) COLON t=located(ttype)
{
	DeclareExtern(id,t)
}
| vdef=vdefinition
{
	vdef
}


vdefinition:
(** A toplevel definition for a value. *)
| v=val_def
{
	let x,e=v in
	DefineValue(x,e)
}
| VAL x=located(identifier) EQUAL e=located(expression)
{
	DefineValue(x,e)
}
| VAL x=located(identifier) COLON t=located(ttype) EQUAL e=located(expression)
{
	DefineValue(x, e)
}
| FUN x = separated_list(AND, pair(located(identifier), vdeffun ))
{
	DefineRecFuns (x)
}


vdeffun:
| x=var_fun LPAREN p_list=separated_nonempty_list(COMMA, located(pattern)) RPAREN l=option(preceded(COLON, located(ttype))) EQUAL e=located(expression)
{
	match l with 
	| None -> Position.with_poss $startpos $endpos (FunctionDefinition( x, p_list, e))
	| Some a -> let ta=(Position.with_poss $startpos $endpos (TypeAnnotation(e,a))) in 
		Position.with_poss $startpos $endpos (FunctionDefinition( x, p_list, ta ))
}

var_fun:
| LBRACKET le= separated_list(COMMA, located(type_var)) RBRACKET
{
	le
}
|
{
	[]
}


val_def: 
| VAL v=located(identifier) COLON tip=located(ttype) ? EQUAL e=located(expression)
{
	let extract source dest = Position.with_pos (Position.position source) dest 
	in 
	let register tx odt te = match odt with
	| None -> tx, te
	| Some t -> let ta = TypeAnnotation(te,t) in let fte = extract te ta in tx, ( fte )
	in register v tip e 
}


tdefinition:
| PIPE? td=separated_nonempty_list(PIPE, pair(located(constructor), loption(delimited(LPAREN, separated_nonempty_list(COMMA, located(ttype)), RPAREN ))))
{
	DefineSumType(td)
}
|
{
	Abstract
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
| e=simple_expression
{
	e
}
| e=complex_expression
{
	e
}


complex_expression:
(** A local definition *)
| VAL x=located(identifier) COLON t=located(ttype) EQUAL e1=located(expression) SEMICOLON e2=located(expression)
{
	Define (x,e1,e2)
}


simple_expression:
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
(** Parenthesis *)
| LPAREN e=expression RPAREN
{
	e
}
(**Application *)
(* | e=located(expression) tl=loption(delimited(LBRACKET, separated_nonempty_list(COMMA, located(ttype)), RBRACKET)) LPAREN el=separated_nonempty_list(COMMA, located(expression)) RPAREN
{
	Apply(e, tl, el)
}
*)


pattern:
(** Etiquette *)
| c=located(constructor)
{
	PTaggedValue (c, [])
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

%inline binop:
  x=INFIXID      { String.(sub x 0 (length x - 1)) }
| PLUS           { "`+"  }
| MINUS          { "`-"  }
| STAR           { "`*"  }
| SLASH          { "`/"  }
| GREATEREQUAL   { "`>=" }
| GREATERTHAN    { "`>"  }
| LOWERTHAN      { "`<"  }
| LOWEREQUAL     { "`<=" }
| EQUAL          { "`="  }
| OR             { "`||" }
| AND            { "`&&" }

%inline type_constructeur: ty=VARID
{
	match String.get ty 0 with
	| '`' -> failwith "expects constructor"
	| _ -> TCon ty
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
