%{
  open HopixAST
  open Position

  (* version 1.7 *)

%}
  
%token VAL TYPE EXTERN FUN REF AND
%token LPAREN RPAREN  RBRACKET LBRACKET LRARROW PIPE EQUAL
%token EXCLPOINT
%token COMMA COLON SEMICOLON AMPER UNDERSCORE
%token STAR SLASH MINUS PLUS
%token LOWEREQUAL LOWERTHAN GREATERTHAN GREATEREQUAL ORLOGIC ANDLOGIC
%token EOF
%token<string> INFIXID STRING TYPEVAR VARID CONSTRID
%token<char> CHAR 
%token<Int32.t> INT
%token<bool> BOOL

(*%right VAL TYPE RPAREN FUN EXTERN*) 
%right SEMICOLON
%right LRARROW REF
  
%left ORLOGIC
%left PIPE ANDLOGIC
%nonassoc COLON LOWERTHAN GREATERTHAN GREATEREQUAL LOWEREQUAL EQUAL
%left AMPER
%left INFIXID  
%left PLUS MINUS
%left STAR SLASH
%left EXCLPOINT  


%start<HopixAST.t> program

%%

program: def=located(definition) * EOF
{
   def
}

definition:
| TYPE x=located(type_con)
{
	DefineType( x, [], HopixAST.Abstract )
}
| TYPE tc=located(type_con) ltv=delimited(LPAREN,separated_nonempty_list(COMMA,located(type_variable)), RPAREN)
{
	DefineType( tc, ltv, HopixAST.Abstract )
}
| TYPE tc=located(type_con) EQUAL td=tdefinition
{
	DefineType(tc, [], td)
}
| TYPE tc=located(type_con) ltv=delimited(LPAREN,separated_nonempty_list(COMMA,located(type_variable)), RPAREN) EQUAL td=tdefinition
{
	DefineType(tc, ltv, td)
}
| EXTERN id=located(var_id) COLON t=located(ttype)
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
| VAL x=located(var_id) EQUAL e=located(expression)
{
	DefineValue(x,e)
}
(*| VAL x=located(var_id) COLON ttype EQUAL e=located(expression)
{
	(*Typecheck??????*)
	DefineValue(x, e)
}*)

| FUN x = separated_nonempty_list(AND, pair(located(var_id), vdeffun )) 
{
  DefineRecFuns (x)
}


vdeffun:
| x=type_variable_list LPAREN p_list=separated_nonempty_list(COMMA, located(pattern)) RPAREN l=option(preceded(COLON, located(ttype))) EQUAL e=located(expression) 
{
	match l with 
	| None -> Position.with_poss $startpos $endpos (FunctionDefinition( x, p_list, e))
	| Some a -> let ta=(Position.with_poss $startpos $endpos (TypeAnnotation(e,a))) in 
		Position.with_poss $startpos $endpos (FunctionDefinition( x, p_list, ta ))
}

type_variable_list:
| LBRACKET le= separated_nonempty_list(COMMA, located(type_variable)) RBRACKET
{
	le
}
|
{
	[]
}


val_def: 
| VAL v=located(var_id) COLON tip=located(ttype) ? EQUAL e=located(expression)
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
(**
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

**)

ttype:
| t=type_con  s=loption(delimited(LPAREN, separated_list(COMMA, located(ttype)), RPAREN))
{
	TyCon(t,s)
}	
| LPAREN t=ttype RPAREN
{
	t
}
| t1 = located(ttype) LRARROW t2 = located(ttype)
{
	TyCon ((TCon("->")),[t1;t2])
}
| e=type_variable
{
	TyVar e
}


expression:
(**Application *)
  e=located(simple_expression) tl=loption(delimited(LBRACKET, separated_nonempty_list(COMMA, located(ttype)), RBRACKET)) LPAREN el=separated_nonempty_list(COMMA, located(simple_expression)) RPAREN
{
	Apply(e, tl, el)
}
(** A local definition *)
| VAL x = located(var_id) EQUAL e1 = located(expression) SEMICOLON e2=located(expression)
{
	Define (x,e1,e2)
}
| v = val_def SEMICOLON e2=located(expression)
{
        let (x,e1) = v in Define (x,e1,e2)
}
| FUN x = separated_list(AND, pair(located(var_id), vdeffun)) SEMICOLON e2=located(expression)
{
        DefineRec (x,e2)
}
(** Construction d'une donnee etiquete **)
| c=located(constructor) e1=loption(delimited(LBRACKET, separated_nonempty_list(COMMA, located(ttype)), RBRACKET)) e2=loption(delimited(LPAREN,separated_nonempty_list(COMMA, located(expression)), RPAREN)) 
{
	Tagged(c,e1,e2)
}
(** Reference  *)
| REF e=located(expression)
{
	Ref e
}
(** Type Annotation *)
| LPAREN e=located(expression) COLON t=located(ttype) RPAREN 
{
	TypeAnnotation(e,t)
}
| EXCLPOINT e=located(expression)
{
	Read e
}

(* SHIFT REDUCE CONFLICT
| WHILE e1=located(expression) LBRACKET e2=located(expression) RBRACKET
{
	While (e1,e2)
}
*)
(** Binoperation*)
| e1=located(expression) b_op=located(binop) e2=located(expression)
{
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b_op))) b_op in
  let app1 = Position.with_poss $startpos(e1) $endpos(b_op) (Apply (op,[],[e1]))
  in Apply (app1,[],[e2])
}
| e=simple_expression
{
	e
}

simple_expression:

(** Literals *)
| e=located(literal)
{
	Literal e
}
(** Variables *)
| v=located(var_id)
{
	Variable v
}
(** Parenthesis *)
| LPAREN e=expression RPAREN
{
	e
}



pattern:
(** Littéraux *)
| l=located(literal)
{
  PLiteral (l)
}
(** Motif universel liant *)
| i=located(var_id)
{
  PVariable (i)
}
(** Etiquette *)
| c=located(constructor)
{
  PTaggedValue(c,[])
}
(** Valeurs étiquetées *)
| c=located(constructor) LPAREN l=separated_nonempty_list(COMMA,located(pattern)) RPAREN
{
  PTaggedValue(c,l)
}
(** Parenthésage *)
| LPAREN p=pattern RPAREN
{
  p
}
(** Annotation de type *)
| p=located(pattern) COLON t=located(ttype)
{
  PTypeAnnotation(p,t)
}
(** Motif unversel non liant *)
| UNDERSCORE
{
  PWildcard
}
| lp=located(pattern) PIPE rp=located(pattern)
{
  POr (lp::rp::[])
}
| lp=located(pattern) AMPER rp=located(pattern)
{
  PAnd (lp::rp::[])
}


%inline literal:
| n=INT
{
	LInt  n
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
| ORLOGIC        { "`||" }
| ANDLOGIC       { "`&&" }



%inline type_con: ty = VARID
{
	match String.get ty 0 with
	| '`' -> failwith "expects constructor"
	| _ -> TCon ty
}

%inline var_id: i=VARID
{
	Id i
}

%inline type_variable: t=TYPEVAR
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

