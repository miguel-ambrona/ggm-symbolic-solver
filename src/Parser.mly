%{
  open Util
  open Abbrevs
  open Expressions
  open Equations
  open Eval
%}

%token EOF
%token DOT
%token EQ
%token DEF
%token INEQ
%token LAND
%token PLUS
%token MINUS
%token STAR
%token DIV
%token EXP
%token TRANSPOSE
%token DIAG
%token LBRACK
%token RBRACK
%token LCURLY
%token RCURLY
%token BACKSLASH
%token LPAR
%token RPAR
%token COMMA
%token SEMICOLON
%token COLON
%token FORALL
%token EXISTS
%token SUM
%token PROD
%token UNDERSCORE
%token IN
%token SETS
%token VARS
%token PARAMS
%token ZP
%token GOAL
%token SUBSTITUTE
%token BY
%token SIMPLIFY
%token FULLSIMPLIFY
%token CONTRADICTION
%token EXTRACTCOEFF
%token GO
%token ADDEQUATION
%token CIRC
%token ZO

%token <int> INT
%token <string> NAME


/************************************************************************/
/* Priority & associativity */

/* Multiplication has the highest precedence. */
%left PLUS MINUS
%left STAR DIV
%left CIRC

/************************************************************************/
/* Production types */

%type <Eval.instr list> cmds_t

/************************************************************************/
/* Start productions */

%start cmds_t

%%

/************************************************************************/
/* Types */

ivar :
| idx = NAME   { idx }

name :
| s = NAME     { s }

nat :
| i = INT      { NatInt(i) }
| s = name     { NatPar(s) }

dim :
| EXP; n = nat;  { (n, NatInt(1)) }
| EXP; LPAR; m = nat; COMMA; n = nat; RPAR;  { (m,n) }

atom :
| s = name; UNDERSCORE; i = ivar; { Matrix(s,Param, [(i,("undefined",NatInt(0)))],scalar_dim,Zp) }
| s = name; UNDERSCORE; LCURLY; ids = separated_list(COMMA,ivar); RCURLY;
   { Matrix(s,Param, L.map ids ~f:(fun i -> (i,("undefined",NatInt(1)))), scalar_dim,Zp) }
| s = name;                       { Matrix(s,Param,[],scalar_dim,Zp) }
| i = INT;                        { Int(BI.of_int i) }
                                    
id_exceptions :
| i = ivar; IN; s = name;                                { (i,s,[]) }
| i = ivar; IN; s = name; BACKSLASH;
  LCURLY; ids = separated_list(COMMA,ivar); RCURLY;      { (i,s,ids) }

group_type :
| ZO  { Bool }
| ZP  { Zp }

expr :
| a = atom;                                              { Atom(a) }
| a = atom; TRANSPOSE;                                   { Transpose(Atom(a)) }
| e1 = expr; PLUS; e2 = expr;                            { Add(e1,e2) }
| e1 = expr; STAR; e2 = expr;                            { Mul(e1,e2) }
| e1 = expr; DIV; e2 = expr;                             { Div(e1,e2) }
| e1 = expr; CIRC; e2 = expr;                            { PWProd(e1,e2) }
| MINUS; e = expr;                                       { Opp(e) }
| e1 = expr; MINUS; e2 = expr;                           { Add(e1,Opp(e2)) }
| SUM;  LPAR; i = id_exceptions; COLON; e = expr; RPAR;  { mk_sum i e }
| PROD; LPAR; i = id_exceptions; COLON; e = expr; RPAR;  { mk_prod i e }
| LPAR; e = expr; RPAR; TRANSPOSE;                       { Transpose(e) }
| DIAG; LPAR; e = expr; RPAR;                            { Diag(e) }
| LPAR; e = expr; RPAR;                                  { e }

is_eq :
| EQ   { Eq }
| INEQ  { InEq }

forall :
| FORALL; ids = separated_list(SEMICOLON,id_exceptions); COLON;   { ids }

equation :
| q = forall? e1 = expr; eq = is_eq; e2 = expr              { mk_equation q e1 eq e2 }

exists :
| EXISTS; ids = separated_list(SEMICOLON,id_exceptions); COLON; { ids }

conj :
| e = exists? eqs = separated_list(LAND,equation)           { mk_conjunction e eqs }
                                  
atoms_def :
| l = separated_list(COMMA, atom); IN; t = group_type; d = dim?         { (l,d,t) }

id_no_exceptions :
| i = ivar; IN; s = name;                              { (i,s) }
                                                           
quant :
| FORALL; ids = separated_list(SEMICOLON, id_no_exceptions); COLON;  { ids }

set :
| s = name; LBRACK; n = nat; RBRACK;      { (s,n) }

simple_quant:
| FORALL; i = ivar; IN; s = name;   { (i,s) }

cmd :
| SETS; sets = separated_list(SEMICOLON, set);  DOT;                       { EvalSets(sets) }
| VARS; q = quant?; vars = separated_list(SEMICOLON, atoms_def); DOT;      { EvalAtom(vars,q,true) }
| PARAMS; q = quant?; params = separated_list(SEMICOLON, atoms_def); DOT;  { EvalAtom(params,q,false) }
| GOAL; i = INT; DOT;                                                      { Goal(i) }
| c = conj; DOT;                                                           { EvalConj(c) };
| SUBSTITUTE; a = atom; BY; e = expr; IN; n = INT; DOT;                    { Substitute(a,e,n); }
| SIMPLIFY; DOT;                                                           { Simplify; }
| FULLSIMPLIFY; DOT;                                                       { FullSimplify; }
| CONTRADICTION; DOT;                                                      { Contradiction; }
| EXTRACTCOEFF; a = atom; IN; n = INT; i = simple_quant? DOT;              { ExtractCoeff(a,1,n,i); }
| EXTRACTCOEFF; a = atom; EXP; d = INT IN; n = INT; i = simple_quant? DOT; { ExtractCoeff(a,d,n,i); }
| GO; DOT;                                                                 { Go; }
| ADDEQUATION; e = equation; DOT;                                          { AddEquation(e); }
                                                                 
cmds_t : cs = list(cmd); EOF; { cs };
