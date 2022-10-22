// parser.mly

%token                 EOF
%token                 LPAREN
%token                 RPAREN
%token                 EQ
%token                 COMMA
%token                 SEMICOLON
%token                 ASSIGN
%token                 PLUS
%token                 MINUS
%token                 TIMES
%token                 DIV
%token                 REST
%token                 EQUAL
%token                 NEQUAL
%token                 LT
%token                 LE
%token                 GT
%token                 GE
%token                 AND
%token                 OR
%token                 IF
%token                 THEN
%token                 ELSE
%token                 WHILE
%token                 DO
%token                 LET
%token                 IN
%token                 INT
%token                 BOOL
%token                 UNIT
%token <bool>          LITBOOL
%token <int>           LITINT
%token <Symbol.symbol> ID

(* add precedence rules if needed *)

%start <Ast.lprogram>  program

%%

program:
  (* this rule should be updated *)
| x=LITINT EOF { $loc, Ast.Program x }

(* write the missing production rules *)

exp:
| x=LITINT { $loc, Absyn.IntExp x }
| x=ID { $loc, Absyn.VarExp x }
| x=exp op=operator y=exp { $loc, Absyn.OpExp (op, x, y) }
| IF t=exp THEN x=exp ELSE y=exp { $loc, Absyn.IfExp (t, x, y) }
| f=ID LPAREN a=exps RPAREN { $loc, Absyn.CallExp (f, a) }
| LET x=ID EQ i=exp IN b=exp { $loc, Absyn.LetExp (x, i, b) }

%inline operator:
| PLUS { Absyn.Plus }
| LT { Absyn.LT }

fundecs:
| l=nonempty_list(fundec) { l }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc, (x, p, b) }

symbol:
| x=ID { $loc, x }

typeid:
| INT x=symbol { (Absyn.Int, x) }
| BOOL x=symbol { (Absyn.Bool, x) }

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }

exps:
| x=separated_nonempty_list(COMMA, exp) { x }