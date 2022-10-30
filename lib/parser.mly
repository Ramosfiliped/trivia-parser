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

%left OR
%left AND
%nonassoc EQUAL NEQUAL LT LE GT GE
%left PLUS MINUS
%left TIMES DIV REST

%start <Ast.lprogram>  program

%%

program:
  (* this rule should be updated *)
| x=fns EOF { $loc, Ast.Program x }
;
(* write the missing production rules *)


fns:
| fs = nonempty_list(fn) {fs}
;

fn:
| f1=tpid LPAREN f2=tpids RPAREN EQ e=exp {$loc, (f1, f2, e)}
;

id:
| x = ID {$loc, x}
;

tpid:
| INT xi=id { (Ast.Int, xi) }
| BOOL xb=id { (Ast.Bool, xb) }
| UNIT xu=id { (Ast.Unit, xu) }
;

tpids:
| ti=separated_list(COMMA, tpid) { ti }
;

exp:
| e=LITINT {$loc, Ast.IntExp e}
| e=LITBOOL {$loc, Ast.BoolExp e}
| e=ID {$loc, Ast.VarExp e}
| x=ID ASSIGN e=exp {$loc, Ast.AssignExp (x, e)}
| a=exp bo=binop b=exp {$loc, Ast.OpExp(bo,a,b)}
| IF a=exp THEN b=exp ELSE c=exp {$loc, Ast.IfExp(a, b, Some c) }
| IF a=exp THEN b=exp { $loc, Ast.IfExp(a, b, None) }
| WHILE a=exp DO b=exp { $loc, Ast.WhileExp (a, b) }
| x=ID LPAREN ar=argss RPAREN { $loc, Ast.CallExp (x, ar) }
| LET x=ID EQ a=exp IN b=exp { $loc, Ast.LetExp (x, a, b) }
| LPAREN es=expss RPAREN { $loc, Ast.SeqExp es }
;

argss:
| ar = separated_list(COMMA, exp) {ar}

expss:
| es = separated_list(SEMICOLON, exp) {es}
;

%inline binop:
| PLUS { Ast.Plus }
| MINUS { Ast.Minus }
| TIMES { Ast.Times }
| DIV { Ast.Div }
| REST { Ast.Rest }
| EQUAL { Ast.EQ }
| NEQUAL { Ast.NE }
| LT { Ast.LT }
| LE { Ast.LE }
| GT { Ast.GT }
| GE { Ast.GE }
| AND { Ast.And }
| OR { Ast.Or }
;