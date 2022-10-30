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

%nonassoc EQUAL NEQUAL LT LE GT GE
%left PLUS MINUS
%left TIMES DIV

%start <lprogram>  program

%%

program:
  | s = nonempty_list(fundec) EOF { $loc, Program (s) };

exp:
  | a = LITINT { $loc, IntExp a }
  | a = LITBOOL { $loc, BoolExp a }
  | a = ID { $loc, VarExp a }
  | a = ID ASSIGN e1 = exp { $loc, AssignExp (a, e1) }
  | e1 = exp op = binop e2 = exp { $loc, OpExp (op, e1, e2) }
  | IF e1 = exp THEN e2 = exp ELSE e3 = expoption { $loc, IfExp (e1, e2, e3) }
  | WHILE e1 = exp DO e2 = exp { $loc, WhileExp (e1, e2) }
  | c = ID LPAREN d = explist RPAREN { $loc, CallExp (c, d)}
  | LET c = ID EQ e1 = exp IN e2 = exp { $loc, LetExp (c, e1, e2) }
  | LPAREN a = explist RPAREN { $loc, SeqExp a };

fundec:
  | a = typeid LPAREN b = typeidlist RPAREN EQ e = exp { $loc, (a, b, e) };

typeid:
  | INT  x = ID { Int, ($loc, x) }
  | BOOL x = ID { Bool, ($loc, x) }
  | UNIT x = ID { Unit, ($loc, x) };

typeidlist:
  | b = separated_nonempty_list(COMMA, typeid) { b };

explist:
  | b = separated_nonempty_list(SEMICOLON, exp) { b };

expoption:
 | b = option(e= exp ELSE { e }) { b }

%inline binop:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
  | DIV { Div }
  | REST { Rest }
  | EQUAL { EQ }
  | NEQUAL { NE }
  | LT { LT }
  | LE { LE }
  | GT { GT }
  | GE { GE }
  | AND { And }
  | OR { Or };