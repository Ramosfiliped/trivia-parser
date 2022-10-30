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
%left TIMES DIV REST
%left PLUS MINUS
%left AND
%left OR

%start <Ast.lprogram>  program

%%

program:
| x=fundecs EOF { $loc, Ast.Program x }

exp:
| x=LITINT                       { $loc, Absyn.IntExp x }
| x=LITBOOL                      { $loc, Absyn.BoolExp x}
| x=ID                           { $loc, Absyn.VarExp x }
| x=ID a=ASSIGN y=exp            { $loc, Absyn.AssignExp (x, a, y)}
| x=exp op=operator y=exp        { $loc, Absyn.OpExp (op, x, y) }
| IF t=exp THEN x=exp ELSE y=exp { $loc, Absyn.IfExp (t, x, y) }
| IF t=exp THEN x=exp            { $loc, Absyn.IfExp (t, x) }
| WHILE x=exp DO y=exp           { $loc, Absyn.WhileExp(x, y) }
| f=ID LPAREN a=exps RPAREN      { $loc, Absyn.CallExp (f, a) }
| LET x=ID EQ i=exp IN b=exp     { $loc, Absyn.LetExp (x, i, b) }
| LPAREN x=exp RPAREN            { $loc, Absyn.SeqExp x}

%inline operator:
| PLUS   { Absyn.Plus  }
| MINUS  { Absyn.Minus }
| TIMES  { Absyn.Times }
| DIV    { Absyn.Div   }
| REST   { Absyn.Rest  }
| EQUAL  { Absyn.EQ    }
| NEQUAL { Absyn.NE    }
| LT     { Absyn.LT    }
| LE     { Absyn.LE    }
| GT     { Absyn.GT    }
| GE     { Absyn.GE    }
| AND    { Absyn.And   }
| OR     { Absyn.OR    }

fundecs:
| l=nonempty_list(fundec) { l }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc, (x, p, b) }

symbol:
| x=ID { $loc, x }

typeid:
| INT x=symbol { (Absyn.Int, x) }
| BOOL x=symbol { (Absyn.Bool, x) }
| UNIT x=symbol { (Absyn.Unit, x)}

typeids:
| x=separated_nonempty_list(COMMA, typeid) { x }

exps:
| x=separated_nonempty_list(COMMA, exp) { x }

args:
| x=exp y=argss