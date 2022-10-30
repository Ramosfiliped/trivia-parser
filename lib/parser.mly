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

%left OR
%left AND
%nonassoc EQUAL NEQUAL LT LE GT GE
%left PLUS MINUS
%left TIMES DIV REST

%start <Ast.lprogram>  program

%%

program:
| x=fundecs EOF { $loc, Ast.Program x }

exp:
| x=LITINT                       { $loc, Astl.IntExp x }
| x=LITBOOL                      { $loc, Astl.BoolExp x}
| x=ID                           { $loc, Astl.VarExp x }
| x=ID a=ASSIGN y=exp            { $loc, Astl.AssignExp (x, a, y)}
| x=exp op=operator y=exp        { $loc, Astl.OpExp (op, x, y) }
| IF t=exp THEN x=exp ELSE y=exp { $loc, Astl.IfExp (t, x, y) }
| IF t=exp THEN x=exp            { $loc, Astl.IfExp (t, x) }
| WHILE x=exp DO y=exp           { $loc, Astl.WhileExp(x, y) }
| f=ID LPAREN a=exps RPAREN      { $loc, Astl.CallExp (f, a) }
| LET x=ID EQ i=exp IN b=exp     { $loc, Astl.LetExp (x, i, b) }
| LPAREN x=exp RPAREN            { $loc, Astl.SeqExp x}

%inline operator:
| PLUS   { Astl.Plus  }
| MINUS  { Astl.Minus }
| TIMES  { Astl.Times }
| DIV    { Astl.Div   }
| REST   { Astl.Rest  }
| EQUAL  { Astl.EQ    }
| NEQUAL { Astl.NE    }
| LT     { Astl.LT    }
| LE     { Astl.LE    }
| GT     { Astl.GT    }
| GE     { Astl.GE    }
| AND    { Astl.And   }
| OR     { Astl.OR    }

fundecs:
| l=nonempty_list(fundec) { l }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc, (x, p, b) }

symbol:
| x=ID { $loc, x }

typeid:
| INT x=symbol { (Astl.Int, x) }
| BOOL x=symbol { (Astl.Bool, x) }
| UNIT x=symbol { (Astl.Unit, x) }

typeids:
| x=separated_list(COMMA, typeid) { x }

exps:
| x=separated_list(SEMICOLON, exp) { x }

args:
  x=separated_list(COMMA, exp) { x }