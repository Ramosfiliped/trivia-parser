// parser.mly

%{
  open Ast
%}

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
| x=LITINT                            { $loc, Ast.IntExp x }
| x=LITBOOL                           { $loc, Ast.BoolExp x}
| x=ID                                { $loc, Ast.VarExp x }
| x=ID a=ASSIGN y=exp                 { $loc, Ast.AssignExp (x, a, y)}
| x=exp op=operator y=exp             { $loc, Ast.OpExp (op, x, y) }
| IF x=exp THEN y=exp ELSE z=exp      { $loc, Ast.IfExp x, y, Some z }
| IF x=exp THEN y=exp                 { $loc, Ast.IfExp, x, y, None}
| WHILE x=exp DO y=exp                { $loc, Ast.WhileExp(x, y) }
| f=ID LPAREN a=exps RPAREN           { $loc, Ast.CallExp (f, a) }
| LET x=ID EQ i=exp IN b=exp          { $loc, Ast.LetExp (x, i, b) }
| LPAREN x=exps RPAREN                { $loc, Ast.SeqExp x}

%inline operator:
| PLUS   { Ast.Plus  }
| MINUS  { Ast.Minus }
| TIMES  { Ast.Times }
| DIV    { Ast.Div   }
| REST   { Ast.Rest  }
| EQUAL  { Ast.EQ    }
| NEQUAL { Ast.NE    }
| LT     { Ast.LT    }
| LE     { Ast.LE    }
| GT     { Ast.GT    }
| GE     { Ast.GE    }
| AND    { Ast.And   }
| OR     { Ast.Or    }

fundecs:
| l=nonempty_list(fundec) { l }

fundec:
| x=typeid LPAREN p=typeids RPAREN EQ b=exp { $loc, (x, p, b) }

symbol:
| x=ID { $loc, x }

typeid:
| INT x=symbol { (Ast.Int, x) }
| BOOL x=symbol { (Ast.Bool, x) }
| UNIT x=symbol { (Ast.Unit, x) }

typeids:
| x=separated_list(COMMA, typeid) { x }

exps:
| x=separated_list(SEMICOLON, exp) { x }

args:
  x=separated_list(COMMA, exp) { x }