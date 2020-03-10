

%{
    open Ast



%}


%token <int> INT
%token <string> STR
%token LET REC EQUAL
%token ARROW PLUS MINUS MUL FUN
%token APP LPAREN RPAREN
%token SEMICOLON
%token DOUBLESEMICOLON DOUBLEEQUAL
%token EOF
%token UNIT
%token IF
%token THEN
%token ELSE
%token IN

%left DOUBLEEQUAL

%left SEMICOLON
%left ARROW
%left PLUS
%left MINUS
%left MUL

%left IN

%nonassoc LET
%nonassoc ELSE
%nonassoc FUN STR INT LPAREN IF
%nonassoc APP


%start <Ast.ast> main

%%



main:
| EOF { [] }
| stmt = definition m = main { stmt :: m }

definition:
| LET s = STR EQUAL e = expr DOUBLESEMICOLON {  Def (s,  e) }
| LET REC s = STR EQUAL e = expr DOUBLESEMICOLON { Recdef (s,  e) }
| LET s = STR s2 = STR EQUAL e = expr DOUBLESEMICOLON { Def (s,  Lambda(Str s2,e)) }
| LET s = STR UNIT EQUAL e = expr DOUBLESEMICOLON { Def (s,  Lambda(Unit,e)) }
| LET REC s = STR s2 = STR EQUAL e = expr DOUBLESEMICOLON { Recdef (s,  Lambda(Str s2,e)) }

expr:
| e1 = expr e2 = expr %prec APP
    { Apply (e1,e2) }
| i = INT
    { Value i }
| s = STR
    { Variable s }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr DOUBLEEQUAL e2 = expr
    { Binop(Eq,e1,e2) }
| e1 = expr PLUS e2 = expr
    { Binop(Add,e1,e2) }
| e1 = expr MINUS e2 = expr
    { Binop(Sub,e1,e2) }
| e1 = expr MUL e2 = expr
    { Binop(Mul,e1,e2) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr
    { If(e1,e2,e3) }
| e1 = expr SEMICOLON e2 = expr
                             { Seq(e1,e2) }
| FUN s = STR ARROW e2 = expr
    { Lambda(Str s, e2) }
| FUN UNIT ARROW e2 = expr
    { Lambda(Unit, e2) }
| LET s = STR EQUAL e1 = expr IN e2 = expr  { Let (s,e1,e2) }
