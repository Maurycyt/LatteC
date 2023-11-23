grammar Latte;

program
    : topDef+
    ;

topDef
    : functionDef # DFun
    | classDef    # DClass
    ;

classDef
    : 'class' ID ('extends' ID)? '{' memberDef* '}'
    ;

memberDef
    : ';'              # MEmpty
    | memberVariables  # MVar
    | memberFunction   # MFun
    ;

memberVariables
    : anyType ID ( ',' ID )* ';'
    ;

memberFunction
    : functionDef
    ;

functionDef
    : anyType ID '(' args? ')' block
    ;

args
    : anyType ID ( ',' anyType ID )*
    ;

block
    : '{' stmt* '}'
    ;

stmt
    : ';'                                       # SEmpty
    | block                                     # BlockStmt
    | anyType item ( ',' item )* ';'            # Decl
    | value '=' expr ';'                        # Ass
    | value '++' ';'                            # Incr
    | value '--' ';'                            # Decr
    | 'return' expr ';'                         # Ret
    | 'return' ';'                              # VRet
    | 'if' '(' expr ')' stmt                    # Cond
    | 'if' '(' expr ')' stmt 'else' stmt        # CondElse
    | 'while' '(' expr ')' stmt                 # While
    | 'for' '(' basicType ID ':' expr ')' stmt  # For
    | expr ';'                                  # SExp
    ;

value
    : ID                  # VID
    | value '.' ID        # VMem
    | value '[' expr ']'  # VArr
    ;

anyType
    : basicType         # TBase
    | basicType '[' ']' # TArr
    ;

basicType
    : 'int'     # TInt
    | 'string'  # TStr
    | 'boolean' # TBool
    | 'void'    # TVoid
    | ID        # TClass
    ;

item
    : ID
    | ID '=' expr
    ;

expr
    : ('-'|'!') expr                         # EUnOp
    | expr mulOp expr                        # EMulOp
    | expr addOp expr                        # EAddOp
    | expr relOp expr                        # ERelOp
    | <assoc=right> expr '&&' expr           # EAnd
    | <assoc=right> expr '||' expr           # EOr
    | value                                  # EId
    | INT                                    # EInt
    | 'true'                                 # ETrue
    | 'false'                                # EFalse
    | 'new' basicType                        # ENew
    | 'new' basicType '[' expr ']'           # ENewArr
    | '(' ID ')' 'null'                      # ENull
    | value '(' ( expr ( ',' expr )* )? ')'  # EFunCall
    | STR                                    # EStr
    | '(' expr ')'                           # EParen
    ;

addOp
    : '+'
    | '-'
    ;

mulOp
    : '*'
    | '/'
    | '%'
    ;

relOp
    : '<'
    | '<='
    | '>'
    | '>='
    | '=='
    | '!='
    ;

COMMENT : ('#' ~[\r\n]* | '//' ~[\r\n]*) -> channel(HIDDEN);
MULTICOMMENT : '/*' .*? '*/' -> channel(HIDDEN);

fragment Letter  : Capital | Small ;
fragment Capital : [A-Z\u00C0-\u00D6\u00D8-\u00DE] ;
fragment Small   : [a-z\u00DF-\u00F6\u00F8-\u00FF] ;
fragment Digit : [0-9] ;

INT : Digit+ ;
fragment ID_First : Letter | '_';
ID : ID_First (ID_First | Digit)* ;

WS : (' ' | '\r' | '\t' | '\n')+ ->  skip;

STR
    :   '"' StringCharacters? '"'
    ;
fragment StringCharacters
    :   StringCharacter+
    ;
fragment
StringCharacter
    :   ~["\\]
    |   '\\' [tnr"\\]
    ;