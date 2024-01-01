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
    | block                                     # SBlock
    | anyType item ( ',' item )* ';'            # SDecl
    | value '=' expr ';'                        # SAss
    | value '++' ';'                            # SIncr
    | value '--' ';'                            # SDecr
    | 'return' expr ';'                         # SRetValue
    | 'return' ';'                              # SRetVoid
    | 'if' '(' expr ')' stmt                    # SCond
    | 'if' '(' expr ')' stmt 'else' stmt        # SCondElse
    | 'while' '(' expr ')' stmt                 # SWhile
    | 'for' '(' basicType ID ':' expr ')' stmt  # SFor
    | expr ';'                                  # SExp
    ;

value
    : 'self'              # VSelf
    | ID                  # VID
    | value '.' ID        # VMem
    | value '[' expr ']'  # VArr
    ;

anyType
    : basicType         # TBasic
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
    : ID ('=' expr)?
    ;

expr
    : unOp expr                              # EUnOp
    | expr mulOp expr                        # EMulOp
    | expr addOp expr                        # EAddOp
    | expr relOp expr                        # ERelOp
    | <assoc=right> expr '&&' expr           # EAnd
    | <assoc=right> expr '||' expr           # EOr
    | value                                  # EVal
    | INT                                    # EInt
    | 'true'                                 # ETrue
    | 'false'                                # EFalse
    | STR                                    # EStr
    | 'new' basicType                        # ENew
    | 'new' basicType '[' expr ']'           # ENewArr
    | '(' ID ')' 'null'                      # ENull
    | value '(' ( expr ( ',' expr )* )? ')'  # EFunCall
    | '(' expr ')'                           # EParen
    ;

unOp
    : '-'
    | '!'
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
    : '=='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
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