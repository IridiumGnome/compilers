/*	Definition section */
%{
    	#include "common.h" //Extern variables that communicate with lex
    	// #define YYDEBUG 1
    	// int yydebug = 1;

   	extern int yylineno;
   	extern int yylex();
    	extern FILE *yyin;

    	void yyerror (char const *s)
    	{
        	printf("error:%d: %s\n", yylineno, s);
    	}


    	/* Symbol table function - you can add new function if needed. */
    	static void create_symbol(int);
    	static void insert_symbol(int, int);
    	static int lookup_symbol(char*, int);
    	static void dump_symbol(int);

	static char *helpFunction();
	static char *returnType();
	
	typedef struct
	{
		char name[10];
		char type[10];
		int address;
		int lineno;
		char elementType[10];
	}Table;
	
	Table *symbol_table[10][10];

	int scope_level = 0;
	char name[10];
	char type[10];
	int address = 0;
	char elementType[10];
	char convertTo = '\0';
	char convertFrom = '\0';
	int convert = 0, declared = 0, isIdent = 0;

%}

%define parse.error verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token ADD SUB MUL QUO REM INC DEC
%token GTR LSS GEQ LEQ EQL NEQ
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token AND OR NOT
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
%token PRINT RETURN CONTINUE BREAK
%token IF ELSE FOR WHILE 
%token INT FLOAT STRING BOOL VOID
%token COMMA SEMICOLON QUOTA
%token TRUE FALSE 

/* Token with return, which need to specify type */
%token <i_val> INT_LIT 
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT IDENT 

/* Nonterminal with return, which need to sepcify type */
%type <s_val> cmp_op 
%type <s_val> add_op 
%type <s_val> mul_op 
%type <s_val> unary_op 
%type <s_val> not_op
%type <s_val> type 
%type <s_val> boolean 
%type <s_val> literal 
%type <s_val> ident2
%type <s_val> expression 
%type <s_val> cmpr_expr 
%type <s_val> and_expr 
%type <s_val> add_expr 
%type <s_val> mul_expr 
%type <s_val> una_expr 
%type <s_val> primary
%type <s_val> incdec_expr

/* Yacc will start at this nonterminal */
%start program


/* Grammar section */
%%

program
    	: table statements				{ dump_symbol(scope_level); }			
;

table
	:						{ create_symbol(scope_level); }
;

statements
    	: statements statement
    	| statement
;


statement
    	: decl_stmt
    	| assign_stmt
    	| expression SEMICOLON
    	| print_stmt
    	| if_stmt
	| while_stmt
    	| for_stmt
;

type
    	: INT						{ strcpy(type, "int"); if(convertTo == 'a') convertTo = 'I'; }
    	| FLOAT						{ strcpy(type, "float"); if(convertTo == 'a') convertTo = 'F'; }
    	| STRING					{ strcpy(type, "string"); }
    	| BOOL						{ strcpy(type, "bool"); }
	| VOID						{ strcpy(type, "void"); }
;

literal
    	: INT_LIT 					{ printf("INT_LIT %d\n", $<i_val>$); $$ = "int"; 
							if(convertTo){
								convertFrom = 'I'; 
								printf("%c to %c\n", convertFrom, convertTo);
								convertTo = '\0';
							}
							}
    	| FLOAT_LIT 					{ printf("FLOAT_LIT %f\n", $<f_val>$); $$ = "float"; 
							if(convertTo){
								convertFrom = 'F'; 
								printf("%c to %c\n", convertFrom, convertTo);
								convertTo = '\0';
							}
							}
    	| QUOTA string QUOTA 				{ $$ = "string"; }
;

string
	: STRING_LIT					{ printf("STRING_LIT %s\n", $<s_val>$); }
;

boolean
    	: TRUE 						{ printf("TRUE\n"); $$ = "bool"; }
    	| FALSE 					{ printf("FALSE\n"); $$ = "bool"; }
;

decl_stmt
    	: type ident SEMICOLON				{ insert_symbol(scope_level, 0); }
    	| type ident ASSIGN expression SEMICOLON	{ insert_symbol(scope_level, 0); } 
    	| type ident LBRACK expression RBRACK SEMICOLON { insert_symbol(scope_level, 1); }
;

ident
	: IDENT						{ strcpy(name, $<s_val>$); } 
;

assign_stmt
	: expression ASSIGN expression SEMICOLON	{ if(!convert && declared){
								if(strcmp($1, $3)){
									printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, $1, $3);
									declared = 0;
								}
							}
							printf("ASSIGN\n"); 
							}
	| ident2 ADD_ASSIGN expression SEMICOLON	{ printf("ADD_ASSIGN\n"); }
	| ident2 SUB_ASSIGN expression SEMICOLON	{ printf("SUB_ASSIGN\n"); }
	| ident2 MUL_ASSIGN expression SEMICOLON	{ printf("MUL_ASSIGN\n"); }
	| ident2 QUO_ASSIGN expression SEMICOLON	{ printf("QUO_ASSIGN\n"); }
	| ident2 REM_ASSIGN expression SEMICOLON	{ printf("REM_ASSIGN\n"); }
	| literal ADD_ASSIGN expression SEMICOLON	{ printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("ADD_ASSIGN\n"); }
	| literal SUB_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("SUB_ASSIGN\n"); }
	| literal MUL_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("MUL_ASSIGN\n"); }
	| literal QUO_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("QUO_ASSIGN\n"); }
	| literal REM_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("REM_ASSIGN\n"); }
;

expression
    	: expression OR and_expr 			{ if(!convert){if(strcmp($1, "bool"))
								printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $1);
							else if(strcmp($3, "bool"))
                                                                if(!convert) printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $3);}
							printf("OR\n"); $$ = $3; 
							}
    	| and_expr 					{ $$ = $1; }
;

and_expr
    	: and_expr AND cmpr_expr 			{ if(!convert){if(strcmp($1, "bool"))
                                                                printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $1);
                                                        else if(strcmp($3, "bool"))
                                                                if(!convert) printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $3);
							}printf("AND\n"); $$ = $3; 
							}
    	| cmpr_expr 					{ $$ = $1; }
;

cmpr_expr
    	: cmpr_expr cmp_op add_expr 			{ printf("%s\n", $2); $$ = "bool"; }
    	| add_expr 					{ $$ = $1; }
;

cmp_op
	: EQL						{ $$ = "EQL"; }
	| NEQ						{ $$ = "NEQ"; }
	| GTR						{ $$ = "GTR"; }
	| LSS						{ $$ = "LSS"; }
	| GEQ						{ $$ = "GEQ"; }
	| LEQ						{ $$ = "LEQ"; }
;

add_expr
    	: add_expr add_op mul_expr 			{ if(strcmp($1, $3))
                                                                if(!convert) printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, $2, $1, $3);
							printf("%s\n", $2); $$ = $1; 
							}
    	| mul_expr 					{ $$ = $1; }
;

add_op
    	: ADD 						{ $$ = "ADD"; }
    	| SUB 						{ $$ = "SUB"; }
;

mul_expr
    	: mul_expr mul_op una_expr 			{ if(!convert){ if(strcmp($1, $3)){
                                                                if(strcmp($2, "REM"))
									printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, $2, $1, $3);
								else if(!strcmp($2, "REM") && strcmp($1, "FLOAT") && strcmp($3, "FLOAT"))
									printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno);
							}}
							printf("%s\n", $2); $$ = $1; 
							}
    	| una_expr 					{$$ = $1; }
;

mul_op
    	: MUL	 					{ $$ = "MUL"; }
    	| QUO 						{ $$ = "QUO"; }
    	| REM 						{ $$ = "REM"; }
;

una_expr
    	: primary 					{ $$ = $1; }
    	| unary_op primary 				{ printf("%s\n", $1); $$ = $2; }
    	| not_op primary 				{ printf("%s\n", $1); $$ = $2; }
    	| primary incdec_expr 				{ printf("%s\n", $2); $$ = $1; }
;

primary
    	: ident2 					{ $$ = $1; }
    	| literal 					{ $$ = $1; }
    	| boolean 					{ $$ = $1; }
    	| LPAREN expression RPAREN 			{ $$ = $2; }
    	| ident2 LBRACK expression RBRACK 		{ $$ = $1; }
    	| convert_stmt type end_convert ident2 		{ $$ = $2; convert = 1; }
    	| convert_stmt type end_convert literal 	{ $$ = $2; convert = 1; }
;

convert_stmt
	: LPAREN					{ convertTo = 'a'; }
;

end_convert
	: RPAREN					{ convert = 0; }	
;

ident2
	: IDENT						{ int addressTmp = lookup_symbol($<s_val>$, scope_level);
							if(addressTmp < 0) { printf("error:%d: undefined: %s\n", yylineno, $<s_val>$); declared = 0; }
							else if(addressTmp >= 0){
								printf("IDENT (name=%s, address=%d)\n", $<s_val>$, addressTmp);
								declared = 1;
                                                        	$$ = returnType($<s_val>$, scope_level); 
								if(convertTo){
									if(!strcmp($$, "int")) convertFrom = 'I';
									else if(!strcmp($$, "float")) convertFrom = 'F';
									printf("%c to %c\n", convertFrom, convertTo);
									convertTo = '\0';	
								}
							}
							}
;

incdec_expr
    	: INC 						{ $$ = "INC"; }
    	| DEC 						{ $$ = "DEC"; }
;

unary_op
    	: ADD						{ $$ = "POS"; }
	| SUB						{ $$ = "NEG"; }
;

not_op
    	: not_op NOT 					{ strcat($1, "\nNOT"); $$ = $1; }   
    	| NOT 						{ $$ = helpFunction("NOT"); }
;

print_stmt
    	: PRINT LPAREN expression RPAREN SEMICOLON 	{ printf("PRINT %s\n", $3); }
;

if_stmt
    	: IF LPAREN condition RPAREN start_scope statements end_scope
    	| IF LPAREN condition RPAREN start_scope statements end_scope else_stmt
;

else_stmt
    	: ELSE IF LPAREN condition RPAREN start_scope statements end_scope else_stmt
	| ELSE IF LPAREN condition RPAREN start_scope statements end_scope
    	| ELSE start_scope statements end_scope
;

while_stmt
    	: WHILE LPAREN condition RPAREN start_scope statements end_scope
;

condition
	: expression					{ if(!convert)	
								if(strcmp($1, "bool"))
									printf("error:%d: non-bool (type %s) used as for condition\n", yylineno+1, $1);
							}
;

for_stmt
    	: FOR LPAREN assign_stmt expression SEMICOLON expression RPAREN start_scope statements end_scope
;

start_scope
	: LBRACE					{ create_symbol(++scope_level); }
;

end_scope
	: RBRACE					{ dump_symbol(scope_level); scope_level--; }
;



%%

/* C code section */
static void create_symbol(int level)
{
	//printf("Table level %d created\n", level);
	for(int i = 0; i < 10; i++){
		symbol_table[i][level] = malloc(sizeof(Table));
		strcpy(symbol_table[i][level]->name, "");
                strcpy(symbol_table[i][level]->type, "");
                symbol_table[i][level]->address = -1;
                symbol_table[i][level]->lineno = -1;
                strcpy(symbol_table[i][level]->elementType, "");
      	}
}

static void insert_symbol(int level, int arr)
{
    	for(int j = 0; j < 10; j++){
      		if(strcmp(symbol_table[j][level]->name, "")){
          		if(!strcmp(symbol_table[j][level]->name, name)){
       				printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, name, symbol_table[j][level]->lineno);
				return;
			}
		}
	}

	printf("> Insert {%s} into symbol table (scope level: %d)\n", name, level);

	for(int i = 0; i < 10; i++){
		if(!strcmp(symbol_table[i][level]->name, "")){
			strcpy(symbol_table[i][level]->name, name);
			if(arr == 0) strcpy(symbol_table[i][level]->type, type);
			else strcpy(symbol_table[i][level]->type, "array");
			symbol_table[i][level]->address = address;
			symbol_table[i][level]->lineno = yylineno;
			if(arr == 0) strcpy(symbol_table[i][level]->elementType, "-");
			else strcpy(symbol_table[i][level]->elementType, type);
			break;
		}
	}

	address++;
}

static int lookup_symbol(char *id, int level)
{
	for(int i = level; i >= 0; i--)
		for(int j = 0; j < 10; j++)
			if(strcmp(symbol_table[j][i]->name, ""))
				if(!strcmp(symbol_table[j][i]->name, id))
					return symbol_table[j][i]->address;
	return -1;
}

static void dump_symbol(int level)
{
	printf("> Dump symbol table (scope level: %d)\n", level);
	printf("%-10s%-10s%-10s%-10s%-10s%s\n", "Index", "Name", "Type", "Address", "Lineno", "Element type");
	for(int i = 0; i < 10; i++){
		if(!strcmp(symbol_table[i][level]->name, "")) break;
		else{
			printf("%-10d%-10s%-10s%-10d%-10d%s\n", i, symbol_table[i][level]->name, symbol_table[i][level]->type, symbol_table[i][level]->address, symbol_table[i][level]->lineno, symbol_table[i][level]->elementType);
		}
	}
	
	for(int i = 0; i < 10; i++)
		free(symbol_table[i][level]);
}

char* returnType(char *id, int level)
{
	char *t = calloc(100, sizeof(char));
	
	for(int i = 0; i <= level; i++){
		for(int j = 0; j < 10; j++){
			if(strcmp(symbol_table[j][i]->name, "")){
				if(!strcmp(symbol_table[j][i]->name, id)){
					strcpy(t, symbol_table[j][i]->type);
					if(!strcmp(t, "array")) strcpy(t, symbol_table[j][i]->elementType);
					break;
				}
			}
		}
	}
	return t;
}

char *helpFunction(char *p)
{
    	char *t = calloc(100, sizeof(char));
    	strcpy(t, p);
    	return t;
}

int main(int argc, char *argv[])
{
	//yylineno = 0;
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yyparse();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}
