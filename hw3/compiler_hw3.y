/*	Definition section */
%{
    	#include "common.h" //Extern variables that communicate with lex
    	// #define YYDEBUG 1
    	// int yydebug = 1;

	#define codegen(...) \
        do { \
            for (int i = 0; i < INDENT; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

   	extern int yylineno;
   	extern int yylex();
    	extern FILE *yyin;

	/* Other global variables */
    	FILE *fout = NULL;
    	bool HAS_ERROR = false;
    	int INDENT = 0;

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
	static char *returnType2();
	static void printShit(char*, char*);
	static void printShit2(char*);

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

	int flag_type = 0; // 1 is int, 2 is float, 3 is string, 4 is bool
	int counter = 0;
	int address2 = 0;
	int address3 = 0;
	int address4 = 0;
	int notCount = 0;
	int whileCount = -1;
	int whileBoundary = -1;
	int newWhileCount = -1;
	bool isInc = false;
	int mode = 0; // 1 is for while, 2 is for for, 3 is for if

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
%type <s_val> ident3
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
	| while_stmt					{ if(whileCount == whileBoundary) { whileCount= newWhileCount+1; whileBoundary=newWhileCount+1; }
							else if(newWhileCount < whileCount) newWhileCount = whileCount;
				 			}
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
    	: INT_LIT 					{ printf("INT_LIT %d\n", $<i_val>$); 
							fprintf(fout, "ldc %d\n", $<i_val>$);
							$$ = "int"; 
							flag_type = 1;
							if(convertTo){
								convertFrom = 'I'; 
								printf("%c to %c\n", convertFrom, convertTo);
								convertTo = '\0';
								fprintf(fout, "i2f\n");
							}
							}
    	| FLOAT_LIT 					{ printf("FLOAT_LIT %f\n", $<f_val>$); 
							fprintf(fout, "ldc %f\n", $<f_val>$);
							$$ = "float"; 
							flag_type = 2;
							if(convertTo){
								convertFrom = 'F'; 
								printf("%c to %c\n", convertFrom, convertTo);
								convertTo = '\0';
								fprintf(fout, "f2i\n");
							}
							}
    	| QUOTA string QUOTA 				{ $$ = "string"; }
;

string
	: STRING_LIT					{ fprintf(fout, "ldc \"%s\"\n", $<s_val>$); printf("STRING_LIT %s\n", $<s_val>$); }
;

boolean
    	: TRUE 						{ printf("TRUE\n"); fprintf(fout, "iconst_1\n"); $$ = "bool"; }
    	| FALSE 					{ printf("FALSE\n"); fprintf(fout, "iconst_0\n"); $$ = "bool"; }
;

decl_stmt
    	: type ident SEMICOLON						{ insert_symbol(scope_level, 0); 
									if(!strcmp(type, "int")){ 
										fprintf(fout, "ldc 0\n");
										fprintf(fout, "istore %d\n", address2);
									}
									else if(!strcmp(type, "float")){ 
										fprintf(fout, "ldc 0.0\n");
										fprintf(fout, "fstore %d\n", address2);
									}
									else if(!strcmp(type, "string")){
										fprintf(fout, "ldc \"\"\n");
										fprintf(fout, "astore %d\n", address2);
									}
									else if(!strcmp(type, "bool")){
										fprintf(fout, "ldc 0\n");
										fprintf(fout, "istore %d\n", address2);
									}
									}
    	| type ident ASSIGN expression SEMICOLON			{ insert_symbol(scope_level, 0); 
									if(!strcmp(type, "int")) fprintf(fout, "istore %d\n", address2);
                                                                        else if(!strcmp(type, "float")) fprintf(fout, "fstore %d\n", address2);
                                                                        else if(!strcmp(type, "string")) fprintf(fout, "astore %d\n", address2);
                                                                        else if(!strcmp(type, "bool")) fprintf(fout, "istore %d\n", address2);
									} 
    	| type ident LBRACK expression RBRACK SEMICOLON 		{ insert_symbol(scope_level, 1); 
									if(!strcmp(type, "int")){
                                                                		fprintf(fout, "newarray int\n");
                                                                        	fprintf(fout, "astore %d\n", address2);
                                                                	}
                                                                	else if(!strcmp(type, "float")){ 
                                                                		fprintf(fout, "newarray float\n");
                                                                        	fprintf(fout, "astore %d\n", address2);
                                                                	}}
;

ident
	: IDENT						{ strcpy(name, $<s_val>$); } 
;

ident3
	: IDENT						{int addressTmp = lookup_symbol($<s_val>$, scope_level);
                                                        address2 = addressTmp;
                                                        if(addressTmp < 0) {
                                                                printf("error:%d: undefined: %s\n", yylineno, $<s_val>$);
                                                                declared = 0;
                                                                HAS_ERROR = true;
                                                        }
                                                        else if(addressTmp >= 0){
                                                                printf("IDENT (name=%s, address=%d)\n", $<s_val>$, addressTmp);
                                                                declared = 1;
                                                                $$ = returnType2($<s_val>$, scope_level);
								
                                                                if(convertTo){
                                                                        if(!strcmp($$, "int")) { convertFrom = 'I'; fprintf(fout, "i2f\n"); }
                                                                        else if(!strcmp($$, "float")) { convertFrom = 'F'; fprintf(fout, "f2i\n"); }
                                                                        printf("%c to %c\n", convertFrom, convertTo);
                                                                        convertTo = '\0';
                                                                }
                                                        }
                                                        }

;

assign_stmt
	: ident3 ASSIGN expression SEMICOLON		{ if(!convert && declared){
								if(strcmp($1, $3)){
									HAS_ERROR = true;
									printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, $1, $3);
									declared = 0;
								}
							}
							printf("ASSIGN\n");
							//printShit2($1);
							if(!strcmp($1, "int")) fprintf(fout, "istore %d\n", address2);
							else if(!strcmp($1, "float")) fprintf(fout, "fstore %d\n", address2); 
							else if(!strcmp($1, "string")) fprintf(fout, "astore %d\n", address2);
							else if(!strcmp($1, "bool")) fprintf(fout, "istore %d\n", address2);
							}
	| ident2 LBRACK expression RBRACK ASSIGN expression SEMICOLON	{ if(!convert && declared){
                                                                		if(strcmp($1, $6)){
                                                                        		HAS_ERROR = true;
                                                                        		printf("error:%d: invalid operation: ASSIGN (mismatched types %s and %s)\n", yylineno, $1, $3);
                                                                        		declared = 0;
                                                                		}
                                                        		}
                                                        		printf("ASSIGN\n");
                                                       			if(!strcmp($1, "int")) fprintf(fout, "iastore\n");
                                                                	else if(!strcmp($1, "float")) fprintf(fout, "fastore\n");
                                                                	else if(!strcmp($1, "string")) fprintf(fout, "astore\n");
                							}

	| ident2 ADD_ASSIGN expression SEMICOLON	{ printf("ADD_ASSIGN\n");
							if(!strcmp($1, "int")){ 
								fprintf(fout, "iadd\n");
								fprintf(fout, "istore %d\n", address2);
							}
							else if(!strcmp($1, "float")){
								fprintf(fout, "fadd\n");
                                                                fprintf(fout, "fstore %d\n", address2);
							}}
	| ident2 SUB_ASSIGN expression SEMICOLON	{ printf("SUB_ASSIGN\n"); 
							if(!strcmp($1, "int")){
								fprintf(fout, "isub\n");
								fprintf(fout, "istore %d\n", address2);
							}
                                                        else if(!strcmp($1, "float")){
								fprintf(fout, "fsub\n");
								fprintf(fout, "fstore %d\n", address2); 
							}}
	| ident2 MUL_ASSIGN expression SEMICOLON	{ printf("MUL_ASSIGN\n"); 
							if(!strcmp($1, "int")){
								fprintf(fout, "imul\n");
								fprintf(fout, "istore %d\n", address2);
							}
                                                        else if(!strcmp($1, "float")){
	 							fprintf(fout, "fmul\n"); 
								fprintf(fout, "fstore %d\n", address2);
							}}
	| ident2 QUO_ASSIGN expression SEMICOLON	{ printf("QUO_ASSIGN\n"); 
							if(!strcmp($1, "int")){
				 				fprintf(fout, "idiv\n");
								fprintf(fout, "istore %d\n", address2);
							}
                                                        else if(!strcmp($1, "float")){
		 						fprintf(fout, "fdiv\n"); 
								fprintf(fout, "fstore %d\n", address2);
							}}
	| ident2 REM_ASSIGN expression SEMICOLON	{ printf("REM_ASSIGN\n"); 
							if(!strcmp($1, "int")){
 								fprintf(fout, "irem\n");
								fprintf(fout, "istore %d\n", address2);
							}
                                                        else if(!strcmp($1, "float")){
	 							fprintf(fout, "frem\n"); 
								fprintf(fout, "fstore %d\n", address2);
							}}
	| literal ADD_ASSIGN expression SEMICOLON	{ printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("ADD_ASSIGN\n"); HAS_ERROR = true; }
	| literal SUB_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("SUB_ASSIGN\n"); HAS_ERROR = true; }
	| literal MUL_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("MUL_ASSIGN\n"); HAS_ERROR = true; }
	| literal QUO_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("QUO_ASSIGN\n"); HAS_ERROR = true; }
	| literal REM_ASSIGN expression SEMICOLON       { printf("error:%d: cannot assign to %s\n", yylineno, $1); printf("REM_ASSIGN\n"); HAS_ERROR = true; }
;

expression
    	: expression OR and_expr 			{ if(!convert){
								if(strcmp($1, "bool")){
									printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $1);
									HAS_ERROR = true;
								}
								else if(strcmp($3, "bool")){
                                                                	if(!convert){ 
										HAS_ERROR = true;
										printf("error:%d: invalid operation: (operator OR not defined on %s)\n", yylineno, $3);
									}
							}}
							printf("OR\n"); $$ = $3; 
							fprintf(fout, "ior\n");
							}
    	| and_expr 					{ $$ = $1; }
;

and_expr
    	: and_expr AND cmpr_expr 			{ if(!convert){if(strcmp($1, "bool")){
								HAS_ERROR = true;
                                                                printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $1);}
                                                        else if(strcmp($3, "bool")){
								if(!convert){
									HAS_ERROR = true; 
									printf("error:%d: invalid operation: (operator AND not defined on %s)\n", yylineno, $3);
							}}
							}printf("AND\n"); $$ = $3; 
							fprintf(fout, "iand\n");
							}
    	| cmpr_expr 					{ $$ = $1; }
;

cmpr_expr
    	: cmpr_expr cmp_op add_expr 			{ printf("%s\n", $2); $$ = "bool"; printShit($2, $1); }
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
    	: add_expr add_op mul_expr 			{ if(strcmp($1, $3)){
                                                                if(!convert){ 
									HAS_ERROR = true;
									printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, $2, $1, $3);
								}
							}
							printf("%s\n", $2); $$ = $1;
						 	printShit($2, $1);
							}
    	| mul_expr 					{ $$ = $1; }
;

add_op
    	: ADD 						{ $$ = "ADD"; }
    	| SUB 						{ $$ = "SUB"; }
;

mul_expr
    	: mul_expr mul_op una_expr 			{ if(!convert){ 
								if(strcmp($1, $3)){
                                                                	if(strcmp($2, "REM")){
										HAS_ERROR = true;
										printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno, $2, $1, $3);
									}
									else if(!strcmp($2, "REM") && strcmp($1, "FLOAT") && strcmp($3, "FLOAT")){
										HAS_ERROR = true;
										printf("error:%d: invalid operation: (operator REM not defined on float)\n", yylineno);
									}
							}}
							printf("%s\n", $2); $$ = $1; 
							printShit($2, $1);
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
    	| unary_op primary 				{ printf("%s\n", $1); $$ = $2; printShit($1, $2); }
    	| not_op primary 				{ printf("%s\n", $1); $$ = $2;  
							for(int i = 0; i < notCount; i++) fprintf(fout, "ixor\n"); }
    	| primary incdec_expr 				{ printf("%s\n", $2); $$ = $1; }
;

primary
    	: ident2 					{ $$ = $1; }
    	| literal 					{ $$ = $1; }
    	| boolean 					{ $$ = $1; }
    	| LPAREN expression RPAREN 			{ $$ = $2; }
    	| ident2 LBRACK expression RBRACK 		{ $$ = $1; if(!strcmp($1, "int")) fprintf(fout, "iaload\n"); 
							else if(!strcmp($1, "float")) fprintf(fout, "faload\n"); }
    	| convert_stmt type end_convert ident2 		{ $$ = $2; convert = 1; }
    	| convert_stmt type end_convert literal 	{ $$ = $2; convert = 1; }
	| convert_stmt type end_convert ident2 LBRACK literal RBRACK	{ $$ = $2; convert = 1; }
;

convert_stmt
	: LPAREN					{ convertTo = 'a'; }
;

end_convert
	: RPAREN					{ convert = 0; }	
;

ident2
	: IDENT						{ int addressTmp = lookup_symbol($<s_val>$, scope_level);
							if(addressTmp < 0) { 
								printf("error:%d: undefined: %s\n", yylineno, $<s_val>$); 
								declared = 0; 
								HAS_ERROR = true;
							}
							else if(addressTmp >= 0){
								printf("IDENT (name=%s, address=%d)\n", $<s_val>$, addressTmp);
								declared = 1;
                                                        	$$ = returnType($<s_val>$, scope_level); 
								if(convertTo){
									if(!strcmp($$, "int")) { convertFrom = 'I'; fprintf(fout, "i2f\n"); }
									else if(!strcmp($$, "float")) { convertFrom = 'F'; fprintf(fout, "f2i\n"); }
									printf("%c to %c\n", convertFrom, convertTo);
									convertTo = '\0';	
								}
							}}
;

incdec_expr
    	: INC 						{ $$ = "INC"; 
							if(flag_type == 1){
								fprintf(fout, "ldc 1\n");
								fprintf(fout, "iadd\n");
								fprintf(fout, "istore %d\n", address3);
							}
							else if(flag_type == 2){
                                                                fprintf(fout, "ldc 1.0\n");
                                                                fprintf(fout, "fadd\n");
                                                                fprintf(fout, "fstore %d\n", address3);
							}
							}
    	| DEC 						{ $$ = "DEC"; 
							if(flag_type == 1){
                                                                fprintf(fout, "ldc 1\n");
                                                                fprintf(fout, "isub\n");
                                                                fprintf(fout, "istore %d\n", address3);
                                                        }
                                                        else if(flag_type == 2){
                                                                fprintf(fout, "ldc 1.0\n");
                                                                fprintf(fout, "fsub\n");
                                                                fprintf(fout, "fstore %d\n", address3);
                                                        } 
							}
;

unary_op
    	: ADD						{ $$ = "POS"; }
	| SUB						{ $$ = "NEG"; }
;

not_op
    	: not_op NOT 					{ strcat($1, "\nNOT"); $$ = $1; fprintf(fout, "iconst_1\n"); notCount++; }   
    	| NOT 						{ $$ = helpFunction("NOT"); fprintf(fout, "iconst_1\n"); }
;

print_stmt
    	: PRINT LPAREN expression RPAREN SEMICOLON 	{ printf("PRINT %s\n", $3); printShit2($3); }
;

if_stmt
    	: IF LPAREN condition RPAREN start_scope statements end_scope	
	| IF LPAREN condition RPAREN start_scope statements end_scope else_stmt
;

else_stmt
    	: ELSE LPAREN condition RPAREN start_scope statements end_scope 
	| ELSE LPAREN condition RPAREN start_scope statements end_scope else_stmt						
    	| ELSE start_scope statements end_scope 	
;

while_stmt
    	: WHILE { fprintf(fout, "WhileLoop_begin_%d:\n", ++whileCount); mode = 1; } LPAREN condition RPAREN start_scope statements { mode = 1; } end_scope
;

condition
	: expression					{ if(!convert){	
								if(strcmp($1, "bool")){
									printf("error:%d: non-bool (type %s) used as for condition\n", yylineno+1, $1);
									HAS_ERROR = true;
								}
							}}
;

for_stmt
    	: FOR LPAREN assign_stmt { fprintf(fout, "ForLoop_begin:\n"); mode = 2; } expression SEMICOLON { fprintf(fout, "ifeq ForLoop_exit\n"); } 
	updateCounter RPAREN start_scope statements { mode = 2; } end_scope
;

updateCounter
	: IDENT INC					{ isInc = true; address4 = address3; }
	| IDENT DEC					{ isInc = false; address4 = address3; }
;

start_scope
	: LBRACE					{ create_symbol(++scope_level); 
							if(mode == 1){
								fprintf(fout, "ifeq WhileLoop_exit_%d\n", whileCount);	
							}
							else if(mode == 2);
							/*else if(mode == 3){
							}*/
							mode = 0;
							}
;

end_scope
	: RBRACE					{ dump_symbol(scope_level--);  
							if(mode == 1){
								fprintf(fout, "goto WhileLoop_begin_%d\n", whileCount);
                                                        	fprintf(fout, "WhileLoop_exit_%d:\n", whileCount--);
							}
							else if(mode == 2){
								if(isInc){
                                                                	fprintf(fout, "iload %d\n", address4);
                                                                	fprintf(fout, "ldc 1\n");
                                                                	fprintf(fout, "iadd\n");
                                                                	fprintf(fout, "istore %d\n", address4);
                                                        	}
								else if(!isInc){
                                                                	fprintf(fout, "iload %d\n", address4);
                                                                	fprintf(fout, "ldc 1\n");
                                                                	fprintf(fout, "isub\n");
                                                                	fprintf(fout, "istore %d\n", address4);
                                                        	}
                                                        	fprintf(fout, "goto ForLoop_begin\n");
                                                        	fprintf(fout, "ForLoop_exit:\n");
							}
							/*else if(mode == 3){
							}*/
							mode = 0;
							}
;


%%

/* C code section */
static void create_symbol(int level)
{
	// ne domashka a huiny ebanaya

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

	address2 = address;	

    	for(int j = 0; j < 10; j++){
      		if(strcmp(symbol_table[j][level]->name, "")){
          		if(!strcmp(symbol_table[j][level]->name, name)){
				HAS_ERROR = true;
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

static void printShit2(char *shit)
{
	if(!strcmp(shit, "int")){
       		fprintf(fout,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(fout, "swap\n");
                fprintf(fout,"invokevirtual java/io/PrintStream/print(I)V\n");
     	}
        else if(!strcmp(shit, "float")){ 
        	fprintf(fout,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(fout, "swap\n");
                fprintf(fout,"invokevirtual java/io/PrintStream/print(F)V\n");
      	}
        else if(!strcmp(shit, "string")){ 
        	fprintf(fout,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(fout, "swap\n");
                fprintf(fout,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
     	}
        else if(!strcmp(shit, "bool")){
        	fprintf(fout, "ifne L_cmp_%d\n", counter);
                fprintf(fout, "ldc \"false\"\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "ldc \"true\"\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter); 
                counter++;
                fprintf(fout,"getstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(fout, "swap\n");
                fprintf(fout,"invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
     	}
}

static void printShit(char *shit, char *flag)
{
	if(!strcmp("ADD", shit)){
		if(!strcmp(flag, "int")) fprintf(fout, "iadd\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fadd\n");
        }
	else if(!strcmp("SUB", shit)){				      
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fsub\n");
	}
	else if(!strcmp("MUL", shit)){
	        if(!strcmp(flag, "int")) fprintf(fout, "imul\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fmul\n");
        }
        else if(!strcmp("QUO", shit)){
	        if(!strcmp(flag, "int")) fprintf(fout, "idiv\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fdiv\n");
        }
        else if(!strcmp("REM", shit)){
	        if(!strcmp(flag, "int")) fprintf(fout, "irem\n");
	}
       	else if(!strcmp("POS", shit)) { }
	else if(!strcmp("NEG", shit)){
		if(!strcmp(flag, "int")) fprintf(fout, "ineg\n");
               	else if(!strcmp(flag, "float")) fprintf(fout, "fneg\n");
       	}
	else if(!strcmp("EQL", shit)){
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "ifeq L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
     	}
        else if(!strcmp("NEQ", shit)){
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "ifne L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
      	}
        else if(!strcmp("GTR", shit)){
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "ifgt L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
	}
	else if(!strcmp("LSS", shit)){ 
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "iflt L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
       	}
        else if(!strcmp("GEQ", shit)){ 
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "ifge L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
      	}
        else if(!strcmp("LEQ", shit)){
        	if(!strcmp(flag, "int")) fprintf(fout, "isub\n");
                else if(!strcmp(flag, "float")) fprintf(fout, "fcmpl\n");
                fprintf(fout, "ifle L_cmp_%d\n", counter);
                fprintf(fout, "iconst_0\n");
                fprintf(fout, "goto L_cmp_%d\n", ++counter);
                fprintf(fout, "L_cmp_%d:\n", --counter);
                fprintf(fout, "iconst_1\n");
                fprintf(fout, "L_cmp_%d:\n", ++counter);
                counter++;
     	}
	
	
}

char* returnType(char *id, int level)
{
	char *t = calloc(100, sizeof(char));
	
	for(int i = level; i >= 0; i--){
		for(int j = 0; j < 10; j++){
			if(strcmp(symbol_table[j][i]->name, "")){
				if(!strcmp(symbol_table[j][i]->name, id)){
					strcpy(t, symbol_table[j][i]->type);
					if(!strcmp(t, "array")){
						strcpy(t, symbol_table[j][i]->elementType);
						fprintf(fout, "aload %d\n", symbol_table[j][i]->address);
						if(!strcmp(t, "int")) flag_type = 1; 
                                        	else if(!strcmp(t, "float")) flag_type = 2;
						address3 = symbol_table[j][i]->address;
						return t;
					}
					if(!strcmp(t, "int")){ 
						flag_type = 1; 
						fprintf(fout, "iload %d\n", symbol_table[j][i]->address); 
						address3 = symbol_table[j][i]->address;
					}
                                        else if(!strcmp(t, "float")){ 
						flag_type = 2; 
						fprintf(fout, "fload %d\n", symbol_table[j][i]->address);
						address3 = symbol_table[j][i]->address;
					}
                                        else if(!strcmp(t, "string")){ 
						flag_type = 3; 
						fprintf(fout, "aload %d\n", symbol_table[j][i]->address); 
						address3 = symbol_table[j][i]->address;
					}
                                        else if(!strcmp(t, "bool")){ 
						flag_type = 4; 
						fprintf(fout, "iload %d\n", symbol_table[j][i]->address); 
						address3 = symbol_table[j][i]->address;
					}
					return t;
					//break;
				}
			}
		}
	}
	return t;
}

char* returnType2(char *id, int level)
{
        char *t = calloc(100, sizeof(char));

        for(int i = level; i >= 0; i--){
                for(int j = 0; j < 10; j++){
                        if(strcmp(symbol_table[j][i]->name, "")){
                                if(!strcmp(symbol_table[j][i]->name, id)){
                                        strcpy(t, symbol_table[j][i]->type);
                                        if(!strcmp(t, "array")){
						strcpy(t, symbol_table[j][i]->elementType);
					}
                                        return t;
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

	/* Codegen output init */
    	char *bytecode_filename = "hw3.j";
    	fout = fopen(bytecode_filename, "w");
    	codegen(".source hw3.j\n");
    	codegen(".class public Main\n");
    	codegen(".super java/lang/Object\n");
    	codegen(".method public static main([Ljava/lang/String;)V\n");
    	codegen(".limit stack 100\n");
    	codegen(".limit locals 100\n");
    	INDENT++;

    	yyparse();

	printf("Total lines: %d\n", yylineno);
    	
	/* Codegen end */
    	codegen("return\n");
    	INDENT--;
    	codegen(".end method\n");
    	fclose(fout);
	fclose(yyin);
    	
	if (HAS_ERROR) {
        	remove(bytecode_filename);
    	}

	return 0;
}
