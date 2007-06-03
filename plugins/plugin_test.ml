struct Type:
	int    type
	void*  firm_type

struct SourcePosition:
	byte*         input_name
	unsigned int  linenr

struct Statement:
	int             type
	Statement*      next
	SourcePosition  source_position

struct Expression:
	int             type
	Type*           datatype
	SourcePosition  source_position

struct ForStatement:
	Statement       statement
	Expression*     pre_expression
	Expression*     loop_control
	Expression*     step_expression
	Statement*      loop_body

struct Symbol:
	byte*             string
	unsigned int      id
	EnvironmentEntry* thing
	EnvironmentEntry* label	

struct Token:
	int  type
	V    v

union V:
	Symbol *symbol
	int     intvalue
	byte*   string

struct Lexer:
	int            c
	SourcePosition source_position
	FILE*          input
	// char[1024]    buf
	// more stuff...

struct Parser:
	Token           token
	Lexer           lexer
	// more stuff...

typealias FILE <- void
typealias EnvironmentEntry <- void
typealias ParseStatementFunction <- func Statement* (Parser* parser)

extern func int   register_new_token(byte* token)
extern func int   register_statement()
extern func int   register_expression()
extern func int   register_namespace_entry()

extern func int   puts(byte* string)
extern func void  printf(byte* string, void* ptr)
extern func void  abort()
extern func void  memset(void *ptr, int c, unsigned int size)

extern func void  register_statement_parser(Parser* parser_env, \
                                           ParseStatementFunction* parser, \
                                           int token_type)
extern func void  print_token(FILE* out, Token* token)
extern func void  lexer_next_token(Lexer* lexer, Token* token)
extern func void* allocate_ast(Parser *env, unsigned int size)
extern func void  parser_print_error_prefix(Parser* parser)

extern func Expression* parse_expression(Parser* parser)
extern func Statement*  parse_statement(Parser* parser)

extern var FILE*   stdout
extern var FILE*   stderr
extern var Parser* current_parser
var        int     token_for
var        int     for_statement

typeclass AllocateOnAst<T>:
	func T* allocate(Parser* env)

func T* allocate_zero<T>(Parser*env):
	var res <- cast<T* > allocate_ast(env, __sizeof<T>)
	memset(res, 0, __sizeof<T>)
	return res

instance AllocateOnAst<ForStatement>:
	func ForStatement* allocate(Parser* env):
		var res <- allocate_zero<$ForStatement>(env)
		res.statement.type <- for_statement
		return res

func void expect(Parser* env, int token):
	if env.token.type /= token:
		parser_print_error_prefix(env)
		puts("Parse error expected another token")
		abort()
	next_token(env)

func void assert(int expr):
	if expr /= 0:
		puts("Assert failed")
		abort()

func void next_token(Parser* env):
	lexer_next_token(env.lexer, env.token)
	print_token(stdout, env.token)
	puts("")

func Statement* parse_for_statement(Parser* env):
	puts("parsing a for...")
	var statement <- allocate<$ForStatement>(env)

	//assert(env.token.type = token_for)
	next_token(env)

	expect(env, '(')
	if env.token.type /= ';':
		statement.pre_expression <- parse_expression(env)
	expect(env, ';')
	statement.loop_control <- parse_expression(env)
	expect(env, ';')
	if env.token.type /= ')':
		statement.step_expression <- parse_expression(env)
	expect(env, ')')
	expect(env, ':')

	statement.loop_body <- parse_statement(env)

	return cast<Statement* > statement

func Statement* lower_for_statement(Statement* statement):
	puts("TODO")
	return statement

func void init_plugin():
	puts("init_plugin is here")
	token_for     <- register_new_token("for")
	for_statement <- register_statement()
	register_statement_parser(current_parser, parse_for_statement, token_for)

