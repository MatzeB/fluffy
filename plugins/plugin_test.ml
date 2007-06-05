struct SourcePosition:
	byte*         input_name
	unsigned int  linenr

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

struct Type:
	int      type
	IrType*  firm_type

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

struct BlockStatement:
	Statement   statement
	Statement*  statements

struct ExpressionStatement:
	Statement    statement
	Expression*  expression

struct LabelStatement:
	Statement        statement
	Symbol*          symbol

	IrNode*          block
	LabelStatement*  next

struct IfStatement:
	Statement    statement
	Expression*  condition
	Statement*   true_statement
	Statement*   false_statement

struct GotoStatement:
	Statement        statement
	Symbol*          label_symbol
	LabelStatement*  label

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

typealias Semantic               <- void
typealias FILE                   <- void
typealias EnvironmentEntry       <- void
typealias IrNode                 <- void
typealias IrType                 <- void
typealias ParseStatementFunction <- func Statement* (Parser* parser)
typealias LowerStatementFunction <- func Statement* (Semantic* env, Statement* statement)

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
extern func void* allocate_ast(unsigned int size)
extern func void  parser_print_error_prefix(Parser* parser)

extern func Expression* parse_expression(Parser* parser)
extern func Statement*  parse_statement(Parser* parser)

extern func void  print_error_prefix(Semantic* env, SourcePosition position)
extern func void  print_warning_preifx(Semantic* env, SourcePosition position)
extern func Statement* check_statement(Semantic* env, Statement* statement)
extern func Expression* check_expression(Semantic* env, Expression* expression)
extern func void  register_statement_lowerer(LowerStatementFunction* function, \
                                             int statement_type)


extern var FILE*   stdout
extern var FILE*   stderr
extern var Parser* current_parser
var        int     token_for
var        int     for_statement

typeclass AllocateOnAst<T>:
	func T* allocate()

func T* allocate_zero<T>():
	var res <- cast<T* > allocate_ast(__sizeof<T>)
	memset(res, 0, __sizeof<T>)
	return res

instance AllocateOnAst<ForStatement>:
	func ForStatement* allocate():
		var res <- allocate_zero<$ForStatement>()
		res.statement.type <- for_statement
		return res

instance AllocateOnAst<BlockStatement>:
	func BlockStatement* allocate():
		var res <- allocate_zero<$BlockStatement>()
		res.statement.type <- 1
		return res
	
instance AllocateOnAst<IfStatement>:
	func IfStatement* allocate():
		var res <- allocate_zero<$IfStatement>()
		res.statement.type <- 4
		return res

instance AllocateOnAst<ExpressionStatement>:
	func ExpressionStatement* allocate():
		var res <- allocate_zero<$ExpressionStatement>()
		res.statement.type <- 5
		return res

instance AllocateOnAst<GotoStatement>:
	func GotoStatement* allocate():
		var res <- allocate_zero<$GotoStatement>()
		res.statement.type <- 6
		return res

instance AllocateOnAst<LabelStatement>:
	func LabelStatement* allocate():
		var res <- allocate_zero<$LabelStatement>()
		res.statement.type <- 7
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
	var statement <- allocate<$ForStatement>()

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

func void block_append(BlockStatement* block, Statement* append):
	var statement <- block.statements
	:label
	if statement.next = cast<Statement* > 0:
		statement.next <- append
		return

	statement <- statement.next
	goto label

func Statement* lower_for_statement(Semantic *env, Statement* statement):
	var for_statement <- cast<ForStatement* > statement
	var loop_body     <- for_statement.loop_body

	/* lower&check semantics of inner expressions and statements */
	loop_body <- check_statement(env, loop_body)
	for_statement.pre_expression \
		<- check_expression(env, for_statement.pre_expression)
	for_statement.loop_control \
		<- check_expression(env, for_statement.loop_control)
	for_statement.step_expression \
		<- check_expression(env, for_statement.step_expression)

	var expression        <- allocate<$ExpressionStatement>()
	expression.expression <- for_statement.pre_expression

	var label             <- allocate<$LabelStatement>()

	var if_statement            <- allocate<$IfStatement>()
	if_statement.condition      <- for_statement.loop_control
	if_statement.true_statement <- loop_body

	var loop_body_block        <- cast<BlockStatement* > loop_body

	var step_expression        <- allocate<$ExpressionStatement>()
	step_expression.expression <- for_statement.step_expression
	block_append(loop_body_block, cast<Statement* > step_expression) 

	var goto_statement   <- allocate<$GotoStatement>()
	goto_statement.label <- label
	block_append(loop_body_block, cast<Statement* > goto_statement)

	var block                 <- allocate<$BlockStatement>()
	block.statements          <- cast<Statement* > expression
	expression.statement.next <- cast<Statement* > label
	label.statement.next      <- cast<Statement* > if_statement

	return cast<Statement* > block

func void init_plugin():
	puts("init_plugin is here")
	token_for     <- register_new_token("for")
	for_statement <- register_statement()
	register_statement_parser(current_parser, parse_for_statement, token_for)
	register_statement_lowerer(lower_for_statement, for_statement)
