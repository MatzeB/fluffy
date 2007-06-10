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

typealias Semantic               <- void
typealias FILE                   <- void
typealias EnvironmentEntry       <- void
typealias IrNode                 <- void
typealias IrType                 <- void
typealias ParseStatementFunction <- func Statement* ()
typealias LowerStatementFunction <- func Statement* (Semantic* env, Statement* statement)
typealias String                 <- byte*

extern func int   register_new_token(String token)
extern func int   register_statement()
extern func int   register_expression()
extern func int   register_namespace_entry()

extern func int   puts(String string)
extern func int   fputs(String string, FILE* stream)
extern func void  printf(String string, void* ptr)
extern func void  abort()
extern func void  memset(void *ptr, int c, unsigned int size)

extern func void  register_statement_parser(ParseStatementFunction* parser, \
                                            int token_type)
extern func void  print_token(FILE* out, Token* token)
extern func void  lexer_next_token(Lexer* lexer, Token* token)
extern func void* allocate_ast(unsigned int size)
extern func void  parser_print_error_prefix()
extern func void  next_token()

extern func Expression* parse_expression()
extern func Statement*  parse_statement()

extern func void  print_error_prefix(Semantic* env, SourcePosition position)
extern func void  print_warning_preifx(Semantic* env, SourcePosition position)
extern func Statement* check_statement(Semantic* env, Statement* statement)
extern func Expression* check_expression(Semantic* env, Expression* expression)
extern func void  register_statement_lowerer(LowerStatementFunction* function, \
                                             int statement_type)

extern var FILE*  stdout
extern var FILE*  stderr
extern var Token  token
extern var Lexer  lexer

typeclass AllocateOnAst<T>:
	func T* allocate()

func T* allocate_zero<T>():
	var res <- cast<T* > allocate_ast(__sizeof<T>)
	memset(res, 0, __sizeof<T>)
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

func void expect(int token_type):
	if token.type /= token_type:
		parser_print_error_prefix()
		fputs("Parse error expected another token\n", stderr)
		abort()
	next_token()

func void assert(int expr):
	if expr /= 0:
		fputs("Assert failed\n", stderr)
		abort()

func void block_append(BlockStatement* block, Statement* append):
	var statement <- block.statements
	:label
	if statement.next = cast<Statement* > 0:
		statement.next <- append
		return

	statement <- statement.next
	goto label


