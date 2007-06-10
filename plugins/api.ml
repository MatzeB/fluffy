struct SourcePosition:
	input_name : byte*
	linenr     : unsigned int

struct Symbol:
	string : byte*
	id     : unsigned int
	thing  : EnvironmentEntry*
	label  : EnvironmentEntry*

struct Token:
	type : int
	v    : V

union V:
	symbol   : Symbol*
	intvalue : int
	string   : String

struct Type:
	type      : int
	firm_type : IrType*

struct Statement:
	type            : int
	next            : Statement*
	source_position : SourcePosition

struct Expression:
	type            : int
	datatype        : Type*
	source_position : SourcePosition

struct BlockStatement:
	statement       : Statement
	statements      : Statement*

struct ExpressionStatement:
	statement       : Statement
	expression      : Expression*

struct LabelStatement:
	statement       : Statement
	symbol          : Symbol*

	block           : IrNode*
	next            : LabelStatement*

struct IfStatement:
	statement       : Statement
	condition       : Expression*
	true_statement  : Statement*
	false_statement : Statement*

struct GotoStatement:
	statement    : Statement
	label_symbol : Symbol*
	label        : LabelStatement*

struct Lexer:
	c               : int
	source_position : SourcePosition
	input           : FILE*
	// more stuff...

typealias Semantic               <- void
typealias FILE                   <- void
typealias EnvironmentEntry       <- void
typealias IrNode                 <- void
typealias IrType                 <- void
typealias ParseStatementFunction <- func () : Statement*
typealias LowerStatementFunction <- func (env : Semantic*, statement : Statement*) : Statement*
typealias String                 <- byte*

extern func register_new_token(token : String) : int
extern func register_statement() : int
extern func register_expression() : int
extern func register_namespace_entry() : int

extern func puts(string : String) : int
extern func fputs(string : String, stream : FILE*) : int
extern func printf(string : String, ptr : void*)
extern func abort()
extern func memset(ptr : void*, c : int, size : unsigned int)

extern func register_statement_parser(parser : ParseStatementFunction*, \
                                      token_type : int)
extern func print_token(out : FILE*, token : Token*)
extern func lexer_next_token(lexer : Lexer*, token : Token*)
extern func allocate_ast(size : unsigned int) : void*
extern func parser_print_error_prefix()
extern func next_token()

extern func parse_expression() : Expression*
extern func parse_statement() : Statement*

extern func print_error_prefix(env : Semantic*, position : SourcePosition)
extern func print_warning_preifx(env : Semantic*, position : SourcePosition)
extern func check_statement(env : Semantic*, statement : Statement*) : Statement*
extern func check_expression(env : Semantic*, expression : Expression*) : Expression*
extern func register_statement_lowerer(function : LowerStatementFunction*, \
                                       statement_type : int)

extern var stdout : FILE*
extern var stderr : FILE*
extern var token  : Token
extern var lexer  : Lexer

typeclass AllocateOnAst<T>:
	func allocate() : T*

func allocate_zero<T>() : T*:
	var res <- cast<T* > allocate_ast(__sizeof<T>)
	memset(res, 0, __sizeof<T>)
	return res

instance AllocateOnAst<BlockStatement>:
	func allocate() : BlockStatement*:
		var res <- allocate_zero<$BlockStatement>()
		res.statement.type <- 1
		return res
	
instance AllocateOnAst<IfStatement>:
	func allocate() : IfStatement*:
		var res <- allocate_zero<$IfStatement>()
		res.statement.type <- 4
		return res

instance AllocateOnAst<ExpressionStatement>:
	func allocate() : ExpressionStatement*:
		var res <- allocate_zero<$ExpressionStatement>()
		res.statement.type <- 5
		return res

instance AllocateOnAst<GotoStatement>:
	func allocate() : GotoStatement*:
		var res <- allocate_zero<$GotoStatement>()
		res.statement.type <- 6
		return res

instance AllocateOnAst<LabelStatement>:
	func allocate() : LabelStatement*:
		var res <- allocate_zero<$LabelStatement>()
		res.statement.type <- 7
		return res

func expect(token_type : int):
	if token.type /= token_type:
		parser_print_error_prefix()
		fputs("Parse error expected another token\n", stderr)
		abort()
	next_token()

func assert(expr : int):
	if expr /= 0:
		fputs("Assert failed\n", stderr)
		abort()

func block_append(block : BlockStatement*, append : Statement*):
	var statement <- block.statements
	:label
	if statement.next = cast<Statement* > 0:
		statement.next <- append
		return

	statement <- statement.next
	goto label


