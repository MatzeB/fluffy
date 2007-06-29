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
	type      : unsigned int
	firm_type : IrType*

struct Attribute:
	type            : unsigned int
	source_position : SourcePosition
	next            : Attribute*

struct CompoundEntry:
	type            : Type*
	symbol          : Symbol*
	next            : CompoundEntry*
	attributes      : Attribute*
	source_position : SourcePosition
	entity          : IrEntity*

struct CompoundType:
	type            : Type
	entries         : CompoundEntry*
	symbol          : Symbol*
	attributes      : Attribute
	type_parameters : TypeVariable*
	source_position : SourcePosition

struct TypeConstraint:
	typeclass_symbol : Symbol*
	type_class       : TypeClass*
	next             : TypeConstraint*

struct TypeVariable:
	constraints      : TypeConstraint
	symbol           : Symbol*
	next             : TypeVariable*
	current_type     : Type*

struct Statement:
	type            : unsigned int
	next            : Statement*
	source_position : SourcePosition

struct Expression:
	type            : unsigned int
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

struct NamespaceEntry:
	type            : unsigned int
	next            : NamespaceEntry*
	source_position : SourcePosition

struct TypeClass:
	namespace_entry : NamespaceEntry
	symbol          : Symbol*
	type_parameters : TypeVariable*
	methods         : TypeClassMethod*
	instances       : TypeClassInstance*

struct TypeClassMethod:
	// TODO

struct TypeClassInstance:
	// TODO

struct Lexer:
	c               : int
	source_position : SourcePosition
	input           : FILE*
	// more stuff...

const STATEMENT_INAVLID              <- 0
const STATEMENT_BLOCK                <- 1
const STATEMENT_RETURN               <- 2
const STATEMENT_VARIABLE_DECLARATION <- 3
const STATEMENT_IF                   <- 4
const STATEMENT_EXPRESSION           <- 5
const STATEMENT_GOTO                 <- 6
const STATEMENT_LABEL                <- 7

const TYPE_INVALID                 <- 0
const TYPE_VOID                    <- 1
const TYPE_ATOMIC                  <- 2
const TYPE_COMPOUND_STRUCT         <- 3
const TYPE_COMPOUND_UNION          <- 4
const TYPE_METHOD                  <- 5
const TYPE_POINTER                 <- 6
const TYPE_ARRAY                   <- 7
const TYPE_REFERENCE               <- 8
const TYPE_REFERENCE_TYPE_VARIABLE <- 9

const ATOMIC_TYPE_INVALID          <- 0
const ATOMIC_TYPE_BOOL             <- 1
const ATOMIC_TYPE_BYTE             <- 2

const T_NEWLINE        <- 256
const T_INDENT         <- 257
const T_DEDENT         <- 258
const T_IDENTIFIER     <- 259
const T_INTEGER        <- 260
const T_STRING_LITERAL <- 261

typealias FILE                    <- void
typealias EnvironmentEntry        <- void
typealias IrNode                  <- void
typealias IrType                  <- void
typealias IrEntity                <- void
typealias ParseStatementFunction  <- func () : Statement*
typealias ParseAttributeFunction  <- func () : Attribute*
typealias ParseExpressionFunction <- func (precedence : unsigned int) : Expression*
typealias ParseExpressionInfixFunction <- func (precedence : unsigned int, \
                                               left : Expression*) : Expression*
typealias LowerStatementFunction  <- func (statement : Statement*) : Statement*
typealias LowerExpressionFunction <- func (expression : Expression*) : Expression*
typealias String                  <- byte*

extern func register_new_token(token : String) : unsigned int
extern func register_statement() : unsigned int
extern func register_expression() : unsigned int
extern func register_namespace_entry() : unsigned int
extern func register_attribute() : unsigned int

extern func puts(string : String) : int
extern func fputs(string : String, stream : FILE*) : int
extern func printf(string : String, ptr : void*)
extern func abort()
extern func memset(ptr : void*, c : int, size : unsigned int)

extern func register_statement_parser(parser : ParseStatementFunction*, \
                                      token_type : int)
extern func register_attribute_parser(parser : ParseAttributeFunction*, \
                                      token_type : int)
extern func register_expression_parser(parser : ParseExpressionFunction*, \
                                       token_type : int, \
									   precedence : unsigned int)
extern func register_expression_infix_parser( \
				parser : ParseExpressionInfixFunction, token_type : int, \
				precedence : unsigned int)
extern func print_token(out : FILE*, token : Token*)
extern func lexer_next_token(lexer : Lexer*, token : Token*)
extern func allocate_ast(size : unsigned int) : void*
extern func parser_print_error_prefix()
extern func next_token()

extern func parse_sub_expression(precedence : unsigned int) : Expression*
extern func parse_expression() : Expression*
extern func parse_statement() : Statement*
extern func parse_type() : Type*

extern func print_error_prefix(position : SourcePosition)
extern func print_warning_preifx(position : SourcePosition)
extern func check_statement(statement : Statement*) : Statement*
extern func check_expression(expression : Expression*) : Expression*
extern func register_statement_lowerer(function : LowerStatementFunction*, \
                                       statement_type : unsigned int)
extern func register_expression_lowerer(function : LowerExpressionFunction*, \
                                        expression_type : unsigned int)

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

instance AllocateOnAst BlockStatement:
	func allocate() : BlockStatement*:
		var res <- allocate_zero<$BlockStatement>()
		res.statement.type <- STATEMENT_BLOCK
		return res
	
instance AllocateOnAst IfStatement:
	func allocate() : IfStatement*:
		var res <- allocate_zero<$IfStatement>()
		res.statement.type <- STATEMENT_IF
		return res

instance AllocateOnAst ExpressionStatement:
	func allocate() : ExpressionStatement*:
		var res <- allocate_zero<$ExpressionStatement>()
		res.statement.type <- STATEMENT_EXPRESSION
		return res

instance AllocateOnAst GotoStatement:
	func allocate() : GotoStatement*:
		var res <- allocate_zero<$GotoStatement>()
		res.statement.type <- STATEMENT_GOTO
		return res

instance AllocateOnAst LabelStatement:
	func allocate() : LabelStatement*:
		var res <- allocate_zero<$LabelStatement>()
		res.statement.type <- STATEMENT_LABEL
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

	if block.statements = null:
		block.statements <- append
		return

	:label
	if statement.next = null:
		statement.next <- append
		return

	statement <- statement.next
	goto label

