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

struct LexerState:
	Token           token
	SourcePosition  source_position

struct Parser:
	Token           token
	SourcePosition  source_position
	LexerState      lookahead
	// more stuff...

typealias EnvironmentEntry <- void
typealias ParseStatementFunction <- func Statement* (Parser* parser)

extern func int register_new_token(byte* token)
extern func int puts(byte* string)
extern func void register_statement_parser(Parser* parser_env, ParseStatementFunction* parser, int token_type)

extern var Parser* current_parser
var int     token_for

func void next_token(Parser* env):
	env.token           = env.lookahead.token
	env.source_position = env.source_position
	lexer_next_token(lexer, &env.lookahead.token)

	env.lookahead.source_position = env.lexer.source_position

func Statement* parse_for_statement(Parser* env):
	puts("parsing a for...")
	next_token(env)
	return cast<Statement* > 0

func void init_plugin():
	puts("init_plugin is here")
	token_for <- register_new_token("for")
	register_statement_parser(current_parser, parse_for_statement, token_for)
