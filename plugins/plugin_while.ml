struct WhileStatement:
	Statement    statement
	Expression*  loop_control
	Statement*   loop_body

var int token_while
var int while_statement_type

instance AllocateOnAst<WhileStatement>:
	func WhileStatement* allocate():
		var res <- allocate_zero<$WhileStatement>()
		res.statement.type <- while_statement_type
		return res

func Statement* parse_while_statement(Parser* env):
	puts("parsing a while...")
	var statement <- allocate<$WhileStatement>()

	//assert(env.token.type = token_while)
	next_token(env)

	statement.loop_control <- parse_expression(env)
	expect(env, ':')

	statement.loop_body <- parse_statement(env)

	return cast<Statement* > statement

func Statement* lower_while_statement(Semantic *env, Statement* statement):
	var while_statement <- cast<WhileStatement* > statement
	var loop_body     <- while_statement.loop_body

	/* lower&check semantics of inner expressions and statements */
	loop_body <- check_statement(env, loop_body)
	while_statement.loop_control \
		<- check_expression(env, while_statement.loop_control)

	var label             <- allocate<$LabelStatement>()

	var if_statement            <- allocate<$IfStatement>()
	if_statement.condition      <- while_statement.loop_control
	if_statement.true_statement <- loop_body

	var loop_body_block        <- cast<BlockStatement* > loop_body

	var goto_statement   <- allocate<$GotoStatement>()
	goto_statement.label <- label
	block_append(loop_body_block, cast<Statement* > goto_statement)

	var block                 <- allocate<$BlockStatement>()
	block.statements          <- cast<Statement* > label
	label.statement.next      <- cast<Statement* > if_statement

	return cast<Statement* > block

func void init_plugin():
	puts("init_plugin while is here")
	token_while          <- register_new_token("while")
	while_statement_type <- register_statement()
	register_statement_parser(parse_while_statement, token_while)
	register_statement_lowerer(lower_while_statement, while_statement_type)
