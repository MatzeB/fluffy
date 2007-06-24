struct SqlAttribute:
	attribute : Attribute
	sqltext   : String

struct SqlTypeAttribute:
	attribute : Attribute
	typetext  : String

struct SqlTableDefExpression:
	expression : Expression
	type       : Type*

instance AllocateOnAst SqlAttribute:
	func allocate() : SqlAttribute*:
		var res <- allocate_zero<$SqlAttribute>()
		res.attribute.type <- sql_attribute
		return res

instance AllocateOnAst SqlTypeAttribute:
	func allocate() : SqlTypeAttribute*:
		var res <- allocate_zero<$SqlTypeAttribute>()
		res.attribute.type <- sqltype_attribute
		return res

instance AllocateOnAst SqlTableDefExpression:
	func allocate() : SqlTableDefExpression*:
		var res <- allocate_zero<$SqlTableDefExpression>()
		res.expression.type <- sqltable_expression
		return res

var sql_attribute       : unsigned int
var sqltype_attribute   : unsigned int
var sqltable_expression : unsigned int
var token_sql           : int
var token_sqltype       : int
var token_sql_table_def : int

func parse_sql_attribute() : Attribute*:
	puts("parsing sql attribute...")
	var attribute <- allocate<$SqlAttribute>()
	
	//assert(token.type = token_sql)
	next_token()

	expect('(')
	if token.type /= 261: /* T_STRING_LITERAL */
		parser_print_error_prefix()
		fputs("Parse error in sql attribute: expected string literal\n", stderr)
		abort()
	attribute.sqltext <- token.v.string
	puts(token.v.string)
	next_token()
	expect(')')

	return cast<Attribute* > attribute

func parse_sqltype_attribute() : Attribute*:
	puts("parsing sqltype attribute...")
	var attribute <- allocate<$SqlTypeAttribute>()
	
	//assert(token.type = token_sqltype)
	next_token()

	expect('(')
	if token.type /= 261: /* T_STRING_LITERAL */
		parser_print_error_prefix()
		fputs("Parse error in sql attribute: expected string literal\n", stderr)
		abort()
	attribute.typetext <- token.v.string
	puts(token.v.string)
	next_token()
	expect(')')

	return cast<Attribute* > attribute

func parse_table_def_expression(precedence : unsigned int) : Expression*:
	var expr <- allocate<$SqlTableDefExpression>()

	//assert(token.type = token_sql_table_def)
	next_token()

	expect('<')
	expr.type <- parse_type()
	expect('>')

	return cast<Expression* > expr

func lower_table_def_expression(expression : Expression*) : Expression*:
	var table_def_expression <- cast<SqlTableDefExpression* > (expression)
	var type                 <- table_def_expression.type

	if type.type /= TYPE_COMPOUND_STRUCT:
		//print_error_prefix(expression.source_position)
		fputs("can only create sql table definition for struct types\n", stderr)
		abort()
	
	var compound_type <- cast<CompoundType* > type
	var entry         <- compound_type.entries
	while entry /= null:


	return cast<Expression* > 0

func init_plugin():
	puts("init sql plugin")
	sql_attribute       <- register_attribute()
	sqltype_attribute   <- register_attribute()
	sqltable_expression <- register_expression()

	token_sql           <- register_new_token("sql")
	token_sqltype       <- register_new_token("sqltype")
	token_sql_table_def <- register_new_token("sql_table_def")

	register_attribute_parser(parse_sql_attribute, token_sql)
	register_attribute_parser(parse_sqltype_attribute, token_sqltype)
	register_expression_parser(parse_table_def_expression, \
	                           token_sql_table_def, 1)
