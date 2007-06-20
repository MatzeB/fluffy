struct SqlAttribute:
	attribute : Attribute
	sqltext   : String

struct SqlTypeAttribute:
	attribute : Attribute
	typetext  : String

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

var sql_attribute     : unsigned int
var sqltype_attribute : unsigned int
var token_sql         : int
var token_sqltype     : int

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

func init_plugin():
	puts("init sql plugin")
	sql_attribute     <- register_attribute()
	sqltype_attribute <- register_attribute()

	token_sql     <- register_new_token("sql")
	token_sqltype <- register_new_token("sqltype")

	register_attribute_parser(parse_sql_attribute, token_sql)
	register_attribute_parser(parse_sqltype_attribute, token_sqltype)
