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

extern func int register_new_token(byte* token)
extern func int puts(byte* string)

func void init_plugin():
	puts("init_plugin is here")
	register_new_token("for")
