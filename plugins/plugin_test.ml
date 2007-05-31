struct SourcePosition:
	byte*         input_name
	unsigned int  linenr

struct Statement:
	int             type
	Statement*      next
	SourcePosition  source_position
	
extern func int register_new_token(byte* token)
extern func int puts(byte* string)

func void init_plugin():
	puts("init_plugin is here")
	register_new_token("for")
