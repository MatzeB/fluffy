struct Plugin:
	void   *init_function
	Plugin  next
	
extern func int register_new_token(void* lexer, byte* token)
extern func int register_new_operator(void* lexer, byte* token)
extern func int puts(byte* string)

var void* current_lexer

func void init_plugin():
	puts("init_plugin is here")
	register_new_token(current_lexer, "__mytoken")
