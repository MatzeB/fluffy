
struct MYSQL:
	/* opaque... */

struct MYSQL_RES:
	/* opaque... */

typealias String <- byte*

extern func mysql_init(mysql : MYSQL*) : MYSQL*
extern func mysql_real_connect(mysql : MYSQL*, host : String, user : String, \
                               passwd : String, db : String, \
                               port : unsigned int, unix_socket : String, \
                               client_flag : unsigned int) : MYSQL*
extern func mysql_close(mysql : MYSQL*)

extern func mysql_options(mysql : MYSQL*, option : int, arg : String) : int
extern func mysql_error(mysql : MYSQL*) : String

extern func mysql_query(mysql : MYSQL*, query : String) : int
extern func mysql_store_result(mysql : MYSQL*) : MYSQL_RES*
extern func mysql_free_result(result : MYSQL_RES*)

