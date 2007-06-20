
struct Person $sql("primary key(ID)") $sqltype("InnoDB") :
	ID          : int       $sql("not null auto_increment unique")
	title       : byte[255] $sqltype("VARCHAR(255)")
	pre_name    : byte[255] $sqltype("VARCHAR(255)")
	middle_name : byte[255] $sqltype("VARCHAR(255)")
	last_name   : byte[255] $sqltype("VARCHAR(255)")
	suffix      : byte[255] $sqltype("VARCHAR(255)")

func main() : int:
	var mysql      <- mysql_init(cast<MYSQL* > 0)
	mysql_options(mysql, 5 /*=MYSQL_READ_DEFAULT_GROUP*/, "mysqltest")
	var connection <- mysql_real_connect(mysql, "localhost", "test", "test", \
	                                     "test", 0, cast<String> 0, 0)
	if connection = cast<MYSQL* > 0:
		fputs("problem opening mysql connection: ", stderr)
		fputs(mysql_error(mysql), stderr)
		fputs("\n", stderr)
		return 1

	/* query("select * from persons where person.title != ''");
	 * map_results()
	 */

	fputs("all ok", stdout)

	mysql_close(mysql)
	return 0
