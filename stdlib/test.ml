func main():
	var array <- new<$FlexibleArray>()
	flexarray_append_string(array, "<html>\n")
	flexarray_append_string(array, "\t<body>\n")
	flexarray_append_string(array, "\t\t<h1>Hello ")
	flexarray_append_string(array, "Welt</h1>\n")
	flexarray_append_string(array, "\t</body>\n")
	flexarray_append_string(array, "</html>")
	puts(array.buffer)
	delete(array)

	printf("Jo: %d\n", 42)
