struct FlexibleArray:
	buffer : byte*
	len    : size_t
	size   : size_t

instance Construct FlexibleArray:
	func construct(array : FlexibleArray*):
		array.buffer <- null
		array.len    <- 0
		array.size   <- 0

	func destruct(array : FlexibleArray*):
		free(array.buffer)

func flexarray_resize(array : FlexibleArray*, new_len : size_t):
	if new_len > array.size:
		var new_size <- array.size + 1

		while new_size < new_len:
			new_size <- new_size * 2

		array.buffer <- cast<byte* > realloc(array.buffer, new_size)
		array.size   <- new_size

	array.len <- new_len

func flexarray_append_char(array : FlexibleArray*, c : byte):
	flexarray_resize(array, array.len + 1)
	array.buffer[array.len - 1] <- c

func flexarray_append(array : FlexibleArray*, buffer : byte*, buffer_len : size_t):
	var old_len <- array.len
	flexarray_resize(array, array.len + buffer_len)
	memcpy(array.buffer + cast<byte* > old_len, buffer, buffer_len)

func flexarray_append_string(array : FlexibleArray*, string : String):
	var len <- strlen(string)
	flexarray_append(array, string, len)
