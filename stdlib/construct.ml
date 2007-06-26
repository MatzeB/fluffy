typeclass Construct<T>:
	func construct(object : T*)
	func destruct(object : T*)

func new<Construct T>() : T*:
	var result <- cast<T* > malloc(__sizeof<T>)
	construct(result)
	return result

func delete<Construct T>(obj : T*):
	destruct(obj)
	free(obj)
