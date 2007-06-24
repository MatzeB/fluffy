typeclass Construct<T>:
	func construct(data : T*)

typeclass IteratorClass<T>:
	func at()   : T
	func next()
	func compare(T other_iterator) : bool

typeclass Iterable<T>:
	type Iterator : IteratorClass

	func begin() : Iterator
	func end()   : Iterator

struct FlexibleArray<T>:
	elements     : T*
	size         : unsigned int
	storage_size : unsigned int

instance Construct<T> FlexibleArray<T>*:
	func construct(data : FlexibleArray<T>*):
		data.elements     <- cast<FlexibleArray<T>* > 0
		data.size         <- 0
		data.storage_size <- 0

func flexible_array_add(array : FlexibleArray<T>*, T element):
	/* TODO... */


