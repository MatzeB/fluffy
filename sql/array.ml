typeclass Construct<T>:
	func construct(T* data)


struct FlexibleArray<T>:
	elements     : T*
	size         : unsigned int
	storage_size : unsigned int

instance Construct<T> FlexibleArray<T>*:
	
