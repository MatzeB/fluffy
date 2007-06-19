extern func print_int(val : int)
extern func malloc(size : unsigned int) : void*

struct Blo:
	array : int[10]

typealias arr <- int[7]

func main() : int:
	var blo <- cast<Blo* > malloc(__sizeof<Blo>)
	print_int(__sizeof<Blo>)
	blo.array[2] <- 5
	return blo.array[2]
