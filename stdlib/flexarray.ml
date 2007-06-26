struct FlexibleArray:
	buffer : byte*
	used   : size_t
	size   : size_t

instance Construct FlexibleArray:
	func construct() : FlexibleArray*:
		var result <- cast<FlexibleArray* > malloc(__sizeof<FlexibleArray>)
		result.buffer <- null
		result.used   <- 0
		result.size   <- 0
		return result

