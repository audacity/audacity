#ifndef __gInclude__
#define __gInclude__

#if SGI 
	#undef BEOS 
	#undef MAC 
	#undef WINDOWS
	//
	#define ASIO_BIG_ENDIAN 1
	#define ASIO_CPU_MIPS 1
#elif defined WIN32
	#undef BEOS 
	#undef MAC 
	#undef SGI
	#define WINDOWS 1
	#define ASIO_LITTLE_ENDIAN 1
	#define ASIO_CPU_X86 1
#elif BEOS
	#undef MAC 
	#undef SGI
	#undef WINDOWS
	#define ASIO_LITTLE_ENDIAN 1
	#define ASIO_CPU_X86 1
	//
#else
	#define MAC 1
	#undef BEOS 
	#undef WINDOWS
	#undef SGI
	#define ASIO_BIG_ENDIAN 1
	#define ASIO_CPU_PPC 1
#endif

// always
#define NATIVE_INT64 0
#define IEEE754_64FLOAT 1

#endif	// __gInclude__
