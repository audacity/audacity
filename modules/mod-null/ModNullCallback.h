// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the LIBSCRIPT_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// MOD_NULL_DLL_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.


/* Magic for dynamic library import and export. This is unfortunately
 * compiler-specific because there isn't a standard way to do it. Currently it
 * works with the Visual Studio compiler for windows, and for GCC 4+. Anything
 * else gets all symbols made public, which gets messy */
/* The Visual Studio implementation */
#ifdef _MSC_VER
#define DLL_API _declspec(dllexport)
#else
#define DLL_API __attribute__ ((visibility("default")))
#endif
