include (CheckFunctionExists)
include (CheckIncludeFile)
include (CheckLibraryExists)
include (CheckSymbolExists)
include (CheckTypeSize)
include (TestBigEndian)

include (TestInline)
include (ClipMode)
include(TestLargeFiles)

test_large_files (_LARGEFILES)

if (LARGE_FILES_DEFINITIONS)
	add_definitions(${LARGE_FILES_DEFINITIONS})
endif ()

if (WIN32)
    set(TYPEOF_SF_COUNT_T __int64)
else ()
    set(TYPEOF_SF_COUNT_T int64_t)
endif ()
set (SF_COUNT_MAX 0x7fffffffffffffffll)

if (NOT WIN32)
	find_package (ALSA)
	find_package (Sndio)
endif ()

find_package(Ogg 1.3)
find_package (VorbisEnc)
find_package (FLAC)
find_package (Opus)
if (VORBIS_FOUND AND FLAC_FOUND AND OPUS_FOUND)
	set (HAVE_EXTERNAL_XIPH_LIBS 1)
else ()
	set (HAVE_EXTERNAL_XIPH_LIBS 0)
endif ()

find_package (Speex)
find_package (SQLite3)

check_include_file (byteswap.h		HAVE_BYTESWAP_H)
check_include_file (dlfcn.h			HAVE_DLFCN_H)
check_include_file (direct.h		HAVE_DIRECT_H)
check_include_file (endian.h		HAVE_ENDIAN_H)
check_include_file (inttypes.h		HAVE_INTTYPES_H)
check_include_file (io.h			HAVE_IO_H)
check_include_file (stdint.h		HAVE_STDINT_H)
check_include_file (sys/time.h		HAVE_SYS_TIME_H)
check_include_file (sys/types.h		HAVE_SYS_TYPES_H)
check_include_file (unistd.h		HAVE_UNISTD_H)

# Never checked
# check_include_file (stdlib.h		HAVE_STDLIB_H)
# check_include_file (string.h		HAVE_STRING_H)
# check_include_file (strings.h		HAVE_STRINGS_H)
# check_include_file (sys/stat.h	HAVE_SYS_STAT_H)
# check_include_file (memory.h		HAVE_MEMORY_H)

if (BUILD_TESTING)
	check_include_file (locale.h	HAVE_LOCALE_H)
	check_include_file (sys/wait.h	HAVE_SYS_WAIT_H)
endif ()

check_type_size (int64_t			SIZEOF_INT64_T)
check_type_size (long				SIZEOF_LONG)
check_type_size (long\ long			SIZEOF_LONG_LONG)
check_type_size (ssize_t			SIZEOF_SSIZE_T)
check_type_size (wchar_t			SIZEOF_WCHAR_T)

# Never used
# check_type_size (loff_t			SIZEOF_LOFF_T)
# check_type_size (offt64_t			SIZEOF_OFF64_T)

# Never checked
# check_type_size (size_t			SIZEOF_SIZE_T)

# Used in configre.ac
# check_type_size (double			SIZEOF_DOUBLE)
# check_type_size (float			SIZEOF_FLOAT)
# check_type_size (int				SIZEOF_INT)
# check_type_size (short			SIZEOF_SHORT)

if (ENABLE_TESTING)
	check_type_size (void*			SIZEOF_VOIDP)
endif()

if ((SIZEOF_OFF_T EQUAL 8) OR (SIZEOF_LOFF_T EQUAL 8) OR (SIZEOF_OFF64_T EQUAL 8))
	set (TYPEOF_SF_COUNT_T "int64_t")
	set (SF_COUNT_MAX "0x7FFFFFFFFFFFFFFFLL")
	set (SIZEOF_SF_COUNT_T 8)
else ()
	if (WIN32)
		set (TYPEOF_SF_COUNT_T "__int64")
		set (SF_COUNT_MAX "0x7FFFFFFFFFFFFFFFLL")
		set (SIZEOF_SF_COUNT_T 8)
	else ()
		message ("")
		message ("*** The configure process has determined that this system is capable")
		message ("*** of Large File Support but has not been able to find a type which")
		message ("*** is an unambiguous 64 bit file offset.")
		message ("*** Please contact the author to help resolve this problem.")
		message ("")
		message (FATAL_ERROR "Bad file offset type.")
	endif ()
endif ()

check_type_size (${TYPEOF_SF_COUNT_T} SIZEOF_SF_COUNT_T)

if (NOT WIN32)
	check_library_exists (m floor "" LIBM_REQUIRED)
	if (LIBM_REQUIRED)
		list (APPEND CMAKE_REQUIRED_LIBRARIES m)
	endif ()
endif ()

check_library_exists (sqlite3 sqlite3_close "" HAVE_SQLITE3)

check_function_exists (fstat     		HAVE_FSTAT)
check_function_exists (fstat64			HAVE_FSTAT64)
check_function_exists (gettimeofday		HAVE_GETTIMEOFDAY)
check_function_exists (gmtime			HAVE_GMTIME)
check_function_exists (gmtime_r			HAVE_GMTIME_R)
check_function_exists (localtime		HAVE_LOCALTIME)
check_function_exists (localtime_r		HAVE_LOCALTIME_R)
check_function_exists (lseek      		HAVE_LSEEK)
check_function_exists (open				HAVE_OPEN)
check_function_exists (read				HAVE_READ)
check_function_exists (write			HAVE_WRITE)
check_function_exists (lrint			HAVE_LRINT)
check_function_exists (lrintf			HAVE_LRINTF)

if (NOT WIN32)
	check_function_exists (ftruncate	HAVE_FTRUNCATE)
	check_function_exists (fsync    	HAVE_FSYNC)
endif ()

if (BUILD_TESTING)
	check_function_exists (pipe			HAVE_PIPE)
	check_function_exists (setlocale	HAVE_SETLOCALE)
	check_function_exists (waitpid		HAVE_WAITPID)
endif ()

# Never checked
# check_function_exists (calloc			HAVE_CALLOC)
# check_function_exists (free			HAVE_FREE)
# check_function_exists (getpagesize	HAVE_GETPAGESIZE)
# check_function_exists (malloc			HAVE_MALLOC)
# check_function_exists (realloc		HAVE_REALLOC)
# check_function_exists (snprintf		HAVE_SNPRINTF)
# check_function_exists (vsnprintf		HAVE_VSNPRINTF)
# check_function_exists (floor			HAVE_FLOOR)
# check_function_exists (fmod			HAVE_FMOD)

# Never used
# check_function_exists (mmap			HAVE_MMAP)
# check_function_exists (ceil			HAVE_CEIL)
# check_function_exists (lround			HAVE_LROUND)
# check_function_exists (lseek64		HAVE_LSEEK64)


check_symbol_exists (S_IRGRP sys/stat.h HAVE_DECL_S_IRGRP)

test_big_endian (WORDS_BIGENDIAN)
if (WORDS_BIGENDIAN)
	set (CPU_IS_BIG_ENDIAN 1)
else ()
	set (CPU_IS_LITTLE_ENDIAN 1)
endif ()

if (WIN32)
	set (OS_IS_WIN32 1)
	set (USE_WINDOWS_API 1)
	if (BUILD_SHARED_LIBS)
		set (WIN32_TARGET_DLL 1)
	endif ()
	if (MINGW)
		add_definitions (-D__USE_MINGW_ANSI_STDIO=1)
	endif ()
endif ()

if (CMAKE_SYSTEM_NAME STREQUAL "OpenBSD")
	set (OS_IS_OPENBSD 1)
endif ()


if (CMAKE_COMPILER_IS_GNUCC OR (CMAKE_C_COMPILER_ID MATCHES "Clang"))
	set (COMPILER_IS_GCC 1)
endif ()

test_inline ()
clip_mode ()

if (MSVC)
	add_definitions (-D_CRT_SECURE_NO_WARNINGS -D_CRT_NONSTDC_NO_DEPRECATE)
endif (MSVC)

if (ENABLE_STATIC_RUNTIME)
	if (MSVC)
		foreach (flag_var
			CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
			CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
			CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
			CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
			)
			if (${flag_var} MATCHES "/MD")
				string (REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
			endif ()
		endforeach (flag_var)
	endif (MSVC)
	if (MINGW)
		set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static-libgcc")
		set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static-libgcc -static-libstdc++")
		set (CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "${CMAKE_SHARED_LIBRARY_LINK_C_FLAGS} -static-libgcc -s")
		set (CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS "${CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS} -static-libgcc -static-libstdc++ -s")
	endif (MINGW)
elseif (NOT ENABLE_STATIC_RUNTIME)
	if (MSVC)
		foreach (flag_var
			CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
			CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO
			CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
			CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
			)
			if (${flag_var} MATCHES "/MT")
				string (REGEX REPLACE "/MT" "/MD" ${flag_var} "${${flag_var}}")
			endif (${flag_var} MATCHES "/MT")
		endforeach (flag_var)
	endif ()
	if (MINGW)
		set (CMAKE_C_FLAGS "")
		set (CMAKE_CXX_FLAGS "")
		set (CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "")
		set (CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS "")
	endif ()
endif ( )

if (BUILD_SHARED_LIBS)
	find_package (PythonInterp REQUIRED)
endif()
