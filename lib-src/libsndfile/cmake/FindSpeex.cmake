# - Find Speex
# Find the native Speex includes and libraries
#
#	 SPEEX_INCLUDE_DIRS - where to find speex.h, etc.
#	 SPEEX_LIBRARIES	- List of libraries when using Speex.
#	 SPEEX_FOUND		- True if Speex found.

if (SPEEX_INCLUDE_DIR)
	set (SPEEX_FIND_QUIETLY TRUE)
endif ()

find_package (PkgConfig QUIET)
pkg_check_modules (PC_SPEEX QUIET speex)

set (SPEEX_VERSION ${PC_SPEEX_VERSION})

find_path (SPEEX_INCLUDE_DIR speex/speex.h
	HINTS
		${PC_SPEEX_INCLUDEDIR}
		${PC_SPEEX_INCLUDE_DIRS}
		${SPEEX_ROOT}
	)
find_library (SPEEX_LIBRARY
	NAMES
		speex
		libspeex
	HINTS
		${PC_SPEEX_LIBDIR}
		${PC_SPEEX_LIBRARY_DIRS}
		${SPEEX_ROOT}
	)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Speex
	REQUIRED_VARS
		SPEEX_LIBRARY
		SPEEX_INCLUDE_DIR
	VERSION_VAR
		SPEEX_VERSION
	)

if (SPEEX_FOUND)
	set (SPEEX_LIBRARIES ${SPEEX_LIBRARY})
	set (SPEEX_INCLUDE_DIRS ${SPEEX_INCLUDE_DIR})
	
	if (NOT TARGET Speex::Speex)
		add_library (Speex::Speex UNKNOWN IMPORTED)
		set_target_properties (Speex::Speex PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${SPEEX_INCLUDE_DIRS}"
			IMPORTED_LOCATION "${SPEEX_LIBRARIES}"
		)
	endif ()
endif ()

mark_as_advanced (SPEEX_INCLUDE_DIR SPEEX_LIBRARY)
