# - Find opus
# Find the native opus includes and libraries
#
#  OPUS_INCLUDE_DIRS - where to find opus.h, etc.
#  OPUS_LIBRARIES    - List of libraries when using opus.
#  OPUS_FOUND        - True if Opus found.

if (OPUS_INCLUDE_DIR)
    # Already in cache, be silent
    set(OPUS_FIND_QUIETLY TRUE)
endif ()

find_package (Ogg QUIET)

find_package (PkgConfig QUIET)
pkg_check_modules(PC_OPUS QUIET opus>=1.1)

set (OPUS_VERSION ${PC_OPUS_VERSION})

find_path (OPUS_INCLUDE_DIR opus/opus.h
	HINTS
		${PC_OPUS_INCLUDEDIR}
		${PC_OPUS_INCLUDE_DIRS}
		${OPUS_ROOT}
	)

# MSVC built opus may be named opus_static.
# The provided project files name the library with the lib prefix.

find_library (OPUS_LIBRARY
	NAMES
		opus
		opus_static
		libopus
		libopus_static
	HINTS
		${PC_OPUS_LIBDIR}
		${PC_OPUS_LIBRARY_DIRS}
		${OPUS_ROOT}
	)

# Handle the QUIETLY and REQUIRED arguments and set OPUS_FOUND
# to TRUE if all listed variables are TRUE.
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args (Opus
	REQUIRED_VARS
		OPUS_LIBRARY
		OPUS_INCLUDE_DIR
		OGG_FOUND
	VERSION_VAR
		OPUS_VERSION
	)

if (OPUS_FOUND)
	set (OPUS_LIBRARIES ${OPUS_LIBRARY})
	set (OPUS_INCLUDE_DIRS ${OPUS_INCLUDE_DIR})

	if (NOT TARGET Opus::opus)
		add_library (Opus::opus UNKNOWN IMPORTED)
		set_target_properties (Opus::opus PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${OPUS_INCLUDE_DIRS}"
			IMPORTED_LOCATION "${OPUS_LIBRARIES}"
		)
	endif ()
endif ()

mark_as_advanced(OPUS_INCLUDE_DIR OPUS_LIBRARY)
