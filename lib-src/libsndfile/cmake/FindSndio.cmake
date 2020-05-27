# - Find SoundIO (sndio) includes and libraries
#
#   SNDIO_FOUND        - True if SNDIO_INCLUDE_DIR & SNDIO_LIBRARY are
#                        found
#   SNDIO_LIBRARIES    - Set when SNDIO_LIBRARY is found
#   SNDIO_INCLUDE_DIRS - Set when SNDIO_INCLUDE_DIR is found
#
#   SNDIO_INCLUDE_DIR - where to find sndio.h, etc.
#   SNDIO_LIBRARY     - the sndio library
#

if (SNDIO_INCLUDE_DIR)
	# Already in cache, be silent
	set (SNDIO_FIND_QUIETLY TRUE)
endif ()

find_package (PkgConfig QUIET)
pkg_check_modules (PC_SNDIO QUIET sndio)

set (SNDIO_VERSION ${PC_SNDIO_VERSION})

find_path (SNDIO_INCLUDE_DIR
	NAMES
		sndio.h
	HINTS
		${PC_SNDIO_INCLUDEDIR}
		${PC_SNDIO_INCLUDE_DIRS}
		${SNDIO_ROOT}
	)

find_library (SNDIO_LIBRARY
	NAMES
		sndio
	HINTS
		${PC_SNDIO_LIBDIR}
		${PC_SNDIO_LIBRARY_DIRS}
		${SNDIO_ROOT}
	)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Sndio
	REQUIRED_VARS
		SNDIO_LIBRARY
		SNDIO_INCLUDE_DIR
	VERSION_VAR
		SNDIO_VERSION
	)

if (SNDIO_FOUND)
	set (SNDIO_LIBRARIES ${SNDIO_LIBRARY})
	set (SNDIO_INCLUDE_DIRS ${SNDIO_INCLUDE_DIR})
	if (NOT TARGET Sndio::Sndio)
		add_library (Sndio::Sndio UNKNOWN IMPORTED)
		set_target_properties (Sndio::Sndio PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${SNDIO_INCLUDE_DIRS}"
			IMPORTED_LOCATION "${SNDIO_LIBRARIES}"
		)
	endif()
endif()

mark_as_advanced (SNDIO_INCLUDE_DIR SNDIO_LIBRARY)
