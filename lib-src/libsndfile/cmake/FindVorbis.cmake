# - Find vorbisenc
# Find the native vorbisenc includes and libraries
#
#  VORBIS_INCLUDE_DIRS - where to find vorbis.h, etc.
#  VORBIS_LIBRARIES    - List of libraries when using vorbis.
#  VORBIS_FOUND        - True if vorbis found.

if (VORBIS_INCLUDE_DIR)
	# Already in cache, be silent
	set (VORBIS_FIND_QUIETLY TRUE)
endif ()

find_package (Ogg QUIET)

find_package (PkgConfig QUIET)
pkg_check_modules (PC_VORBIS QUIET vorbis)

set (VORBIS_VERSION ${PC_VORBIS_VERSION})

find_path (VORBIS_INCLUDE_DIR vorbis/codec.h
	HINTS
		${PC_VORBIS_INCLUDEDIR}
		${PC_VORBIS_INCLUDE_DIRS}
		${VORBIS_ROOT}
	)

find_library (VORBIS_LIBRARY
	NAMES
		vorbis
		vorbis_static
		libvorbis
		libvorbis_static
	HINTS
		${PC_VORBIS_LIBDIR}
		${PC_VORBIS_LIBRARY_DIRS}
		${VORBIS_ROOT}
	)

# Handle the QUIETLY and REQUIRED arguments and set VORBIS_FOUND
# to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (Vorbis
	REQUIRED_VARS
		VORBIS_LIBRARY
		VORBIS_INCLUDE_DIR
		OGG_FOUND
	VERSION_VAR
        VORBIS_VERSION
	)

if (VORBIS_FOUND)
	set (VORBIS_INCLUDE_DIRS ${VORBIS_INCLUDE_DIR})
	set (VORBIS_LIBRARIES ${VORBIS_LIBRARY} ${OGG_LIBRARIES})
    if (NOT TARGET Vorbis::Vorbis)
		add_library (Vorbis::Vorbis UNKNOWN IMPORTED)
		set_target_properties (Vorbis::Vorbis PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${VORBIS_INCLUDE_DIR}"
			IMPORTED_LOCATION "${VORBIS_LIBRARY}"
			INTERFACE_LINK_LIBRARIES Ogg::ogg
		)
	endif ()
endif ()

mark_as_advanced (VORBIS_INCLUDE_DIR VORBIS_LIBRARY)
