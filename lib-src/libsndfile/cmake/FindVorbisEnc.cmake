# - Find vorbisenc
# Find the native vorbisenc includes and libraries
#
#  VORBISENC_INCLUDE_DIRS - where to find vorbisenc.h, etc.
#  VORBISENC_LIBRARIES    - List of libraries when using vorbisenc.
#  VORBISENC_FOUND        - True if vorbisenc found.

if (VORBISENC_INCLUDE_DIR)
	# Already in cache, be silent
	set (VORBISENC_FIND_QUIETLY TRUE)
endif ()

find_package (Vorbis QUIET)

find_package (PkgConfig QUIET)
pkg_check_modules (PC_VORBISENC QUIET vorbisenc)

set (VORBISENC_VERSION ${PC_VORBISENC_VERSION})

find_path (VORBISENC_INCLUDE_DIR vorbis/vorbisenc.h
	HINTS
		${PC_VORBISENC_INCLUDEDIR}
		${PC_VORBISENC_INCLUDE_DIRS}
		${VORBISENC_ROOT}
	)

find_library (VORBISENC_LIBRARY
	NAMES
		vorbisenc
		vorbisenc_static
		libvorbisenc
		libvorbisenc_static
	HINTS
		${PC_VORBISENC_LIBDIR}
		${PC_VORBISENC_LIBRARY_DIRS}
		${VORBISENC_ROOT}
	)

# Handle the QUIETLY and REQUIRED arguments and set VORBISENC_FOUND
# to TRUE if all listed variables are TRUE.
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (VorbisEnc
	REQUIRED_VARS
		VORBISENC_LIBRARY
		VORBISENC_INCLUDE_DIR
		VORBIS_FOUND
	VERSION_VAR
        VORBISENC_VERSION
	)

if (VORBISENC_FOUND)
	set (VORBISENC_INCLUDE_DIRS ${VORBISENC_INCLUDE_DIR})
	set (VORBISENC_LIBRARIES ${VORBISENC_LIBRARY} ${VORBIS_LIBRARIES})
    if (NOT TARGET Vorbis::VorbisEnc)
		add_library (Vorbis::VorbisEnc UNKNOWN IMPORTED)
		set_target_properties (Vorbis::VorbisEnc PROPERTIES
			INTERFACE_INCLUDE_DIRECTORIES "${VORBISENC_INCLUDE_DIR}"
			IMPORTED_LOCATION "${VORBISENC_LIBRARY}"
			INTERFACE_LINK_LIBRARIES Vorbis::Vorbis
		)
	endif ()
endif ()

mark_as_advanced (VORBISENC_INCLUDE_DIR VORBISENC_LIBRARY)
