# - Try to find jack
# Once done this will define
#  JACK_FOUND - System has jack
#  JACK_INCLUDE_DIRS - The jack include directories
#  JACK_LIBRARIES - The libraries needed to use jack
#  JACK_DEFINITIONS - Compiler switches required for using jack

if (JACK_LIBRARIES AND JACK_INCLUDE_DIRS)

	# in cache already
	set(JACK_FOUND TRUE)

else (JACK_LIBRARIES AND JACK_INCLUDE_DIRS)

	set(JACK_DEFINITIONS "")

	# Look for pkg-config and use it (if available) to find package
	find_package(PkgConfig QUIET)
	if (PKG_CONFIG_FOUND)
		pkg_search_module(JACK QUIET jack)
	endif (PKG_CONFIG_FOUND)

	if (NOT JACK_FOUND)

		find_path(JACK_INCLUDE_DIR jack/jack.h HINTS ${JACK_INCLUDEDIR} ${JACK_INCLUDE_DIRS} PATH_SUFFIXES jack)
		find_library(JACK_LIBRARY NAMES jack HINTS ${JACK_LIBDIR} ${JACK_LIBRARY_DIRS})

		set(JACK_LIBRARIES    ${JACK_LIBRARY})
		set(JACK_INCLUDE_DIRS ${JACK_INCLUDE_DIR})

		include(FindPackageHandleStandardArgs)

		# Set JACK_FOUND if the library and include paths were found
		find_package_handle_standard_args(jack DEFAULT_MSG JACK_LIBRARY JACK_INCLUDE_DIR)

		# Don't show include/library paths in cmake GUI
		mark_as_advanced(JACK_INCLUDE_DIR JACK_LIBRARY)

	endif (NOT JACK_FOUND)

endif (JACK_LIBRARIES AND JACK_INCLUDE_DIRS)
