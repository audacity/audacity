# - Find SQLITE3
# Find the native SQLITE3 includes and libraries
#
#  SQLITE3_INCLUDE_DIRS - where to find sqlite.h, etc.
#  SQLITE3_LIBRARIES    - List of libraries when using SQLITE3.
#  SQLITE3_FOUND        - True if SQLITE3 found.

if (SQLITE3_INCLUDE_DIR)
    # Already in cache, be silent
    set (SQLITE3_FIND_QUIETLY TRUE)
endif ()

find_package (PkgConfig QUIET)
pkg_check_modules (PC_SQLITE3 QUIET sqlite3)

set (SQLITE3_VERSION ${PC_SQLITE3_VERSION})

find_path (SQLITE3_INCLUDE_DIR sqlite3.h
    HINTS
        ${PC_SQLITE3_INCLUDEDIR}
        ${PC_SQLITE3_INCLUDE_DIRS}
        ${SQLITE3_ROOT}
    )

find_library (SQLITE3_LIBRARY
    NAMES
        sqlite3
    HINTS
        ${PC_SQLITE3_LIBDIR}
        ${PC_SQLITE3_LIBRARY_DIRS}
        ${SQLITE3_ROOT}
    )

include (FindPackageHandleStandardArgs)

find_package_handle_standard_args (SQLITE3
    REQUIRED_VARS
        SQLITE3_LIBRARY
        SQLITE3_INCLUDE_DIR
    VERSION_VAR
        SQLITE3_VERSION
    )

if (SQLITE3_FOUND)
	set (SQLITE3_INCLUDE_DIRS ${SQLITE3_INCLUDE_DIR})
    set (SQLITE3_LIBRARIES ${SQLITE3_LIBRARY})
	if (NOT TARGET SQLite3::SQLite3)
        add_library (SQLite3::SQLite3 UNKNOWN IMPORTED)
        set_target_properties (SQLite3::SQLite3 PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${SQLITE3_INCLUDE_DIRS}"
            IMPORTED_LOCATION "${SQLITE3_LIBRARIES}"
        )
    endif ()
endif ()

mark_as_advanced (SQLITE3_INCLUDE_DIR SQLITE3_LIBRARY)
