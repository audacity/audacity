# Some default installation locations. These should be global, with any project
# specific locations added to the end. These paths are all relative to the
# install prefix.
#
# These paths attempt to adhere to the FHS, and are similar to those provided
# by autotools and used in many Linux distributions.

# Use GNU install directories
include(GNUInstallDirs)

if(NOT INSTALL_RUNTIME_DIR)
  set(INSTALL_RUNTIME_DIR ${CMAKE_INSTALL_BINDIR})
endif()
if(NOT INSTALL_LIBRARY_DIR)
  set(INSTALL_LIBRARY_DIR ${CMAKE_INSTALL_LIBDIR})
endif()
if(NOT INSTALL_ARCHIVE_DIR)
  set(INSTALL_ARCHIVE_DIR ${CMAKE_INSTALL_LIBDIR})
endif()
if(NOT INSTALL_INCLUDE_DIR)
  set(INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_INCLUDEDIR})
endif()
if(NOT INSTALL_DATADIR)
  set(INSTALL_DATADIR ${CMAKE_INSTALL_DATADIR})
endif()
if(NOT INSTALL_DOC_DIR)
  set(INSTALL_DOC_DIR ${CMAKE_INSTALL_DOCDIR}${KDDockWidgets_LIBRARY_QTID})
endif()

set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
if(APPLE)
  set(CMAKE_MACOSX_RPATH ON)
else()
  set(CMAKE_INSTALL_RPATH "$ORIGIN/../${INSTALL_LIBRARY_DIR}")
endif()
