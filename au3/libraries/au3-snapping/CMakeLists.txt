#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library that provides snapping functionality for the project.

This library:
1. Provides a registry for the snapping functions
2. Implements the required snapping functions
3. Provides a project extension that is responsible for snapping

]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set(TARGET au3-snapping)
set(TARGET_ROOT ${CMAKE_CURRENT_SOURCE_DIR} )

set(SOURCES
   details/BeatsSnapFunctions.cpp
   details/FrameSnapFunctions.cpp
   details/TimeSnapFunctions.cpp

   ProjectSnap.cpp
   ProjectSnap.h
   Snap.cpp
   Snap.h
   SnapUtils.cpp
   SnapUtils.h
)

set( LIBRARIES
   au3-project-rate-interface
   au3-numeric-formats-interface
   au3-screen-geometry-interface
   au3-track-interface
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
