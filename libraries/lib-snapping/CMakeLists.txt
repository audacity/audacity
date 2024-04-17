#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library that provides snapping functionality for the project.

This library:
1. Provides a registry for the snapping functions
2. Implements the required snapping functions
3. Provides a project extension that is responsible for snapping

]]

set(TARGET lib-snapping)
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
   lib-project-rate-interface
   lib-numeric-formats-interface
   lib-screen-geometry-interface
   lib-track-interface
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
