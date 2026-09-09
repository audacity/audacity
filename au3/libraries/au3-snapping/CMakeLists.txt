#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library that provides snapping functionality for the project.

This library:
1. Provides a registry for the snapping functions
2. Implements the required snapping functions
3. Provides a project extension that is responsible for snapping

]]

set(TARGET au3-snapping)

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
PUBLIC
   au3-components
   au3-preferences
   au3-registries
   au3-utility
PRIVATE
   au3-numeric-formats
   au3-project
   au3-project-rate
   au3-screen-geometry
   au3-strings
   au3-track
   au3-xml
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "" "" )
