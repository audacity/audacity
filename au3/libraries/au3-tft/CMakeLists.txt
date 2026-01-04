#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library for time frequency transforms

]]

set( TARGET au3-tft )

set( SOURCES
)
set( LIBRARIES
PUBLIC
   tft
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
