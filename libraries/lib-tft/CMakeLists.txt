#  SPDX-License-Identifier: GPL-2.0-or-later
#[[
A library for time frequency transforms

]]

set( SOURCES
   TF_Calculator.cpp
   TF_Calculator.h
)
set( LIBRARIES
   libtft
   lib-math-interface
)
audacity_library( lib-tft "${SOURCES}" "${LIBRARIES}"
   "" ""
)
