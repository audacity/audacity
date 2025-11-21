# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of helpers for working with cryptographic functions.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   crypto/SHA256.cpp
   crypto/SHA256.h
)
set( LIBRARIES
   PUBLIC
)
audacity_library( lib-crypto "${SOURCES}" "${LIBRARIES}"
   "" "" )
