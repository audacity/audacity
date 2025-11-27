# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of concurrency primitives
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   concurrency/CancellationContext.cpp
   concurrency/CancellationContext.h
   concurrency/ICancellable.h
)
set( LIBRARIES
   PUBLIC
)
audacity_library( au3-concurrency "${SOURCES}" "${LIBRARIES}"
   "" "" )
