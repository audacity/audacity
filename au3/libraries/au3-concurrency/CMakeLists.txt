# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of concurrency primitives
]]

set( TARGET au3-concurrency )

set( SOURCES
   concurrency/CancellationContext.cpp
   concurrency/CancellationContext.h
   concurrency/ICancellable.h
)
set( LIBRARIES
   PUBLIC
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" "" )
