# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of concurrency primitives
]]

set( SOURCES
   concurrency/CancellationContext.cpp
   concurrency/CancellationContext.h
   concurrency/ICancellable.h
)
set( LIBRARIES
   PUBLIC
)
audacity_library( lib-concurrency "${SOURCES}" "${LIBRARIES}"
   "" "" )
