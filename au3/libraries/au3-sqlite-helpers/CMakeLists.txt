# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of helpers for working with sqlite databases.
]]

set( TARGET au3-sqlite-helpers )

set( SOURCES
   sqlite/Blob.cpp
   sqlite/Blob.h
   sqlite/Connection.cpp
   sqlite/Connection.h
   sqlite/Error.cpp
   sqlite/Error.h
   sqlite/Function.cpp
   sqlite/Function.h
   sqlite/Result.cpp
   sqlite/Result.h
   sqlite/SafeConnection.cpp
   sqlite/SafeConnection.h
   sqlite/SQLiteUtils.cpp
   sqlite/SQLiteUtils.h
   sqlite/Statement.cpp
   sqlite/Statement.h
   sqlite/Transaction.cpp
   sqlite/Transaction.h
)
set( LIBRARIES
PRIVATE
   au3-exceptions
   au3-string-utils
   au3-strings
   au3-utility
   wxBase
   sqlite
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" "" )
