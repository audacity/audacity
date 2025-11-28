# SPDX-License-Identifier: GPL-2.0-or-later
# SPDX-FileName: CMakeLists.txt
# SPDX-FileContributor: Dmitry Vedenko
#[[
A set of helpers for working with sqlite databases.
]]

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
   PUBLIC
      sqlite
      au3-exceptions-interface
      au3-strings-interface
      au3-string-utils-interface
)
audacity_library( au3-sqlite-helpers "${SOURCES}" "${LIBRARIES}"
   "" "" )
