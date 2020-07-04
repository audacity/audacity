/**********************************************************************

Audacity: A Digital Audio Editor

Sqlite3Wrappers.h

Paul Licameli

Some C++ RAII around the structures of the sqlite3 library

**********************************************************************/

#ifndef __AUDACITY_SQLITE3_WRAPPERS__
#define __AUDACITY_SQLITE3_WRAPPERS__

#include <memory>
#include <sqlite3.h>

///\brief parameterized type alias for functions that free sqlite resources
/// and return an error code
template< typename Object > using sqlite_deleter_fn = int (*)(Object *);

///\brief template that generates deleter classes, to be used as the second
/// parameter of std::unique_ptr, and may be constructed with an optional
/// pointer to integer that receives the error code from invocation of the
/// deleter function
template< typename Object, sqlite_deleter_fn<Object> fn >
struct sqlite3_object_deleter {
   int *pRc = nullptr;
   void operator () (Object *p) {
      auto rc = fn(p);
      if ( pRc )
         *pRc = rc;
   }
};

///\brief A smart pointer to sqlite3_stmt
using sqlite3_stmt_ptr = std::unique_ptr< sqlite3_stmt,
   sqlite3_object_deleter< sqlite3_stmt, sqlite3_finalize > >;

///\brief a C++ overload of the sqlite3 library function, it assigns a
/// smart @ref sqlite3_stmt_ptr
inline int sqlite3_prepare_v2(
  sqlite3 *db,              /* Database handle. */
  const char *zSql,         /* UTF-8 encoded SQL statement. */
  int nBytes,               /* Length of zSql in bytes. */
  sqlite3_stmt_ptr *ppStmt, /* OUT: A pointer to the prepared statement */
  const char **pzTail       /* OUT: End of parsed string */
){
   sqlite3_stmt *ptr = nullptr;
   auto result = ::sqlite3_prepare_v2(db, zSql, nBytes, &ptr, pzTail);
   ppStmt->reset( ptr );
   return result;
}

#endif
