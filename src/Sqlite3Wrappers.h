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

///\brief A smart pointer to sqlite3_backup
using sqlite3_backup_ptr = std::unique_ptr< sqlite3_backup,
   sqlite3_object_deleter< sqlite3_backup, sqlite3_backup_finish > >;

///\brief A type for use as the second parameter of std::unique_ptr
struct sqlite3_message_deleter{
   void operator () (void *p) const {
      sqlite3_free( p ); // void return
   }
};

///\brief A smart pointer to a character string that must be freed with
/// sqlite3_free, which returns void
using sqlite3_message_ptr = std::unique_ptr< char[], sqlite3_message_deleter >;

///\brief a C++ overload of the sqlite3 library function, it assigns a
/// smart @ref sqlite_message_ptr
inline int sqlite3_exec(
  sqlite3 *db,                /* The database on which the SQL executes */
  const char *zSql,           /* The SQL to be executed */
  sqlite3_callback xCallback, /* Invoke this callback routine */
  void *pArg,                 /* First argument to xCallback() */
  sqlite3_message_ptr *pzErrMsg /* Write error messages here */
){
   char *errMsg = nullptr;
   auto result = ::sqlite3_exec(db, zSql, xCallback, pArg, &errMsg );
   pzErrMsg->reset( errMsg );
   return result;
}

///\brief another C++ overload of the sqlite3 library function, needed to
/// disambiguate overloads
inline int sqlite3_exec(
  sqlite3 *db,                /* The database on which the SQL executes */
  const char *zSql,           /* The SQL to be executed */
  sqlite3_callback xCallback, /* Invoke this callback routine */
  void *pArg,                 /* First argument to xCallback() */
  std::nullptr_t
){
   char **p = nullptr;
   return ::sqlite3_exec(db, zSql, xCallback, pArg, p);
}

#endif
