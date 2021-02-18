/*!
  @file InconsistencyException.h
  @brief MessageBoxException for violation of preconditions or assertions

  Created by Paul Licameli on 11/27/16.

*/

#ifndef __AUDACITY_INCONSISTENCY_EXCEPTION__
#define __AUDACITY_INCONSISTENCY_EXCEPTION__

#include "AudacityException.h"

//! Exception that should be impossible in production, thrown only from provably unreachable places
/*!  Some errors that formerly were assertion violations now throw exceptions,
  even in production code.  These may be violations of function preconditions
  or the results of logical errors internal to functions.  These conditions
  are supposed to be deducible statically as never happening.
 
  The error message identifies source file and line number, possibly the function too (depending on
  the compiler), and suggests that the user inform the development team.
 */
class EXCEPTIONS_API InconsistencyException final : public MessageBoxException
{
public:
   InconsistencyException () 
       : MessageBoxException{ ExceptionType::Internal, XO ("Internal Error") } 
   {}

   //! Don't call this directly but use @ref CONSTRUCT_INCONSISTENCY_EXCEPTION or @ref THROW_INCONSISTENCY_EXCEPTION
   explicit InconsistencyException(
      const char *fn, //!< file name supplied by preprocessor
      const char *f, //!< function name supplied by preprocessor
      unsigned l //!< line number supplied by preprocessor
   )
       : MessageBoxException { ExceptionType::Internal, XO("Internal Error") }
       , func { fn }, file { f }, line { l }
   {}

   InconsistencyException(InconsistencyException&& that)
      : MessageBoxException(std::move(that))
      , func{ that.func }
      , file{ that.file }
      , line{ that.line }
   {}

   ~InconsistencyException() override;

   unsigned GetLine() const { return line; }

private:
   // Format a default, internationalized error message for this exception.
   TranslatableString ErrorMessage() const override;

   const char *func {};
   const char *file {};
   unsigned line {};
};

#ifdef __func__

#define CONSTRUCT_INCONSISTENCY_EXCEPTION \
   InconsistencyException( __func__, __FILE__ , __LINE__ )

#else

/*! @def CONSTRUCT_INCONSISTENCY_EXCEPTION
@brief Construct InconsistencyException, using C++ preprocessor to identify the source code location

For cases where the exception object is not immediately thrown */
#define CONSTRUCT_INCONSISTENCY_EXCEPTION \
   InconsistencyException( "", __FILE__ , __LINE__ )

#endif

/*! @def THROW_INCONSISTENCY_EXCEPTION
@brief Throw InconsistencyException, using C++ preprocessor to identify the source code location
*/
#define THROW_INCONSISTENCY_EXCEPTION throw CONSTRUCT_INCONSISTENCY_EXCEPTION

#endif
