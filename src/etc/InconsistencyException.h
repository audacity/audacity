//
//  InconsistencyException.h
//  
//
//  Created by Paul Licameli on 11/27/16.
//
//  Some errors that formerly were assertion violations now throw exceptions,
//  even in production code.  These may be violations of function preconditions
//  or the results of logical errors internal to functions.  These conditions
//  are supposed to be deducible statically as never happening.
//

#ifndef __AUDACITY_INCONSISTENCY_EXCEPTION__
#define __AUDACITY_INCONSISTENCY_EXCEPTION__

#include "AudacityException.h"

class InconsistencyException final : public MessageBoxException
{
public:
   InconsistencyException() {}

   explicit InconsistencyException
      ( const char *fn, const char *f, unsigned l )
         : MessageBoxException{ XO("Internal Error") }
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

// This macro constructs this exception type, using C++ preprocessor to identify
// the source code location.

#ifdef __func__

#define CONSTRUCT_INCONSISTENCY_EXCEPTION \
   InconsistencyException( __func__, __FILE__ , __LINE__ )

#else

#define CONSTRUCT_INCONSISTENCY_EXCEPTION \
   InconsistencyException( "", __FILE__ , __LINE__ )

#endif

#define THROW_INCONSISTENCY_EXCEPTION throw CONSTRUCT_INCONSISTENCY_EXCEPTION

#endif
