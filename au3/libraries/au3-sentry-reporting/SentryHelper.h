/**********************************************************************

  Audacity: A Digital Audio Editor

  SentryHelper.h

  Defines a macro ADD_EXCEPTION_CONTEXT, that is a no op if Sentry reporting is disabled.

  Dmitry Vedenko

**********************************************************************/

#ifndef __AUDACITY_SENTRY__
#define __AUDACITY_SENTRY__

#ifdef HAS_SENTRY_REPORTING
#     include "SentryReport.h"

#   define ADD_EXCEPTION_CONTEXT(name, value) audacity::sentry::AddExceptionContext(name, value)
#else
#   define ADD_EXCEPTION_CONTEXT(name, value)
#endif // HAS_SENTRY_REPORTING

#endif /* __AUDACITY_SENTRY__ */
