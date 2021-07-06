/*!
 @file UserException.h
 @brief An SneedacityException with no visible message

 Created by Paul Licameli on 11/27/16.

*/

#ifndef __SNEEDACITY_USER_EXCEPTION__
#define __SNEEDACITY_USER_EXCEPTION__

#include "SneedacityException.h"

 //! Can be thrown when user cancels operations, as with a progress dialog.  Delayed handler does nothing
/*! This class does not inherit from MessageBoxException. */
class SNEEDACITY_DLL_API UserException final : public SneedacityException
{
public:
   UserException() {}

   ~UserException() override;

   void DelayedHandlerAction() override;
};

#endif
