//
//  UserException.h
//  
//
//  Created by Paul Licameli on 11/27/16.
//
//  An exception to throw when the user cancels an operation, as for instance
//  with a progress dialog.  Its delayed handler action does nothing.
//

#ifndef __AUDACITY_USER_EXCEPTION__
#define __AUDACITY_USER_EXCEPTION__

#include "AudacityException.h"

// This class does not inherit from MessageBoxException, and it does nothing
// in its delayed handler.  It might be thrown after the user clicks a
// cancel button, as on a progress dialog.
class UserException final : public AudacityException
{
public:
   UserException() {}

   ~UserException() override;

   void DelayedHandlerAction() override;
};

#endif
