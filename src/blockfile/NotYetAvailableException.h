//
//  NotYetAvailableException.h
//  
//
//  Created by Paul Licameli on 12/25/16.
//
//

#ifndef __AUDACITY_NOT_YET_AVAILABLE_EXCEPTION__
#define __AUDACITY_NOT_YET_AVAILABLE_EXCEPTION__

#include "../FileException.h"

// This exception can be thrown when attempting read of on-demand block files
// that have not yet completed loading.
class NotYetAvailableException final : public FileException
{
public:
   NotYetAvailableException( const wxFileName &fileName )
      : FileException{ Cause::Read, fileName } {}
   ~NotYetAvailableException();

protected:
   TranslatableString ErrorMessage() const override;
};

#endif
