//
//  NotYetAvailableException.cpp
//  
//
//  Created by Paul Licameli on 12/25/16.
//
//

#include "../Audacity.h"
#include "NotYetAvailableException.h"

NotYetAvailableException::~NotYetAvailableException()
{
}

wxString NotYetAvailableException::ErrorMessage() const
{
   return wxString::Format(
      _("This operation cannot be done until importation of %s completes."),
      fileName.GetFullName()
   );
}
