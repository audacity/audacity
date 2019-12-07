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

TranslatableString NotYetAvailableException::ErrorMessage() const
{
   return
      XO("This operation cannot be done until importation of %s completes.")
         .Format( fileName.GetFullName() );
}
