//
//  UserException.cpp
//  
//
//  Created by Paul Licameli on 11/27/16.
//
//

#include "Audacity.h"
#include "UserException.h"

UserException::~UserException()
{
}

std::unique_ptr< AudacityException > UserException::Move()
{
   return std::unique_ptr< AudacityException >
   { safenew UserException{ std::move( *this ) } };
}

void UserException::DelayedHandlerAction()
{
}
