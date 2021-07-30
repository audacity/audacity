/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleConstants.cpp

  Paul Licameli

**********************************************************************/

#include "ModuleConstants.h"

// We want Audacity with a capital 'A'
// DA: App name
const std::wstring AppName =
#ifndef EXPERIMENTAL_DA
   L"Audacity"
#else
   L"DarkAudacity"
#endif
;
