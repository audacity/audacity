/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.cpp

Paul Licameli

**********************************************************************/
#include "BasicUI.h"

namespace  BasicUI {
Services::~Services() = default;

static Services *theInstance = nullptr;

Services *Get() { return theInstance; }

Services *Install(Services *pInstance)
{
   auto result = theInstance;
   theInstance = pInstance;
   return result;
}
}
