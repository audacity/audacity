/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file VetoDialogHook.cpp
 @brief Implements GetVetoDialogHook, SetVetoDialogHook
 
 Paul Licameli
 
 **********************************************************************/

#include "VetoDialogHook.h"

namespace {

VetoDialogHook &Hook()
{
   static VetoDialogHook sHook = nullptr;
   return sHook;
}

}

VetoDialogHook SetVetoDialogHook( VetoDialogHook hook )
{
   auto &theHook = Hook();
   auto result = theHook;
   theHook = hook;
   return result;
}

bool CallVetoDialogHook( wxDialog *pDialog )
{
   auto hook = Hook();
   return hook && hook(pDialog);
}
