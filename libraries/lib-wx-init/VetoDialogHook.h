/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file VetoDialogHook.h
 @brief Hook function for the screenshot utility, to show dialogs only briefly
 
 Paul Licameli
 
 **********************************************************************/

#ifndef __AUDACITY_VETO_DIALOG_HOOK__
#define __AUDACITY_VETO_DIALOG_HOOK__

#include "GlobalVariable.h"

class wxDialog;

//! Call before `Show`-ing certain dialogs; don't show if it returns true
struct WX_INIT_API VetoDialogHook : GlobalHook<VetoDialogHook,
   bool( wxDialog* )
> {};

#endif
