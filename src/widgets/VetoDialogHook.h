/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file VetoDialogHook.h
 @brief Hook function for the screenshot utility, to show modal dialogs only briefly
 
 Paul Licameli
 
 **********************************************************************/

#ifndef __AUDACITY_VETO_DIALOG_HOOK__
#define __AUDACITY_VETO_DIALOG_HOOK__

class wxDialog;

//! Type of a registered function that, if it returns true, causes only brief display of certain modal dialogs
using VetoDialogHook = bool (*) ( wxDialog* );

//! Install a hook function, returning the previously installed
AUDACITY_DLL_API VetoDialogHook SetVetoDialogHook( VetoDialogHook hook );

//! Invoke the currently installed hook function, or return false if there is none
AUDACITY_DLL_API bool CallVetoDialogHook( wxDialog *pDialog );

#endif
