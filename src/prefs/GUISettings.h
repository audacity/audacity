/**********************************************************************

Audacity: A Digital Audio Editor

GUISettings.h

Paul Licameli
   Moved a constant here from Envelope.h where it did not belong
   Define the key string in one place here too

**********************************************************************/
#ifndef __AUDACITY_GUI_SETTINGS__
#define __AUDACITY_GUI_SETTINGS__

#define ENV_DB_KEY (wxT("/GUI/EnvdBRange"))
#define ENV_DB_RANGE 60

// Right to left languages fail in many wx3 dialogs with missing buttons.
// The workaround is to use LTR in those dialogs.
#ifndef __WXMAC__
#define RTL_WORKAROUND( pWnd ) \
   if ( gPrefs->Read( "/GUI/RtlWorkaround", true) ) \
       pWnd->SetLayoutDirection(wxLayout_LeftToRight);
#else
   #define RTL_WORKAROUND( pWnd )
#endif

#endif
