/*!********************************************************************

Audacity: A Digital Audio Editor

@file Decibels.h

Paul Licameli
   Moved a constant here from Envelope.h where it did not belong
   Define the key string in one place here too
 
   Split from GUISettings.h

**********************************************************************/
#ifndef __AUDACITY_DECIBELS__
#define __AUDACITY_DECIBELS__

#include "Prefs.h"

//! Negation of this value is the lowest dB level that should be shown in dB scales
extern SCREEN_GEOMETRY_API IntSetting DecibelScaleCutoff;

#endif
