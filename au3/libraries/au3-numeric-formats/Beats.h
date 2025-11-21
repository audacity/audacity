/*!********************************************************************

Audacity: A Digital Audio Editor

@file Beats.h

Paul Licameli
Michael Papadopoulos split from Decibels.h

**********************************************************************/
#ifndef __AUDACITY_BEATS__
#define __AUDACITY_BEATS__

#include "Prefs.h"

//! The tempo used for drawing the Beats & Measures ruler
extern NUMERIC_FORMATS_API DoubleSetting BeatsPerMinute;
//! The upper time signature of the Beats & Measures ruler
extern NUMERIC_FORMATS_API IntSetting UpperTimeSignature;
//! The lower time signature of the Beats & Measures ruler
extern NUMERIC_FORMATS_API IntSetting LowerTimeSignature;

#endif
