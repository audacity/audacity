/*!********************************************************************

Audacity: A Digital Audio Editor

@file Beats.cpp

Paul Licameli
Michael Papadopoulos split from Decibels.cpp

**********************************************************************/
#include "Beats.h"

DoubleSetting BeatsPerMinute{ L"/GUI/BPM", 120.0 };
IntSetting UpperTimeSignature{ L"/GUI/UpperTimeSig", 4 };
IntSetting LowerTimeSignature{ L"/GUI/LowerTimeSig", 4 };
