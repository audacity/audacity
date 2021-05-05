/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file TelemetryHelper.h
 @brief Declare a helper class, used for telemetry initialization.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include "Prefs.h"
class TelemetryHelper final : private PrefsListener
{
public:
    TelemetryHelper ();
    ~TelemetryHelper ();
private:
	void UpdatePrefs () override;
};