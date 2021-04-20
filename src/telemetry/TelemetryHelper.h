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