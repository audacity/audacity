#include "TelemetryHelper.h"


#include "TempDirectory.h"

#include "lib-telemetry/TelemetryManager.h"
#include "lib-string-utils/CodeConversions.h"

static const char* prefsKey = "/Telemetry/Enabled";

TelemetryHelper::TelemetryHelper ()
{
    audacity::telemetry::Initialize (
        "Audacity",
        audacity::ToUTF8 (AUDACITY_VERSION_STRING),
        audacity::ToUTF8 (TempDirectory::TempDir ()),
        gPrefs->ReadBool(prefsKey, true)
    );

    audacity::telemetry::SetService (audacity::telemetry::Service::GoogleAnalytics_UA, "UA-193331022-1");
}

TelemetryHelper::~TelemetryHelper ()
{
    audacity::telemetry::Terminate ();
}

void TelemetryHelper::UpdatePrefs ()
{
    audacity::telemetry::SetTelemetryEnabled (gPrefs->ReadBool (prefsKey, true));
}
