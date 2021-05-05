/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file TelemetryHelper.h
 @brief Define a helper class, used for telemetry initialization.

 Dmitry Vedenko
 **********************************************************************/

#include "TelemetryHelper.h"

#include "TempDirectory.h"

#include "lib-telemetry/TelemetryManager.h"
#include "lib-string-utils/CodeConversions.h"

#include "TelemetryDialog.h"

static const char* prefsKeyEnabled = "/Telemetry/Enabled";
static const char* prefsKeyDialogShown = "/Telemetry/PersmissionDialogShown";

TelemetryHelper::TelemetryHelper ()
{
    using namespace audacity;

    if (!gPrefs->ReadBool (prefsKeyDialogShown, false))
    {
        TelemetryDialog dlg (nullptr);
        const int code = dlg.ShowModal ();

        gPrefs->Write (prefsKeyDialogShown, true);
        gPrefs->Write (prefsKeyEnabled, code == wxID_YES);
    }

    telemetry::Initialize (
        "Audacity",
        ToUTF8 (AUDACITY_VERSION_STRING),
        ToUTF8 (TempDirectory::TempDir ()),
        gPrefs->ReadBool(prefsKeyEnabled, false)
    );

#ifdef GOOGLE_UA_TRACKING_ID
    if (*(GOOGLE_UA_TRACKING_ID) != '\0')
        telemetry::SetTelemetryService (telemetry::TelemetryService::GoogleAnalytics_UA, GOOGLE_UA_TRACKING_ID);
#endif

#ifdef METRICA_TRACKING_ID
    if (*(METRICA_TRACKING_ID) != '\0')
        telemetry::SetUserTrackingService (telemetry::UserTrackingService::YandexMetrica, METRICA_TRACKING_ID);
#endif
}

TelemetryHelper::~TelemetryHelper ()
{
    audacity::telemetry::Terminate ();
}

void TelemetryHelper::UpdatePrefs ()
{
    audacity::telemetry::SetTelemetryEnabled (gPrefs->ReadBool (prefsKeyEnabled, true));
}
