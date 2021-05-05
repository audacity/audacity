/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file TelemetryManager.h
 @brief Declare an API for reporting telemetry events.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include "TelemetryApi.h"

#include <string>
#include <mutex>
#include <unordered_map>

namespace audacity
{
namespace telemetry
{
enum class TelemetryService
{
    // Measurement protocol for the "Universal Analytics" by Goggle.
    GoogleAnalytics_UA,
};

enum class UserTrackingService
{
    YandexMetrica
};

enum class BuiltinCategory
{
    Effect,
    Analyzer,
    AudioGenerator,
    Tool,
    AudioExport,
    AudioImport,
};

TELEMETRY_API void Initialize (const std::string& appName, const std::string& appVersion, std::string tempDir, bool enabled);
TELEMETRY_API void Terminate ();

TELEMETRY_API std::string GetClientID ();

TELEMETRY_API void SetTelemetryEnabled (bool enabled);
TELEMETRY_API bool IsTelemetryEnabled ();

TELEMETRY_API void SetTempPath (std::string tempDir);

TELEMETRY_API void SetTelemetryService (TelemetryService service, const std::string& configuration);
TELEMETRY_API void SetUserTrackingService (UserTrackingService service, const std::string& configuration);

TELEMETRY_API void ReportScreenView (const std::string& screenName);

TELEMETRY_API void ReportException (const std::string& exceptionMessage, bool isFatal = false);

TELEMETRY_API void ReportBuiltinEvent (BuiltinCategory category, const std::string& name, const std::string& action);

TELEMETRY_API void ReportCustomEvent (
    const std::string& category,
    const std::string& name,
    const std::string& action,
    const std::unordered_map<std::string, std::string>& params = {}
);


}
}
