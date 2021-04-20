#pragma once

#include "TelemetryApi.h"

#include <string>
#include <mutex>
#include <unordered_map>

namespace audacity
{
namespace telemetry
{
enum class Service
{
    // Measurement protocol for the "Universal Analytics" by Goggle.
    GoogleAnalytics_UA  
};

enum class BuiltinCategory
{
    EffectApplied,
    EffectAddedToStack,
    AnalyzerUsed,
    AudioGenerated,
    AudioExport,
    AudioImport,
    Track,
};

TELEMETRY_API void Initialize (const std::string& appName, const std::string& appVersion, std::string tempDir, bool enabled);
TELEMETRY_API void Terminate ();

TELEMETRY_API std::string GetClientID ();

TELEMETRY_API void SetTelemetryEnabled (bool enabled);
TELEMETRY_API bool IsTelemetryEnambled ();

TELEMETRY_API void SetTempPath (std::string tempDir);

TELEMETRY_API void SetService (Service service, const std::string& configuration);

TELEMETRY_API void ReportScreenView (const std::string& screenName);

TELEMETRY_API void ReportException (const std::string& exceptionMessage, bool isFatal = false);

TELEMETRY_API void ReportEvent (BuiltinCategory category, const std::string& name, const std::string& action);

TELEMETRY_API void ReportCustomEvent (
    const std::string& category,
    const std::string& name,
    const std::string& action,
    const std::unordered_map<std::string, std::string>& params = {}
);


}
}
