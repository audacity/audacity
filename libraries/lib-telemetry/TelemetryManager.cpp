#include "TelemetryManager.h"

#include "lib-string-utils/UrlEncode.h"
#include "lib-string-utils/CodeConversions.h"

#include "lib-uuid/Uuid.h"
#include "lib-timer/Timer.h"

#include <utility>
#include <array>
#include <algorithm>
#include <deque>
#include <mutex>
#include <random>

#include <wx/file.h>
#include <wx/filefn.h> 
#include <wx/filename.h>
#include <wx/tokenzr.h>

#include "details/Event.h"

#include "details/ITelemetryService.h"
#include "details/IUserTrackingService.h"

#include "details/GoogleAnalyticsUA.h"
#include "details/YandexMetricaUserTracking.h"
#include "details/CommonHeaders.h"

namespace audacity
{
namespace telemetry
{
namespace
{

const char* telemetryFileName = "telemetry.cfg";

const char* builtinCategoriesNames[] = {
    "Effect",
    "Analyzer",
    "AudioGenerator",
    "Tool",
    "AudioExport",
    "AudioImport",
};

struct TelemetryManager final
{
    using EventsQueue = std::deque<details::Event>;

    std::string AppName;
    std::string AppVersion;

    std::string TempDir;

    std::string ClientID;
    std::string SessionID;

    std::unique_ptr<details::CommonHeaders> CommonHeaders;

    mutable std::mutex TelemetryMutex;

    EventsQueue QueuedEvents;
    size_t NextSubmissionIndex { 0 };

    std::unique_ptr<details::ITelemetryService> TelemetryService;
    std::unique_ptr<details::IUserTrackingService> UserTtrackingService;

    bool TelemetryEnabled { false };

    void processTimer ()
    {
        std::lock_guard<std::mutex> lock (TelemetryMutex);

        auto firstUnqueuedEventIt = getFirstUnsubmittedEvent ();

        if (!TelemetryEnabled || 
            TelemetryService == nullptr || 
            firstUnqueuedEventIt == QueuedEvents.end ())
        {
            enqueueTimer (false);
            return;
        }

        const size_t consumedEvents = TelemetryService->submitEvents (ClientID, QueuedEvents, 
            [index = NextSubmissionIndex, this](details::SubmissionResult result) {

            std::lock_guard<std::mutex> lock (TelemetryMutex);

            if (result == details::SubmissionResult::Success || 
                result == details::SubmissionResult::Failed)
            {
                QueuedEvents.erase (std::remove_if (QueuedEvents.begin (), QueuedEvents.end (), 
                    [index](const details::Event& evt) {
                    return evt.SumbmissionIndex == index;
                }), QueuedEvents.end ());

                persistTelemetryData ();
            }
            else
            {
                for (details::Event& evt : QueuedEvents)
                {
                    if (evt.SumbmissionIndex == index)
                        evt.Submitted = false;
                }
            }
        });

        for (size_t idx = 0;
            firstUnqueuedEventIt != QueuedEvents.end () && idx < consumedEvents;
            ++firstUnqueuedEventIt, ++idx)
        {
            if (firstUnqueuedEventIt->Submitted)
                continue;

            firstUnqueuedEventIt->SumbmissionIndex = NextSubmissionIndex;
            firstUnqueuedEventIt->Submitted = true;
        }

        NextSubmissionIndex++;
        enqueueTimer (firstUnqueuedEventIt != QueuedEvents.end ());
    }

    void generateClientID ()
    {
        ClientID = Uuid::Generate ().toString ();

        std::lock_guard<std::mutex> lock (TelemetryMutex);

        persistTelemetryData ();
    }

    bool readTelemetryData ()
    {
        wxString fileName = wxFileName (
            wxString::FromUTF8 (TempDir),
            telemetryFileName
        ).GetFullPath ();

        if (!wxFileExists (fileName))
            return false;

        wxFile inputFile (fileName, wxFile::read);

        if (!inputFile.IsOpened ())
            return false;

        wxString content;

        if (!inputFile.ReadAll (&content) || content.empty ())
            return false;

        wxStringTokenizer tokenizer (content, "\r\n");

        ClientID = tokenizer.GetNextToken ();

        std::lock_guard<std::mutex> lock (TelemetryMutex);

        while (tokenizer.HasMoreTokens ())
        {
            wxString evtString = tokenizer.GetNextToken ();

            if (evtString.empty ())
                continue;

            details::Event evt;

            if (evt.deserialize (evtString))
                QueuedEvents.push_back (evt);
        }

        return true;
    }

    bool persistTelemetryData ()
    {
        wxString fileName = wxFileName (
            wxString::FromUTF8 (TempDir),
            telemetryFileName
        ).GetFullPath ();

        wxFile inputFile (fileName, wxFile::write);

        if (!inputFile.IsOpened ())
            return false;

        if (!inputFile.Write (wxString::FromUTF8 (ClientID + "\n")))
            return false;

        for (const details::Event& evt : QueuedEvents)
        {
            if (!inputFile.Write (evt.serialize () + '\n'))
                return false;
        }

        return true;
    }

private:
    EventsQueue::iterator getFirstUnsubmittedEvent () noexcept
    {
        return std::find_if (QueuedEvents.begin (), QueuedEvents.end (), [](const details::Event& evt) {
            return !evt.Submitted;
        });
    }

    std::chrono::milliseconds getNextSubmissionTimeout (bool hasMoreEvents) const noexcept
    {
        static std::mt19937 engine ((std::random_device ())());
        static std::uniform_int_distribution<int> generator (500, 1500);

        const std::chrono::milliseconds randomFactor (generator (engine));

        if (hasMoreEvents)
            return randomFactor;

        return std::chrono::milliseconds (30'000) + randomFactor;
    }

    void enqueueTimer (bool hasMoreEvents)
    {
        Timer::Singleshot (getNextSubmissionTimeout (hasMoreEvents), [this]() {
            processTimer ();
        });
    }
};

TelemetryManager telemetryManager;
}

void Initialize (const std::string& appName, const std::string& appVersion, std::string tempDir, bool enabled)
{
    telemetryManager.AppName = UrlEncode (appName);
    telemetryManager.AppVersion = UrlEncode (appVersion);

    telemetryManager.CommonHeaders = std::make_unique<details::CommonHeaders> (appName, appVersion);

    telemetryManager.TelemetryEnabled = enabled;

    SetTempPath (std::move (tempDir));

    telemetryManager.SessionID = Uuid::Generate ().toString ();

    if (!telemetryManager.readTelemetryData ())
        telemetryManager.generateClientID ();

    const std::chrono::seconds ts (
        std::chrono::duration_cast<std::chrono::seconds> (
            std::chrono::system_clock::now ().time_since_epoch ()
    ));

    ReportCustomEvent ("AppEvent", 
        "",
        "started", {
            { "ts", std::to_string (ts.count ()) },
            { "sid", telemetryManager.SessionID }
    });
}

void Terminate ()
{
    const std::chrono::seconds ts (
        std::chrono::duration_cast<std::chrono::seconds> (
            std::chrono::system_clock::now ().time_since_epoch ()
            ));

    ReportCustomEvent ("AppEvent",
        "",
        "terminated", {
            { "ts", std::to_string (ts.count ()) },
            { "sid", telemetryManager.SessionID }
    });

    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    // TrackingService may block the thread for a short period
    // of time if required.
    if (telemetryManager.UserTtrackingService != nullptr)
        telemetryManager.UserTtrackingService->reportFinished();

    telemetryManager.TelemetryEnabled = false;
    telemetryManager.TelemetryService.reset ();
    telemetryManager.UserTtrackingService.reset ();
    telemetryManager.QueuedEvents.clear ();
}

std::string GetClientID ()
{
    return telemetryManager.ClientID;
}

void SetTelemetryEnabled (bool enabled)
{
    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    if (telemetryManager.TelemetryEnabled != enabled)
    {
        telemetryManager.TelemetryEnabled = enabled;

        if (!enabled)
        {
            telemetryManager.QueuedEvents.clear ();
            telemetryManager.persistTelemetryData ();
        }
    }
}

bool IsTelemetryEnabled ()
{
    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    return telemetryManager.TelemetryEnabled;
}

void SetTempPath (std::string tempDir)
{
    telemetryManager.TempDir = std::move (tempDir);
}

void SetTelemetryService (TelemetryService service, const std::string& configuration)
{
    if (service == TelemetryService::GoogleAnalytics_UA)
    {
        std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

        telemetryManager.TelemetryService = std::make_unique<details::GoogleAnalyticsUA> (
            configuration,
            telemetryManager.CommonHeaders.get ()
        );
    }

    if (telemetryManager.TelemetryService != nullptr)
        telemetryManager.processTimer ();
}

void SetUserTrackingService (UserTrackingService service, const std::string& configuration)
{
    if (service == UserTrackingService::YandexMetrica)
    {
        wxString fileName = wxFileName (
                wxString::FromUTF8 (telemetryManager.TempDir),
                "YM.cfg"
        ).GetFullPath ();

        std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

        telemetryManager.UserTtrackingService = std::make_unique<details::YandexMetricaUserTracking> (
            ToUTF8 (fileName),
            configuration,
            telemetryManager.CommonHeaders.get ()
        );

        telemetryManager.UserTtrackingService->reportAppStarted();
    }
}

void ReportScreenView (const std::string& screenName)
{
    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    if (!telemetryManager.TelemetryEnabled)
        return;

    telemetryManager.QueuedEvents.push_back (details::Event { details::EventType::ScreenView, screenName });

    telemetryManager.persistTelemetryData ();
}

void ReportException (const std::string& exceptionMessage, bool isFatal)
{
    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    if (!telemetryManager.TelemetryEnabled)
        return;

    telemetryManager.QueuedEvents.push_back (details::Event {
        isFatal ? details::EventType::FatalException : details::EventType::Exception,
        exceptionMessage 
    });

    telemetryManager.persistTelemetryData ();
}

void ReportBuiltinEvent (BuiltinCategory category, const std::string& name, const std::string& action)
{
    ReportCustomEvent (
        builtinCategoriesNames[size_t (category)],
        name,
        action
    );
}

void ReportCustomEvent (
    const std::string& category,
    const std::string& name,
    const std::string& action,
    const std::unordered_map<std::string, std::string>& params
)
{
    std::lock_guard<std::mutex> lock (telemetryManager.TelemetryMutex);

    if (!telemetryManager.TelemetryEnabled)
        return;

    telemetryManager.QueuedEvents.push_back (details::Event {
        details::EventType::Event,
        name,
        category,
        action,
        params
    });

    telemetryManager.persistTelemetryData ();
}


}
}
