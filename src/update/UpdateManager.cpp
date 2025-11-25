/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.cpp
 @brief Declare a class that handles managing of updates.

 Anton Gerasimov
 **********************************************************************/

#include "UpdateManager.h"

#include "Prefs.h"
#include "UpdatePopupDialog.h"
#include "UpdateNoticeDialog.h"
#include "UpdateNotificationDialog.h"
#include "CustomNotificationRegistry.h"
#include "NoUpdatesAvailableDialog.h"

#include "AudioIO.h"
#include "BasicUI.h"
#include "HeadersList.h"
#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"
#include "Uuid.h"

#include <wx/arrstr.h>
#include <wx/log.h>
#include <wx/utils.h>
#include <wx/frame.h>
#include <wx/app.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include <wx/tokenzr.h>

#include <rapidjson/document.h>
#include <rapidjson/stringbuffer.h>
#include <rapidjson/writer.h>

#include <cstdint>

#define UPDATE_LOCAL_TESTING 0

static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";
static BoolSetting prefUpdatesNoticeShown { wxT("/Update/UpdateNoticeShown"), false };
static StringSetting prefShownNotifications { wxT("/Update/ShownNotifications"), wxEmptyString };
static StringSetting perfAudioComUserId { wxT("/cloud/audiocom/userId"), wxEmptyString };

using Clock = std::chrono::system_clock;
using TimePoint = Clock::time_point;
using Duration = TimePoint::duration;

#if UPDATE_LOCAL_TESTING == 1
constexpr Duration updatesCheckInterval = std::chrono::minutes(1);
#else
constexpr Duration updatesCheckInterval = std::chrono::hours(12);
#endif

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager& UpdateManager::GetInstance()
{
    static UpdateManager updateManager;

    return updateManager;
}

void UpdateManager::Start(bool suppressModal)
{
    auto& instance = GetInstance();

    if (!suppressModal && !prefUpdatesNoticeShown.Read())
    {
        // DefaultUpdatesCheckingFlag survives the "Reset Preferences"
        // action, so check, if the updates were previously disabled as well.
        if (DefaultUpdatesCheckingFlag->Read())
        {
            UpdateNoticeDialog notice(nullptr);

            int result = notice.ShowModal();
            SendAnonymousUsageInfo->Write(result != wxNO);
            GetInstance().UpdatePrefs();
        }

        prefUpdatesNoticeShown.Write(true);
        gPrefs->Flush();
    }

    static std::once_flag flag;
    std::call_once(flag, [&instance] {
        instance.mTimer.SetOwner(&instance, ID_TIMER);
        instance.mTimer.StartOnce(1);
        });
}

VersionPatch UpdateManager::GetVersionPatch() const
{
    return mUpdateDataFeed.versionPatch;
}

void UpdateManager::GetUpdates(bool ignoreNetworkErrors, bool configurableNotification)
{

    const audacity::network_manager::Request request(GetUpdatesUrl());
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    mUpdateDataFeed = {};
    response->setRequestFinishedCallback([response, ignoreNetworkErrors, configurableNotification, this](audacity::network_manager::IResponse*) {

        // We don't' want to duplicate the updates checking if that already launched.
        {
            std::lock_guard<std::mutex> lock(mUpdateMutex);
            if (mOnProgress)
            {
                response->abort();
                return;
            }
            mOnProgress = true;
        }

        using namespace BasicUI;
        auto gAudioIO = AudioIO::Get();
        if (response->getError() != audacity::network_manager::NetworkError::NoError)
        {
           if (!ignoreNetworkErrors)
           {
              gAudioIO->CallAfterRecording([] {ShowErrorDialog( {},
                 XC("Error checking for update", "update dialog"),
                 XC("Unable to connect to Audacity update server.", "update dialog"),
                 wxString(),
                 ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
              });
           }

           mOnProgress = false;
           return;
        }

        if (!mUpdateDataParser.Parse(response->readAll<UpdateDataFeed::UpdateDataFormat>(), &mUpdateDataFeed))
        {
           if (!ignoreNetworkErrors)
           {
              gAudioIO->CallAfterRecording([] {ShowErrorDialog( {},
                 XC("Error checking for update", "update dialog"),
                 XC("Update data was corrupted.", "update dialog"),
                 wxString(),
                 ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
              });
           }

           mOnProgress = false;
           return;
        }

#if UPDATE_LOCAL_TESTING == 0
        if (mUpdateDataFeed.versionPatch.version > CurrentBuildVersion())
#endif
        {
            gAudioIO->CallAfterRecording([this, ignoreNetworkErrors, configurableNotification] {
                UpdatePopupDialog dlg(nullptr, mUpdateDataFeed.versionPatch, configurableNotification);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    const audacity::network_manager::Request downloadRequest(mUpdateDataFeed.versionPatch.download.ToStdString());
                    auto downloadResponse = audacity::network_manager::NetworkManager::GetInstance().doGet(downloadRequest);

                    // Called once, when downloading is real will finish.
                    downloadResponse->setRequestFinishedCallback([downloadResponse, ignoreNetworkErrors, this](audacity::network_manager::IResponse*) {
                        // First - close all opened resources.
                        wxTheApp->CallAfter([this]{ mProgressDialog.reset(); });

                        if (mAudacityInstaller.is_open())
                            mAudacityInstaller.close();

                        if (downloadResponse->getError() != audacity::network_manager::NetworkError::NoError)
                        {
                            if (!ignoreNetworkErrors)
                            {
                                wxTheApp->CallAfter([] {ShowErrorDialog( {},
                                  XC("Error downloading update", "update dialog"),
                                  XC("Can't open the Audacity download link.", "update dialog"),
                                  wxString(),
                                  ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
                               });
                            }

                            mOnProgress = false;
                            return;
                        }

                        const wxPlatformInfo& info = wxPlatformInfo::Get();
                        if ((info.GetOperatingSystemId() & wxOS_WINDOWS) ||
                            info.GetOperatingSystemId() & wxOS_MAC)
                        {
                            if (wxFileName(mAudacityInstallerPath).Exists())
                            {
                                std::string cmd = info.GetOperatingSystemId() & wxOS_MAC ? "Open " + mAudacityInstallerPath : mAudacityInstallerPath;
                                wxTheApp->CallAfter([cmd] { wxExecute(cmd, wxEXEC_ASYNC); });
                            }
                        }

                        mOnProgress = false;
                        }
                    );

                    auto audacityPatchFilename = wxFileName(mUpdateDataFeed.versionPatch.download).GetName();

                    mProgressDialog = BasicUI::MakeProgress(XO("Audacity update"), XO("Downloading %s").Format(audacityPatchFilename));
                    wxASSERT(mProgressDialog);

                    wxString installerExtension;
                    const wxPlatformInfo& info = wxPlatformInfo::Get();
                    if (info.GetOperatingSystemId() & wxOS_WINDOWS)
                        installerExtension = "exe";
                    else if(info.GetOperatingSystemId() & wxOS_MAC)
                        installerExtension = "dmg";

                    mAudacityInstallerPath = wxFileName(
                        wxStandardPaths::Get().GetUserDir(wxStandardPaths::Dir_Downloads), audacityPatchFilename, installerExtension)
                        .GetFullPath()
                        .ToStdString();

                    mAudacityInstaller.open(mAudacityInstallerPath, std::ios::binary);

                    // Called each time, since downloading for update progress status.
                    downloadResponse->setDownloadProgressCallback(
                        [this](int64_t current, int64_t expected) {

                        wxTheApp->CallAfter([this, current, expected]{
                            if(mProgressDialog != nullptr)
                                mProgressDialog->Poll(current, expected);
                        });
                    }
                    );

                    // Called each time, since downloading for get data.
                    downloadResponse->setOnDataReceivedCallback([downloadResponse, this](audacity::network_manager::IResponse*) {
                        if (downloadResponse->getError() == audacity::network_manager::NetworkError::NoError)
                        {
                            std::vector<char> buffer(downloadResponse->getBytesAvailable());

                            size_t bytes = downloadResponse->readData(buffer.data(), buffer.size());

                            if (mAudacityInstaller.is_open())
                                mAudacityInstaller.write(buffer.data(), buffer.size());
                        }
                        }
                    );

                }
                else if (code == wxID_NO)
                {
                    mOnProgress = false;
                }

                ShowNotifications();
            });
        }
#if UPDATE_LOCAL_TESTING == 0
        else // mVersionPatch.version > CurrentBuildVersion()
        {
            // That also shows, that updates checking was called manually from menu.
            if (!configurableNotification)
            {
                gAudioIO->CallAfterRecording([this] {
                    NoUpdatesAvailableDialog(nullptr).ShowModal();

                    // Show notifications after no updates dialog is dismissed
                    ShowNotifications();
                    });
            }
            else
            {
                // No dialog shown, but we can still show notifications
                wxTheApp->CallAfter([this] {
                    ShowNotifications();
                });
            }

            mOnProgress = false;
        }
#endif
    });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    bool updatesCheckingEnabled = DefaultUpdatesCheckingFlag->Read();

#if UPDATE_LOCAL_TESTING == 0
    if (updatesCheckingEnabled && IsTimeForUpdatesChecking())
#endif
    {
        GetUpdates(true, true);
    }

    mTimer.StartOnce(std::chrono::duration_cast<std::chrono::milliseconds>(
                        updatesCheckInterval)
                        .count());
}

bool UpdateManager::IsTimeForUpdatesChecking()
{
    // We use atoll here, so there is no need to handle the exception,
    // if prefsUpdateScheduledTime is corrupted.
    // atoll will return 0 on failure, which suits us well.
    const TimePoint nextUpdatesCheckingTime(std::chrono::milliseconds(
       atoll(gPrefs->Read(prefsUpdateScheduledTime, "0").c_str())));

    // Get current time
    const TimePoint currentTime = Clock::now();

    // If next update time 0 or less then current time -> show update dialog,
    // else this condition allow us to avoid from duplicating update notifications.
    if (nextUpdatesCheckingTime < currentTime)
    {

        // Round down the nextUpdatesChecking time to a day.
        // This is required to ensure, that update is
        // checked daily
        using DayDuration =
          std::chrono::duration<int32_t, std::ratio<60 * 60 * 24>>;

        const auto postponeUpdateUntil =
          std::chrono::time_point_cast<DayDuration>(
           currentTime) + DayDuration(1);

        const std::chrono::milliseconds postponeUpdateUntilMS(
           postponeUpdateUntil.time_since_epoch());

        gPrefs->Write(
           prefsUpdateScheduledTime,
           wxString(std::to_string(postponeUpdateUntilMS.count())));

        gPrefs->Flush();

        return true;
    }

    return false;
}

std::string UpdateManager::GetUpdatesUrl() const
{
   #if AUDACITY_BUILD_LEVEL == 0
      std::string url = "https://updates.audacityteam.org/builds/alpha.xml";
   #elif AUDACITY_BUILD_LEVEL == 1
      std::string url = "https://updates.audacityteam.org/builds/beta.xml";
   #else
      std::string url = "https://updates.audacityteam.org/feed/latest.xml";
   #endif

   if (SendAnonymousUsageInfo->Read())
   {
      url += "?audacity-instance-id=" + InstanceId->Read().ToStdString();

      if (!perfAudioComUserId.Read().IsEmpty())
      {
         url += "&user_id=" + perfAudioComUserId.Read().ToStdString();
      }
   }

   return url;
}

std::string UpdateManager::GetOptOutUrl() const {
   const std::string url = "https://api.audio.com/analytics/audacity-uuid/opt-out";
   return url;
}

void UpdateManager::SendOptOutRequest() const {
   using namespace rapidjson;
   using namespace audacity::network_manager;

   std::string uuid = InstanceId->Read().ToStdString();

   Document document;
   document.SetObject().AddMember("uuid",
      Value(uuid.c_str(), document.GetAllocator()),
      document.GetAllocator());

   StringBuffer buffer;
   Writer<StringBuffer> writer(buffer);
   document.Accept(writer);

   Request request(GetOptOutUrl());

   request.setHeader(common_headers::ContentType, common_content_types::ApplicationJson);
   request.setHeader(common_headers::Accept, common_content_types::ApplicationJson);

   auto response = NetworkManager::GetInstance().doPost(request, buffer.GetString(), buffer.GetSize());

   response->setRequestFinishedCallback([response](auto) {
      if (response->getHTTPCode() != HttpCode::OK) {
         wxLogWarning(wxT("Failed to communicate with Audio.com. Error code: %d."), response->getHTTPCode());
         wxLogWarning(wxT("Server response: %s"), response->readAll<std::string>());
      }
   });
}

void UpdateManager::UpdatePrefs()
{
   //! Create the instance id if the user allowed it and it was not created before
   if (SendAnonymousUsageInfo->Read()) {
      if(InstanceId->Read().IsEmpty()) {
         InstanceId->Write(audacity::Uuid::Generate().ToHexString());
         gPrefs->Flush();
      }
   }
   else {
      SendOptOutRequest();
   }
}

std::vector<Notification> UpdateManager::GetActiveNotifications() const
{
   std::vector<Notification> active;
   const wxDateTime now = wxDateTime::Now();

   for (const auto& notification : mUpdateDataFeed.notifications)
   {
      if (notification.IsActive(now))
      {
         active.push_back(notification);
      }
   }

   return active;
}

std::vector<wxString> UpdateManager::GetShownNotificationUUIDs() const
{
   wxString shownUUIDs = prefShownNotifications.Read();
   std::vector<wxString> result;

   if (shownUUIDs.IsEmpty())
      return result;

   // Notifications UUIDs are comma-separated string
   wxStringTokenizer tokenizer(shownUUIDs, wxT(","));
   while (tokenizer.HasMoreTokens())
   {
      wxString uuid = tokenizer.GetNextToken();
      uuid.Trim(true).Trim(false);
      if (!uuid.IsEmpty())
         result.push_back(uuid);
   }

   return result;
}

bool UpdateManager::IsNotificationShown(const wxString& uuid) const
{
   auto shownUUIDs = GetShownNotificationUUIDs();
   return std::find(shownUUIDs.begin(), shownUUIDs.end(), uuid) != shownUUIDs.end();
}

void UpdateManager::MarkNotificationAsShown(const wxString& uuid)
{
   auto shownUUIDs = GetShownNotificationUUIDs();

   // Do not add duplicates
   if (std::find(shownUUIDs.begin(), shownUUIDs.end(), uuid) != shownUUIDs.end())
      return;

   shownUUIDs.push_back(uuid);

   wxString uuidString;
   for (size_t i = 0; i < shownUUIDs.size(); i++)
   {
      if (i > 0)
         uuidString += wxT(",");
      uuidString += shownUUIDs[i];
   }

   prefShownNotifications.Write(uuidString);
   gPrefs->Flush();
}

void UpdateManager::ShowNotifications()
{
   auto activeNotifications = GetActiveNotifications();

   for (const auto& notification : activeNotifications)
   {
      if (IsNotificationShown(notification.uuid))
         continue;

      if (CustomNotificationRegistry::HasCustomDialog(notification.uuid))
      {
         int result = CustomNotificationRegistry::ShowCustomDialog(nullptr, notification);
         if (result != wxID_APPLY && !IsNotificationShown(notification.uuid))
            MarkNotificationAsShown(notification.uuid);
         continue;
      }

      UpdateNotificationDialog dlg(nullptr, notification);

      dlg.Bind(EVT_NOTIFICATION_DISMISSED, [this](wxCommandEvent& evt) {
         MarkNotificationAsShown(evt.GetString());
      });

      dlg.Bind(EVT_NOTIFICATION_REMIND_LATER, [](wxCommandEvent&) {
         // If remind later, close the dialog, and show it again next time
      });

      int result = dlg.ShowModal();

      if ((result == wxID_OK || result == wxID_CANCEL) && !IsNotificationShown(notification.uuid))
         MarkNotificationAsShown(notification.uuid);
   }
}
