/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.h
 @brief Declare a class that handles managing of updates.

 Anton Gerasimov
 **********************************************************************/
#pragma once

#include "VersionId.h"
#include "UpdateDataStructures.h"
#include "UpdateDataParser.h"

#include "Prefs.h"
#include "BasicUI.h"

#include <wx/string.h>
#include <wx/event.h>
#include <wx/timer.h>
#include <wx/progdlg.h>

#include <iostream>
#include <fstream>
#include <mutex>

/// A class that managing of updates.
/**
    Opt-in request and show update dialog by the scheduled time.
    Have a built-in check that allows avoiding multiplying update notifications
    when multiple Audacity windows are shown.
*/
class UpdateManager final : public wxEvtHandler, public PrefsListener
{
public:
    UpdateManager();

    static UpdateManager& GetInstance();
    static void Start(bool suppressModal);

    void GetUpdates(bool ignoreNetworkErrors, bool configurableNotification);

    VersionPatch GetVersionPatch() const;

    std::vector<Notification> GetActiveNotifications() const;
    void ShowNotifications();
    bool IsNotificationShown(const wxString& uuid) const;
    void MarkNotificationAsShown(const wxString& uuid);

    // PrefsListener implementation
    void UpdatePrefs() override;

private:
    UpdateDataParser mUpdateDataParser;
    UpdateDataFeed mUpdateDataFeed;

    wxTimer mTimer;

    void OnTimer(wxTimerEvent& event);

    /// Scheduling update time for avoiding multiplying update notifications.
    bool IsTimeForUpdatesChecking();

    void SendOptOutRequest() const;

    std::string GetUpdatesUrl() const;
    std::string GetOptOutUrl() const;


    std::vector<wxString> GetShownNotificationUUIDs() const;

    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;

    std::string mAudacityInstallerPath;
    std::ofstream mAudacityInstaller;

    std::mutex mUpdateMutex;
    bool mOnProgress{ false };
    bool mSendAnonymousUsageInfo{ false };

public:
    DECLARE_EVENT_TABLE()
};
