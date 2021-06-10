/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.h
 @brief Declare a class that managing of updates.

 Anton Gerasimov
 **********************************************************************/
#pragma once

#include "VersionId.h"
#include "VersionPatch.h"
#include "UpdateDataParser.h"

#include "Project.h"
#include "Prefs.h"

#include <wx/string.h>
#include <wx/event.h>
#include <wx/timer.h>

/// A class that managing of updates.
/**
    Opt-in request and show update dialog by the scheduled time.
    Have a built-in check that allows avoiding multiplying update notifications
    when multiple Audacity windows are shown.
*/
class UpdateManager final : public wxEvtHandler
{
public:
    UpdateManager(AudacityProject& project);
    ~UpdateManager();

    void getUpdates();

    void enableTracking(bool enable);
    bool isTrackingEnabled();

    VersionPatch getVersionPatch() const;

private:
    UpdateDataParser mUpdateDataParser;
    VersionPatch mVersionPatch;

    wxWindow* mParent;

    wxTimer mTimer;
    const int mTrackingInterval;

    void OnTimer(wxTimerEvent& event);

	/// Scheduling update time for avoiding multiplying update notifications.
    bool isTimeToUpdate();

public:
    DECLARE_EVENT_TABLE()
};
