#pragma once

#include "VersionId.h"
#include "VersionPatch.h"
#include "ServerCommunication.h"
#include "UpdateDataParser.h"

#include "Project.h"
#include "Prefs.h"

#include <wx/string.h>
#include <wx/event.h>
#include <wx/timer.h>

class UpdateManager final : public wxEvtHandler
{
public:
	UpdateManager(AudacityProject& project);
	~UpdateManager();

	void getUpdates();

	void enableNotification(bool enable);
	bool isNotificationEnabled();

	VersionPatch getVersionPatch() const;

	void OnTimer(wxTimerEvent& event);

	DECLARE_EVENT_TABLE()

private:
	VersionPatch mVersionPatch;

	wxWindow* mParent;
	wxTimer mTimer;

	// TODO: recalc to each 12 hrs per twenty-four hours
	enum { kTimerInterval = 1000 * 60 };
};
