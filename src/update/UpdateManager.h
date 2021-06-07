#pragma once

#include "VersionId.h"
#include "VersionPatch.h"
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

	void enableTracking(bool enable);
	bool isTrackingEnabled();

	VersionPatch getVersionPatch() const;

private:
	UpdateDataParser mUpdateDataParser;

	VersionPatch mVersionPatch;

	wxWindow* mParent;

	wxTimer mTimer;

	void OnTimer(wxTimerEvent& event);

public:
	DECLARE_EVENT_TABLE()
};
