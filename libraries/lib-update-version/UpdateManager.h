#pragma once

#include "update/VersionPatch.h"

#include "xml/XMLTagHandler.h"
#include "Project.h"
#include "Prefs.h"

#include <wx/string.h>
#include <wx/event.h>
#include <wx/timer.h>

#include <map>

class UpdateManager final : public XMLTagHandler,
	                        public wxEvtHandler
{
public:
	UpdateManager (AudacityProject& project);
	~UpdateManager();

	void getUpdates();

	void enableNotification (bool enable);
	bool isNotificationEnabled ();

	VersionPatch getVersionPatch() const;

	void OnTimer (wxTimerEvent& event);

	DECLARE_EVENT_TABLE()
	
private:
	bool requestUpdateData (std::string *response);
	bool parseUpdateData (const std::string& updateInfo);

	enum class XmlParsedTags : int
	{
		kNotUsedTag,
		kUpdateTag,
		kDescriptionTag,
		kOsTag,
		kWindowsTag,
		kMacosTag,
		kLinuxTag,
		kVersionTag,
		kLinkTag
	};
	XmlParsedTags mXmlParsingState { XmlParsedTags::kNotUsedTag };
	std::map<XmlParsedTags, wxString> mXmlTagNames
	{
		{XmlParsedTags::kUpdateTag, wxT("Updates")},
		{XmlParsedTags::kDescriptionTag, wxT("Description")},
		{XmlParsedTags::kOsTag, wxT("OS")},
		{XmlParsedTags::kWindowsTag, wxT("Windows")},
		{XmlParsedTags::kMacosTag, wxT("Macos")},
		{XmlParsedTags::kLinuxTag, wxT("Linux")},
		{XmlParsedTags::kVersionTag, wxT("Version")},
		{XmlParsedTags::kLinkTag, wxT("Link")},
	};

	bool HandleXMLTag (const wxChar* tag, const wxChar** attrs) override;
	void HandleXMLEndTag (const wxChar* tag) override;
	void HandleXMLContent (const wxString& content) override;
	XMLTagHandler* HandleXMLChild (const wxChar* tag) override;

	VersionPatch mVersionPatch;

	wxWindow* mParent;
	wxTimer mTimer;
	enum { kTimerInterval = 1000 * 60 };
};
