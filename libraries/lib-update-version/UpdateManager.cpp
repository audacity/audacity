#include "update/UpdateManager.h"
#include "update/UpdatePopupDialog.h"

#include <fstream>
#include <iostream>

#include "xml/XMLFileReader.h"
#include "widgets/AudacityMessageBox.h"

#include <wx/platinfo.h>
#include <wx/utils.h>

static const char* prefsUpdatePopupDialogShown = "/Update/UpdatePopupDialogShown";

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager::UpdateManager (AudacityProject& project)
{
    wxFrame* parrentFrame = project.GetFrame();
    wxASSERT(parrentFrame);

    mParent = reinterpret_cast<wxWindow*> (parrentFrame);

    getUpdates();

    mTimer.SetOwner (this, ID_TIMER);
    mTimer.StartOnce (kTimerInterval);
}

UpdateManager::~UpdateManager()
{}

void UpdateManager::enableNotification (bool enable)
{
    gPrefs->Write (prefsUpdatePopupDialogShown, enable);
}

bool UpdateManager::isNotificationEnabled()
{
    return gPrefs->ReadBool (prefsUpdatePopupDialogShown, false);
}

VersionPatch UpdateManager::getVersionPatch() const
{
    return mVersionPatch;
}

bool UpdateManager::requestUpdateData (std::string* response)
{
    std::ifstream xmlfile ("d:\\xml.log");

    if (xmlfile.is_open())
    {
        std::string line;
        while (std::getline (xmlfile, line))
        {
            response->append (line);
        }
        xmlfile.close();

        return true;
    }

    return false;
}

bool UpdateManager::parseUpdateData (const std::string& updateInfo)
{
    XMLFileReader xmlReader;

    return xmlReader.ParseString (this, updateInfo);
}

void UpdateManager::getUpdates()
{
    std::string response;

    if (!requestUpdateData (&response))
    {
        // TODO: remake text and may be Parrent.
        AudacityMessageBox(
            XO("Unable to connect on Audacity update server."),
            XO("Error update check"),
            wxOK | wxCENTRE,
            mParent);

        return;
    }

    // TODO: check and show MessageBox.
    parseUpdateData (response);

    //if (isNotificationEnabled())
    {
        UpdatePopupDialog dlg (nullptr, this);
        const int code = dlg.ShowModal();

        if (code == wxID_YES)
        {
            if (!wxLaunchDefaultBrowser (mVersionPatch.download))
            {
                // TODO: MessageBox with error;
            }
        }
    }
}

void UpdateManager::OnTimer (wxTimerEvent& event)
{
    getUpdates();

    mTimer.StartOnce (kTimerInterval);
}

bool UpdateManager::HandleXMLTag (const wxChar* tag, const wxChar** attrs)
{
    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kDescriptionTag]) == 0)
    {
        mXmlParsingState = XmlParsedTags::kDescriptionTag;
        return true;
    }

    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kWindowsTag]) == 0)
    {
        if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_WINDOWS)
            mXmlParsingState = XmlParsedTags::kOsTag;
        return true;
    }

    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kMacosTag]) == 0)
    {
        if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_MAC)
            mXmlParsingState = XmlParsedTags::kOsTag;
        return true;
    }

    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kLinuxTag]) == 0)
    {
        if (wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_UNIX_LINUX)
            mXmlParsingState = XmlParsedTags::kOsTag;
        return true;
    }

    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kVersionTag]) == 0)
    {
        if (mXmlParsingState == XmlParsedTags::kOsTag)
            mXmlParsingState = XmlParsedTags::kVersionTag;
        return true;
    }

    if (wxStrcmp (tag, mXmlTagNames[XmlParsedTags::kLinkTag]) == 0)
    {
        if (mXmlParsingState == XmlParsedTags::kOsTag)
            mXmlParsingState = XmlParsedTags::kLinkTag;
        return true;
    }

    for (auto& xmlTag : mXmlTagNames)
    {
        if (wxStrcmp (tag, xmlTag.second) == 0)
            return true;
    }

    return false;
}

void UpdateManager::HandleXMLEndTag (const wxChar* tag)
{
    if (mXmlParsingState == XmlParsedTags::kDescriptionTag ||
        mXmlParsingState == XmlParsedTags::kLinkTag)
        mXmlParsingState = XmlParsedTags::kNotUsedTag;

    if (mXmlParsingState == XmlParsedTags::kVersionTag)
        mXmlParsingState = XmlParsedTags::kOsTag;
}

void UpdateManager::HandleXMLContent (const wxString& content)
{
    wxString trimedContent (content);

    switch (mXmlParsingState)
    {
    case XmlParsedTags::kDescriptionTag:
        trimedContent.Trim(true).Trim(false);
        // TODO: need current spliting by ". "
        mVersionPatch.changelog = wxSplit (trimedContent, '.');
        break;

    case XmlParsedTags::kVersionTag:
        trimedContent.Trim(true).Trim(false);
        mVersionPatch.version = VersionId::ParseFromString (trimedContent);
        break;

    case XmlParsedTags::kLinkTag:
        trimedContent.Trim(true).Trim(false);
        mVersionPatch.download = trimedContent;
        break;

    default:
        break;
    }
}

XMLTagHandler* UpdateManager::HandleXMLChild (const wxChar* tag)
{
    for (auto& xmlTag : mXmlTagNames)
    {
        if (wxStrcmp (tag, xmlTag.second) == 0)
            return this;
    }

    return NULL;
}
