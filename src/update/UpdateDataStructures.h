/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateDataStructures.h
 @brief Data structures for the update system.
 **********************************************************************/
#pragma once

#include <wx/datetime.h>
#include <wx/string.h>
#include <wx/arrstr.h>
#include <vector>
#include <string>

#include "VersionId.h"

struct NotificationAction
{
    wxString label;
    wxString link;
};

struct ChangelogItem
{
    wxString version;
    wxString text;
};

struct Notification
{
    wxString uuid;
    wxString title;
    wxString message;
    wxDateTime start;
    wxDateTime expires;

    NotificationAction notificationAction;

    bool IsActive(const wxDateTime& now = wxDateTime::Now()) const
    {
        if (start.IsValid() && now < start)
            return false;
        if (expires.IsValid() && now > expires)
            return false;
        return true;
    }
};

struct VersionPatch final
{
    VersionId version;
    wxString description;
    std::vector<ChangelogItem> changelog;
    wxString download;
    wxString releaseNotesUrl;
};

struct UpdateDataFeed final
{
    using UpdateDataFormat = std::string;

    VersionPatch versionPatch;
    std::vector<Notification> notifications;
};
