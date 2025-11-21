/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file CustomNotificationRegistry.h

 **********************************************************************/
#pragma once

#include "UpdateDataStructures.h"

#include <wx/string.h>

class wxWindow;

class CustomNotificationRegistry
{
public:
    static bool HasCustomDialog(const wxString& uuid);

    /// Show a custom dialog for the given UUID
    /// wxID_APPLY: "Remind me later" pressed
    static int ShowCustomDialog(wxWindow* parent, const Notification& notification);
};
