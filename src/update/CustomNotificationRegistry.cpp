/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file CustomNotificationRegistry.cpp

 **********************************************************************/

#include "CustomNotificationRegistry.h"
#include "Audacity40PromoDialog.h"

bool CustomNotificationRegistry::HasCustomDialog(const wxString& uuid)
{
    // UUID for Audacity 4.0 promotional dialog
    if (uuid == "audacity-4.0-release-promo") return true;

    return false;
}

int CustomNotificationRegistry::ShowCustomDialog(wxWindow* parent, const Notification& notification)
{
    const wxString& uuid = notification.uuid;

    if (uuid == "audacity-4.0-release-promo")
    {
        Audacity40PromoDialog dlg(parent, notification);
        return dlg.ShowModal();
    }

    return wxID_NONE;
}
