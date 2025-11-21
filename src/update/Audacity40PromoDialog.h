/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file Audacity40PromoDialog.h
 @brief Promotional dialog for Audacity 4.0 release.

 **********************************************************************/
#pragma once

#include "UpdateDataStructures.h"
#include "wxPanelWrapper.h"

#include <wx/dialog.h>

/// Custom dialog for Audacity 4.0 release announcement
class Audacity40PromoDialog final : public wxDialogWrapper
{
public:
    Audacity40PromoDialog(wxWindow* parent, const Notification& notification);
};
