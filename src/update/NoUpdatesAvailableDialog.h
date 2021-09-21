/**********************************************************************

  Audacity: A Digital Audio Editor

  NoUpdatesAvailableDialog.h

  Anton Gerasimov

**********************************************************************/

#pragma once

	
#include "widgets/wxPanelWrapper.h"

/// Information dialog about no updates available, that allows to navigate to settings quickly
class NoUpdatesAvailableDialog final : public wxDialogWrapper
{
public:
   explicit NoUpdatesAvailableDialog(wxWindow* parent);
   virtual ~NoUpdatesAvailableDialog();

   void OnOk(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
