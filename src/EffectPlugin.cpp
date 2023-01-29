/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file EffectPlugin.cpp

  Paul Licameli split from EffectInterface.cpp

**********************************************************************/
#include "EffectPlugin.h"
#include <wx/window.h>

EffectPlugin::~EffectPlugin() = default;

const wxString EffectPlugin::kUserPresetIdent = wxT("User Preset:");
const wxString EffectPlugin::kFactoryPresetIdent = wxT("Factory Preset:");
const wxString EffectPlugin::kCurrentSettingsIdent = wxT("<Current Settings>");
const wxString EffectPlugin::kFactoryDefaultsIdent = wxT("<Factory Defaults>");

EffectInstanceEx::~EffectInstanceEx() = default;

bool EffectInstanceEx::Init()
{
   return true;
}

EffectUIValidator::EffectUIValidator(
   EffectUIServices &services, EffectSettingsAccess &access)
   : mUIServices{ services }
   , mAccess{ access }
{}

EffectUIValidator::~EffectUIValidator() = default;

bool EffectUIValidator::UpdateUI()
{
   return true;
}

bool EffectUIValidator::IsGraphicalUI()
{
   return false;
}

void EffectUIValidator::Disconnect()
{
}

void EffectUIValidator::OnClose()
{
   if (!mUIClosed)
   {
      mUIServices.CloseUI();
      mUIClosed = true;
   }
}

bool EffectUIValidator::EnableApply(wxWindow *parent, bool enable)
{
   // May be called during initialization, so try to find the dialog
   if (auto dlg = wxGetTopLevelParent(parent)) {
      wxWindow *apply = dlg->FindWindow(wxID_APPLY);

      // Don't allow focus to get trapped
      if (!enable)
      {
         wxWindow *focus = dlg->FindFocus();
         if (focus == apply)
         {
            dlg->FindWindow(wxID_CLOSE)->SetFocus();
         }
      }

      if (apply)
         apply->Enable(enable);
   }

   EnablePreview(parent, enable);

   return enable;
}

bool EffectUIValidator::EnablePreview(wxWindow *parent, bool enable)
{
   // May be called during initialization, so try to find the dialog
   if (auto dlg = wxGetTopLevelParent(parent)) {
      wxWindow *play = dlg->FindWindow(kPlayID);
      if (play)
      {
         // Don't allow focus to get trapped
         if (!enable)
         {
            wxWindow *focus = dlg->FindFocus();
            if (focus == play)
            {
               dlg->FindWindow(wxID_CLOSE)->SetFocus();
            }
         }

         play->Enable(enable);
      }
   }

   return enable;
}

EffectUIServices::~EffectUIServices() = default;
