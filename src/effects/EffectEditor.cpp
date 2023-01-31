/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEditor.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from EffectPlugin.cpp

**********************************************************************/
#include "EffectEditor.h"
#include "EffectPlugin.h"
#include "EffectUIServices.h"
#include <wx/window.h>

EffectEditor::EffectEditor(
   const EffectUIServices &services, EffectSettingsAccess &access)
   : mUIServices{ services }
   , mAccess{ access }
{}

EffectEditor::~EffectEditor() = default;

bool EffectEditor::UpdateUI()
{
   return true;
}

bool EffectEditor::IsGraphicalUI()
{
   return false;
}

void EffectEditor::Disconnect()
{
}

void EffectEditor::OnClose()
{
   if (!mUIClosed)
   {
      mUIServices.CloseUI();
      mUIClosed = true;
   }
}

bool EffectEditor::EnableApply(wxWindow *parent, bool enable)
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

bool EffectEditor::EnablePreview(wxWindow *parent, bool enable)
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
