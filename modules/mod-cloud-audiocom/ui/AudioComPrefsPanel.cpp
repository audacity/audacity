/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioComPrefsPanel.h

  Dmitry Vedenko

**********************************************************************/

#include <cassert>

#include <wx/button.h>
#include <wx/textctrl.h>

#include "prefs/PrefsPanel.h"

#include "CloudSettings.h"
#include "CloudModuleSettings.h"
#include "MixdownPrefsPanel.h"
#include "ShuttleGui.h"
#include "UserPanel.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UserService.h"

#include "TempDirectory.h"

namespace
{
using namespace cloud::audiocom;

class AudioComPrefsPanel final : public PrefsPanel
{
public:
   AudioComPrefsPanel(wxWindow* parent, wxWindowID winid)
       : PrefsPanel { parent, winid, XO("Cloud") }
   {
      ShuttleGui S(this, eIsCreatingFromPrefs);
      PopulateOrExchange(S);
   }

   bool Commit() override
   {
      ShuttleGui S(this, eIsSavingToPrefs);
      PopulateOrExchange(S);

      MixdownGenerationFrequency.Write(mMixdownPrefsPanel->GetFrequency());

      CloudProjectsSavePath.Invalidate();
      sync::ShowCloudSyncDialog.Invalidate();
      DaysToKeepFiles.Invalidate();

      return true;
   }

   void PopulateOrExchange(ShuttleGui& S) override
   {
      S.SetBorder(2);
      S.StartScroller();
      {
         S.SetBorder(8);

         S.StartStatic(XO("Account"));
         {
            S.SetBorder(8);
            S.AddWindow(safenew UserPanel { GetServiceConfig(),
                                            GetOAuthService(), GetUserService(),
                                            true, S.GetParent() }, wxEXPAND);
         }
         S.EndStatic();

         S.StartStatic(XO("Generate mixdown for audio.com playback"));
         {
            S.SetBorder(8);
            mMixdownPrefsPanel =
               safenew sync::MixdownPrefsPanel { S.GetParent(), true };
            mMixdownPrefsPanel->SetFrequency(MixdownGenerationFrequency.Read());
            S.AddWindow(mMixdownPrefsPanel, wxEXPAND);
         }
         S.EndStatic();

         S.StartStatic(XO("On save behavior"));
         {
            S.SetBorder(8);
            S.TieCheckBox(
               XXO("Show 'How would you like to save?' modal"),
               sync::ShowCloudSyncDialog);
         }
         S.EndStatic();

         S.StartStatic(XO("Temporary cloud files directory"));
         {
            S.SetBorder(8);
            S.StartMultiColumn(3, wxEXPAND);
            {
               S.SetStretchyCol(1);

               mCloudProjectsSavePath = S.TieTextBox(XXO("&Location:"), CloudProjectsSavePath, 30);
               S.AddButton(XXO("Brow&se..."))
                  ->Bind(wxEVT_BUTTON, [this](auto&) { Browse(); });
            }
            S.EndMultiColumn();

            S.SetBorder(8);
            S.StartMultiColumn(3);
            {
               S.TieIntegerTextBox(
                  XXO("Remove temporary files after:"), DaysToKeepFiles, 10);
               S.AddFixedText(XO("days"), true);
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndScroller();
   }

   ComponentInterfaceSymbol GetSymbol() const override
   {
      return ComponentInterfaceSymbol { XO("Cloud") };
   }

   TranslatableString GetDescription() const override
   {
      return XO("Preferences for Cloud");
   }

   void Browse()
   {
      const auto currentPath = CloudProjectsSavePath.Read();

      wxDirDialogWrapper dlog(
         this, XO("Choose a location to place the temporary directory"),
         currentPath);
      int retval = dlog.ShowModal();

      if (retval != wxID_CANCEL && !dlog.GetPath().empty())
      {
         wxFileName tmpDirPath;
         tmpDirPath.AssignDir(dlog.GetPath());

         if (TempDirectory::FATFilesystemDenied(
                tmpDirPath.GetFullPath(),
                XO("Temporary files directory cannot be on a FAT drive.")))
         {
            return;
         }

         if (!FileNames::WritableLocationCheck(
                dlog.GetPath(), XO("Cannot set the preference.")))
         {
            return;
         }

         mCloudProjectsSavePath->SetValue(
            tmpDirPath.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR));
      }
   }

private:
   sync::MixdownPrefsPanel* mMixdownPrefsPanel {};
   wxTextCtrl* mCloudProjectsSavePath {};

}; // class AudioComPrefsPanel

PrefsPanel::Registration sAttachment {
   "AudioComPrefsPanel",
   [](wxWindow* parent, wxWindowID winid, AudacityProject*) -> PrefsPanel*
   {
      assert(parent != nullptr);
      return parent != nullptr ? safenew AudioComPrefsPanel(parent, winid) :
                                 nullptr;
   },
   false,
   // Register with an explicit ordering hint because this one might be
   // absent
   { "", { Registry::OrderingHint::End, {} } }
};
} // namespace
