/**********************************************************************

  Audacity: A Digital Audio Editor

  PlaybackPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class PlaybackPrefs
\brief A PrefsPanel used to select playback options.

  Presents interface for user to update the various playback options
  like previewing and seeking.

*//********************************************************************/

#include "../Audacity.h"
#include "PlaybackPrefs.h"

#include <wx/defs.h>
#include <wx/textctrl.h>

#include "../ShuttleGui.h"
#include "../Prefs.h"

PlaybackPrefs::PlaybackPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Playback"))
{
   Populate();
}

PlaybackPrefs::~PlaybackPrefs()
{
}

void PlaybackPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void PlaybackPrefs::PopulateOrExchange(ShuttleGui & S)
{
   wxTextCtrl *w;

   S.SetBorder(2);

   S.StartStatic(_("Effects Preview"));
   {
      S.StartThreeColumn();
      {
         w = S.TieNumericTextBox(_("&Length of preview:"),
                                 wxT("/AudioIO/EffectsPreviewLen"),
                                 6.0,
                                 9);
         S.AddUnits(_("seconds"));
         w->SetName(w->GetName() + wxT(" ") + _("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   /* i18n-hint: (noun) this is a preview of the cut */
   S.StartStatic(_("Cut Preview"));
   {
      S.StartThreeColumn();
      {
         w = S.TieNumericTextBox(_("Preview &before cut region:"),
                                 wxT("/AudioIO/CutPreviewBeforeLen"),
                                 2.0,
                                 9);
         S.AddUnits(_("seconds"));
         w->SetName(w->GetName() + wxT(" ") + _("seconds"));

         w = S.TieNumericTextBox(_("Preview &after cut region:"),
                                 wxT("/AudioIO/CutPreviewAfterLen"),
                                 1.0,
                                 9);
         S.AddUnits(_("seconds"));
         w->SetName(w->GetName() + wxT(" ") + _("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Seek Time when playing"));
   {
      S.StartThreeColumn();
      {
         w = S.TieNumericTextBox(_("&Short period:"),
                                 wxT("/AudioIO/SeekShortPeriod"),
                                 1.0,
                                 9);
         S.AddUnits(_("seconds"));
         w->SetName(w->GetName() + wxT(" ") + _("seconds"));

         w = S.TieNumericTextBox(_("Lo&ng period:"),
                                 wxT("/AudioIO/SeekLongPeriod"),
                                 15.0,
                                 9);
         S.AddUnits(_("seconds"));
         w->SetName(w->GetName() + wxT(" ") + _("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();
}

bool PlaybackPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

PrefsPanel *PlaybackPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew PlaybackPrefs(parent);
}

