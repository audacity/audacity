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
#include "../Internat.h"

PlaybackPrefs::PlaybackPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, _("Playback"))
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

namespace {
   const char *UnpinnedScrubbingPreferenceKey()
   {
      return "/AudioIO/UnpinnedScrubbing";
   }
   bool UnpinnedScrubbingPreferenceDefault()
   {
      return true;
   }
   int iPreferenceUnpinned = -1;
}

void PlaybackPrefs::PopulateOrExchange(ShuttleGui & S)
{
   wxTextCtrl *w;

   S.StartScroller();
   S.SetBorder(2);

   S.StartStatic(_("Effects Preview"));
   {
      S.StartThreeColumn();
      {
         w = S.TieNumericTextBox(_("&Length:"),
                                 wxT("/AudioIO/EffectsPreviewLen"),
                                 6.0,
                                 9);
         S.AddUnits(_("seconds"));
         if( w ) w->SetName(w->GetName() + wxT(" ") + _("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   /* i18n-hint: (noun) this is a preview of the cut */
   S.StartStatic(_("Cut Preview"));
   {
      S.StartThreeColumn();
      {
         w = S.TieNumericTextBox(_("&Before cut region:"),
                                 wxT("/AudioIO/CutPreviewBeforeLen"),
                                 2.0,
                                 9);
         S.AddUnits(_("seconds"));
         if( w ) w->SetName(w->GetName() + wxT(" ") + _("seconds"));

         w = S.TieNumericTextBox(_("&After cut region:"),
                                 wxT("/AudioIO/CutPreviewAfterLen"),
                                 1.0,
                                 9);
         S.AddUnits(_("seconds"));
         if( w ) w->SetName(w->GetName() + wxT(" ") + _("seconds"));
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
         if( w ) w->SetName(w->GetName() + wxT(" ") + _("seconds"));

         w = S.TieNumericTextBox(_("Lo&ng period:"),
                                 wxT("/AudioIO/SeekLongPeriod"),
                                 15.0,
                                 9);
         S.AddUnits(_("seconds"));
         if( w ) w->SetName(w->GetName() + wxT(" ") + _("seconds"));
      }
      S.EndThreeColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Options"));
   {
      S.StartVerticalLay();
      {
         S.TieCheckBox(_("&Vari-Speed Play"), "/AudioIO/VariSpeedPlay", true);
         S.TieCheckBox(_("&Micro-fades"), "/AudioIO/Microfades", false);
         S.TieCheckBox(_("Always scrub un&pinned"),
            UnpinnedScrubbingPreferenceKey(),
            UnpinnedScrubbingPreferenceDefault());
      }
      S.EndVerticalLay();
   }
   S.EndStatic();


   S.EndScroller();

}

bool PlaybackPrefs::GetUnpinnedScrubbingPreference()
{
   if ( iPreferenceUnpinned >= 0 )
      return iPreferenceUnpinned == 1;
   bool bResult = gPrefs->ReadBool(
      UnpinnedScrubbingPreferenceKey(),
      UnpinnedScrubbingPreferenceDefault());
   iPreferenceUnpinned = bResult ? 1: 0;
   return bResult;
}

bool PlaybackPrefs::Commit()
{
   iPreferenceUnpinned = -1;

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

wxString PlaybackPrefs::HelpPageName()
{
   return "Playback_Preferences";
}

PrefsPanel *PlaybackPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew PlaybackPrefs(parent, winid);
}

