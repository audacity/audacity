/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksBehaviorsPrefs.cpp

  Steve Daulton


*******************************************************************//**

\class TracksBehaviorsPrefs
\brief A PrefsPanel for Tracks Behaviors settings.

*//*******************************************************************/

#include "../Audacity.h"
#include "TracksBehaviorsPrefs.h"

#include "../Experimental.h"

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../Internat.h"

TracksBehaviorsPrefs::TracksBehaviorsPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: two nouns */
:  PrefsPanel(parent, winid, _("Tracks Behaviors"))
{
   Populate();
}

TracksBehaviorsPrefs::~TracksBehaviorsPrefs()
{
}

const wxChar *TracksBehaviorsPrefs::ScrollingPreferenceKey()
{
   static auto string = wxT("/GUI/ScrollBeyondZero");
   return string;
}

void TracksBehaviorsPrefs::Populate()
{
   mSoloCodes.push_back(wxT("Simple"));
   mSoloCodes.push_back(wxT("Multi"));
   mSoloCodes.push_back(wxT("None"));

   mSoloChoices.push_back(_("Simple"));
   mSoloChoices.push_back(_("Multi-track"));
   mSoloChoices.push_back(_("None"));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TracksBehaviorsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Behaviors"));
   {
      S.TieCheckBox(_("&Select all audio, if selection required"),
                    wxT("/GUI/SelectAllOnNone"),
                    false);
      /* i18n-hint: Cut-lines are lines that can expand to show the cut audio.*/
      S.TieCheckBox(_("Enable cut &lines"),
                    wxT("/GUI/EnableCutLines"),
                    false);
      S.TieCheckBox(_("Enable &dragging selection edges"),
                    wxT("/GUI/AdjustSelectionEdges"),
                    true);
      S.TieCheckBox(_("Editing a clip can &move other clips"),
                    wxT("/GUI/EditClipCanMove"),
                    true);
      S.TieCheckBox(_("\"Move track focus\" c&ycles repeatedly through tracks"),
                    wxT("/GUI/CircularTrackNavigation"),
                    false);
      S.TieCheckBox(_("&Type to create a label"),
                    wxT("/GUI/TypeToCreateLabel"),
                    true);
      S.TieCheckBox(_("Use dialog for the &name of a new label"),
                    wxT("/GUI/DialogForNameNewLabel"),
                    false);
#ifdef EXPERIMENTAL_SCROLLING_LIMITS
      S.TieCheckBox(_("Enable scrolling left of &zero"),
                    ScrollingPreferenceKey(),
                    ScrollingPreferenceDefault());
#endif
      S.TieCheckBox(_("Advanced &vertical zooming"),
                    wxT("/GUI/VerticalZooming"),
                    false);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Solo &Button:"),
                     wxT("/GUI/Solo"),
                     wxT("Standard"),
                     mSoloChoices,
                     mSoloCodes);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndScroller();
}

bool TracksBehaviorsPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

wxString TracksBehaviorsPrefs::HelpPageName()
{
   return "Tracks_Behaviors_Preferences";
}

TracksBehaviorsPrefsFactory::TracksBehaviorsPrefsFactory()
{
}

PrefsPanel *TracksBehaviorsPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew TracksBehaviorsPrefs(parent, winid);
}
