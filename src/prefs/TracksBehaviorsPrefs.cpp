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

#include "../Prefs.h"
#include "../ShuttleGui.h"
#include "../Experimental.h"

TracksBehaviorsPrefs::TracksBehaviorsPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Tracks Behaviors"))
{
   Populate();
}

TracksBehaviorsPrefs::~TracksBehaviorsPrefs()
{
}

void TracksBehaviorsPrefs::Populate()
{
   mSoloCodes.Add(wxT("Simple"));
   mSoloCodes.Add(wxT("Multi"));
   mSoloCodes.Add(wxT("None"));

   mSoloChoices.Add(_("Simple"));
   mSoloChoices.Add(_("Multi-track"));
   mSoloChoices.Add(_("None"));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TracksBehaviorsPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Behaviors"));
   {
      /* i18n-hint: auto-select makes a selection if there was none.*/
      S.TieCheckBox(_("&Auto-select"),
                    wxT("/GUI/SelectAllOnNone"),
                    false);
      /* i18n-hint: auto-move moves clips out the way if necessary.*/
      S.TieCheckBox(_("&Auto-move for clips"),
                    wxT("/GUI/EditClipCanMove"),
                    true);
      S.TieCheckBox(_("Cut &lines"),
                    wxT("/GUI/EnableCutLines"),
                    false);
      S.TieCheckBox(_("&Drag selection edges"),
                    wxT("/GUI/AdjustSelectionEdges"),
                    true);
/* Stopping at either end is best (DA decision) 
   Works for VI users and regular users alike.
*/
      S.TieCheckBox(_("\"Move track focus\" c&ycles repeatedly through tracks"),
                    wxT("/GUI/CircularTrackNavigation"),
                    false);
      S.TieCheckBox(_("&Type to create a label"),
                    wxT("/GUI/TypeToCreateLabel"),
                    true);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Solo &Button:"),
                     wxT("/GUI/Solo"),
                     wxT("Standard"),
                     mSoloChoices,
                     mSoloCodes);
         S.SetSizeHints(mSoloChoices);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

bool TracksBehaviorsPrefs::Apply()
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

PrefsPanel *TracksBehaviorsPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew TracksBehaviorsPrefs(parent);
}
