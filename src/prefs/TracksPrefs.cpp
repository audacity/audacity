/**********************************************************************

  Audacity: A Digital Audio Editor

  TracksPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class TracksPrefs
\brief A PrefsPanel for track display and behavior properties.

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>

#include "../ShuttleGui.h"

#include "TracksPrefs.h"

////////////////////////////////////////////////////////////////////////////////

TracksPrefs::TracksPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Tracks"))
{
   Populate();
}

TracksPrefs::~TracksPrefs()
{
}

void TracksPrefs::Populate()
{
   mSoloCodes.Add(wxT("Standard"));
   mSoloCodes.Add(wxT("Simple"));
   mSoloCodes.Add(wxT("None"));

   mSoloChoices.Add(_("Standard"));
   mSoloChoices.Add(_("Simple"));
   mSoloChoices.Add(_("None"));


   // Keep the same order as in TrackPanel.cpp menu: OnWaveformID, OnWaveformDBID, OnSpectrumID, OnSpectrumLogID, OnPitchID
   mViewCodes.Add(0);
   mViewCodes.Add(1);
   mViewCodes.Add(2);
   mViewCodes.Add(3);
   mViewCodes.Add(4);

   mViewChoices.Add(_("Waveform"));
   mViewChoices.Add(_("Waveform (dB)"));
   mViewChoices.Add(_("Spectrogram"));
   mViewChoices.Add(_("Spectrogram log(f)"));
   mViewChoices.Add(_("Pitch (EAC)"));

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TracksPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Display"));
   {
      S.TieCheckBox(_("&Update display while playing"),
                    wxT("/GUI/AutoScroll"),
                    true);
      S.TieCheckBox(_("Automatically &fit tracks vertically zoomed"),
                    wxT("/GUI/TracksFitVerticallyZoomed"),
                    false);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Default &View Mode:"),
                     wxT("/GUI/DefaultViewMode"),
                     0,
                     mViewChoices,
                     mViewCodes);
         S.SetSizeHints(mViewChoices);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Behaviors"));
   {
      S.TieCheckBox(_("&Select all audio in project, if none selected"),
                    wxT("/GUI/SelectAllOnNone"),
                    true);
      /* i18n-hint: cut-lines are a lines indicating where to cut.*/
      S.TieCheckBox(_("Enable cu&t lines"),
                    wxT("/GUI/EnableCutLines"),
                    false);
      S.TieCheckBox(_("Enable &dragging of left and right selection edges"),
                    wxT("/GUI/AdjustSelectionEdges"),
                    true);
      S.TieCheckBox(_("\"Move track focus\" c&ycles repeatedly through tracks"),
                    wxT("/GUI/CircularTrackNavigation"),
                    false);
      S.TieCheckBox(_("Editing a clip can &move other clips"),
                    wxT("/GUI/EditClipCanMove"),
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

bool TracksPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}
