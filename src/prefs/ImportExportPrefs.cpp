/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExportPrefs.cpp

  Joshua Haberman
  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class ImportExportPrefs
\brief A PrefsPanel used to select import and export options.

*//*******************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "ImportExportPrefs.h"

#include <wx/defs.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "../Internat.h"

ImportExportPrefs::ImportExportPrefs(wxWindow * parent, wxWindowID winid)
:   PrefsPanel(parent, winid, _("Import / Export"))
{
   Populate();
}

ImportExportPrefs::~ImportExportPrefs()
{
}

/// Creates the dialog and its contents.
void ImportExportPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ImportExportPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("When importing audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("copy"));
      {
         S.TieRadioButton(_("&Copy uncompressed files into the project (safer)"),
                          wxT("copy"));
         S.TieRadioButton(_("&Read uncompressed files from original location (faster)"),
                          wxT("edit"));
      }
      S.EndRadioButtonGroup();

      S.TieCheckBox(_("&Normalize all tracks in project"),
                    wxT("/AudioFiles/NormalizeOnLoad"),
                    false);
   }
   S.EndStatic();

   S.StartStatic(_("When exporting tracks to an audio file"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/ExportDownMix"), true);
      {
         S.TieRadioButton(_("&Mix down to Stereo or Mono"),
                          true);
         S.TieRadioButton(_("&Use custom mix"),
                          false);
      }
      S.EndRadioButtonGroup();

      S.TieCheckBox(_("S&how Metadata Tags editor before export"),
                    wxT("/AudioFiles/ShowId3Dialog"),
                    true);
      // This documentation is unlikely to help somebody who cannot figure it out by discovering the Options button in the dialog.
      // It's only clutter in this Prefs tab, so removed.
      //    S.AddFixedText(_("Note: Export quality options can be chosen by clicking the Options\nbutton in the Export dialog."));
   }
   S.EndStatic();
#ifdef USE_MIDI
   S.StartStatic(_("Exported Allegro (.gro) files save time as:"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/AllegroStyle"), true);
      {
         S.TieRadioButton(_("&Seconds"),
                          true);
         S.TieRadioButton(_("&Beats"),
                          false);
      }
      S.EndRadioButtonGroup();
   }
   S.EndStatic();
#endif
   S.EndScroller();
}

bool ImportExportPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

wxString ImportExportPrefs::HelpPageName()
{
   return "Import_-_Export_Preferences";
}

PrefsPanel *ImportExportPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew ImportExportPrefs(parent, winid);
}
