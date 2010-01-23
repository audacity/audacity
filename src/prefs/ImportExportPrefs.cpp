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

#include "../Audacity.h"

#include <wx/defs.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "ImportExportPrefs.h"

ImportExportPrefs::ImportExportPrefs(wxWindow * parent)
:   PrefsPanel(parent, _("Import / Export"))
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

   S.StartStatic(_("When importing audio files"));
   {
      S.StartRadioButtonGroup(wxT("/FileFormats/CopyOrEditUncompressedData"), wxT("edit"));
      {
         S.TieRadioButton(_("&Make a copy of uncompressed audio files before editing (safer)"),
                          wxT("copy"));
         S.TieRadioButton(_("&Read uncompressed audio files directly from the original (faster)"),
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
         S.TieRadioButton(_("&Always mix all tracks down to Stereo or Mono channel(s)"),
                          true);
         S.TieRadioButton(_("&Use custom mix (for example to export a 5.1 multichannel file)"),
                          false);
      }
      S.EndRadioButtonGroup();

      S.TieCheckBox(_("S&how Metadata Editor prior to export step"),
                    wxT("/AudioFiles/ShowId3Dialog"),
                    true);
      S.AddFixedText(_("Note: Export quality options can be chosen by clicking the Options\nbutton in the Export dialog."));
   }
   S.EndStatic();
}

bool ImportExportPrefs::Apply()
{  
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);    
   
   return true;
}


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 427b9e64-3fc6-40ef-bbf8-e6fff1d442f0
