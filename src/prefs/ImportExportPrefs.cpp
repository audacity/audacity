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


#include "ImportExportPrefs.h"

#include <wx/defs.h>

#include "LabelTrack.h"
#include "Prefs.h"
#include "ShuttleGui.h"

ImportExportPrefs::ImportExportPrefs(wxWindow * parent, wxWindowID winid)
:   PrefsPanel(parent, winid, XO("Import / Export"))
{
   Populate();
}

ImportExportPrefs::~ImportExportPrefs()
{
}

ComponentInterfaceSymbol ImportExportPrefs::GetSymbol() const
{
   return IMPORT_EXPORT_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ImportExportPrefs::GetDescription() const
{
   return XO("Preferences for ImportExport");
}

ManualPageID ImportExportPrefs::HelpPageName()
{
   return "Import_-_Export_Preferences";
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

EnumSetting< bool > ImportExportPrefs::AllegroStyleSetting{
   wxT("/FileFormats/AllegroStyleChoice"),
   {
      EnumValueSymbol{ wxT("Seconds"), XXO("&Seconds") },
      /* i18n-hint: The music theory "beat" */
      EnumValueSymbol{ wxT("Beats"), XXO("&Beats") },
   },
   0, // true

   // for migrating old preferences:
   {
      true, false,
   },
   wxT("/FileFormats/AllegroStyle"),
};

void ImportExportPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("Exported Label Style:"));
   {
      // Bug 2692: Place button group in panel so tabbing will work and,
      // on the Mac, VoiceOver will announce as radio buttons.
      S.StartPanel();
      {
         S.StartRadioButtonGroup(LabelTrack::StyleSetting);
         {
            S.TieRadioButton();
            S.TieRadioButton();
         }
         S.EndRadioButtonGroup();
      }
      S.EndPanel();
   }
   S.EndStatic();

#ifdef USE_MIDI
   S.StartStatic(XO("Exported Allegro (.gro) files save time as:"));
   {
      // Bug 2692: Place button group in panel so tabbing will work and,
      // on the Mac, VoiceOver will announce as radio buttons.
      S.StartPanel();
      {
         S.StartRadioButtonGroup(ImportExportPrefs::AllegroStyleSetting);
         {
            S.TieRadioButton();
            S.TieRadioButton();
         }
         S.EndRadioButtonGroup();
      }
      S.EndPanel();
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

namespace{
PrefsPanel::Registration sAttachment{ "ImportExport",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ImportExportPrefs(parent, winid);
   }
};
}
