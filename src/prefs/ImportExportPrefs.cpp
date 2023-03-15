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

EnumSetting< bool > ImportExportPrefs::ExportDownMixSetting{
   wxT("/FileFormats/ExportDownMixChoice"),
   {
      EnumValueSymbol{ wxT("MixDown"), XXO("&Mix down to Stereo or Mono") },
      EnumValueSymbol{ wxT("Custom"), XXO("&Use Advanced Mixing Options") },
   },
   0, // true

   // for migrating old preferences:
   {
      true, false,
   },
   wxT("/FileFormats/ExportDownMix"),
};

EnumSetting< bool > ImportExportPrefs::LabelStyleSetting{
   wxT("/FileFormats/LabelStyleChoice"),
   {
      EnumValueSymbol{ wxT("Standard"), XXO("S&tandard") },
      EnumValueSymbol{ wxT("Extended"), XXO("E&xtended (with frequency ranges)") },
   },
   0, // true

   {
      true, false,
   },
};

EnumSetting< bool > ImportExportPrefs::AllegroStyleSetting{
   wxT("/FileFormats/AllegroStyleChoice"),
   {
      EnumValueSymbol{ wxT("Seconds"), XXO("&Seconds") },
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

   S.StartStatic(XO("When exporting tracks to an audio file"));
   {
      // Bug 2692: Place button group in panel so tabbing will work and,
      // on the Mac, VoiceOver will announce as radio buttons.
      S.StartPanel();
      {
         S.StartRadioButtonGroup(ImportExportPrefs::ExportDownMixSetting);
         {
            S.TieRadioButton();
            S.TieRadioButton();
         }
         S.EndRadioButtonGroup();
      }
      S.EndPanel();

      S.TieCheckBox(XXO("S&how Metadata Tags editor before export"),
                    {wxT("/AudioFiles/ShowId3Dialog"),
                     true});
      /* i18n-hint 'blank space' is space on the tracks with no audio in it*/
      S.TieCheckBox(XXO("&Ignore blank space at the beginning"),
                    {wxT("/AudioFiles/SkipSilenceAtBeginning"),
                     false});
   }
   S.EndStatic();

   S.StartStatic(XO("Exported Label Style:"));
   {
      // Bug 2692: Place button group in panel so tabbing will work and,
      // on the Mac, VoiceOver will announce as radio buttons.
      S.StartPanel();
      {
         S.StartRadioButtonGroup(ImportExportPrefs::LabelStyleSetting);
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
