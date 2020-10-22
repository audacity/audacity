/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportExportPrefs.h

  Joshua Haberman
  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_IMPORT_EXPORT_PREFS__
#define __AUDACITY_IMPORT_EXPORT_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define IMPORT_EXPORT_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("IMPORT EXPORT") }

template< typename Enum > class EnumSetting;

class ImportExportPrefs final : public PrefsPanel
{
 public:
   static EnumSetting< bool > ExportDownMixSetting;
   static EnumSetting< bool > LabelStyleSetting;
   static EnumSetting< bool > AllegroStyleSetting;

   ImportExportPrefs(wxWindow * parent, wxWindowID winid);
   ~ImportExportPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;
   void PopulateOrExchange(ShuttleGui & S) override;

 private:
   void Populate();
};

#endif
