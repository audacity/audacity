/**********************************************************************

  Audacity: A Digital Audio Editor

  @file BasicEffectUIServices.cpp

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/
#include "BasicEffectUIServices.h"
#include "EffectPlugin.h"
#include "FileNames.h"

#include <wx/dialog.h>
#include <wx/ffile.h>
#include <wx/filedlg.h>
#include "SelectFile.h"
#include "AudacityMessageBox.h"
#include "VetoDialogHook.h"

int BasicEffectUIServices::ShowClientInterface(const EffectPlugin &plugin,
   wxWindow &parent, wxDialog &dialog, EffectEditor *, bool forceModal) const
{
   dialog.Layout();
   dialog.Fit();
   dialog.SetMinSize(dialog.GetSize());
   if (VetoDialogHook::Call(&dialog))
      return 0;
   if (plugin.SupportsRealtime() && !forceModal) {
      dialog.Show();
      // Return false to bypass effect processing
      return 0;
   }
   return dialog.ShowModal();
}

bool BasicEffectUIServices::ValidateUI(
   const EffectPlugin &context, EffectSettings &) const
{
   return true;
}

bool BasicEffectUIServices::CloseUI() const
{
   return true;
}

static const FileNames::FileTypes &PresetTypes()
{
   static const FileNames::FileTypes result {
      { XO("Presets"), { wxT("txt") }, true },
      FileNames::AllFiles
   };
   return result;
};

void BasicEffectUIServices::ExportPresets(
   const EffectPlugin &plugin, const EffectSettings &settings) const
{
   wxString params;
   plugin.SaveSettingsAsString(settings, params);
   auto commandId = plugin.GetSquashedName(plugin.GetSymbol().Internal());
   params =  commandId.GET() + ":" + params;

   auto path = SelectFile(FileNames::Operation::Presets,
                                     XO("Export Effect Parameters"),
                                     wxEmptyString,
                                     wxEmptyString,
                                     wxEmptyString,
                                     PresetTypes(),
                                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                                     nullptr);
   if (path.empty()) {
      return;
   }

   // Create/Open the file
   wxFFile f(path, wxT("wb"));
   if (!f.IsOpened())
   {
      AudacityMessageBox(
         XO("Could not open file: \"%s\"").Format( path ),
         XO("Error Saving Effect Presets"),
         wxICON_EXCLAMATION,
         NULL);
      return;
   }

   f.Write(params);
   if (f.Error())
   {
      AudacityMessageBox(
         XO("Error writing to file: \"%s\"").Format( path ),
         XO("Error Saving Effect Presets"),
         wxICON_EXCLAMATION,
         NULL);
   }

   f.Close();


   //SetWindowTitle();

}

OptionalMessage BasicEffectUIServices::ImportPresets(
   const EffectPlugin &plugin, EffectSettings &settings) const
{
   wxString params;

   auto path = SelectFile(FileNames::Operation::Presets,
                                     XO("Import Effect Parameters"),
                                     wxEmptyString,
                                     wxEmptyString,
                                     wxEmptyString,
                                     PresetTypes(),
                                     wxFD_OPEN | wxRESIZE_BORDER,
                                     nullptr);
   if (path.empty()) {
      return {};
   }

   wxFFile f(path);
   if (!f.IsOpened())
      return {};

   OptionalMessage result{};

   if (f.ReadAll(&params)) {
      wxString ident = params.BeforeFirst(':');
      params = params.AfterFirst(':');

      auto commandId = plugin.GetSquashedName(plugin.GetSymbol().Internal());

      if (ident != commandId) {
         // effect identifiers are a sensible length!
         // must also have some params.
         if ((params.Length() < 2 ) || (ident.Length() < 2) ||
             (ident.Length() > 30))
         {
            EffectUIServices::DoMessageBox(plugin,
               /* i18n-hint %s will be replaced by a file name */
               XO("%s: is not a valid presets file.\n")
               .Format(wxFileNameFromPath(path)));
         }
         else
         {
            EffectUIServices::DoMessageBox(plugin,
               /* i18n-hint %s will be replaced by a file name */
               XO("%s: is for a different Effect, Generator or Analyzer.\n")
               .Format(wxFileNameFromPath(path)));
         }
         return {};
      }
      result = plugin.LoadSettingsFromString(params, settings);
   }

   //SetWindowTitle();

   return result;
}

void BasicEffectUIServices::ShowOptions(const EffectPlugin &) const
{
}
