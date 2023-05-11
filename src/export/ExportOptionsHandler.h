/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportOptionsHandler.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <memory>
#include <unordered_map>
#include <wx/event.h>
#include "ExportOptionsEditor.h"
#include "ExportPlugin.h"

class ShuttleGui;

class wxWindowUpdateLocker;
class wxStaticText;
class wxControl;

// For a file suffix change from the options.
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API,
   AUDACITY_FILE_SUFFIX_EVENT, wxCommandEvent);

class ExportOptionsHandler final
   : public ExportOptionsEditor::Listener
{
public:

   ExportOptionsHandler(ShuttleGui& S, const ExportPlugin& plugin, int format);

   ~ExportOptionsHandler();

   bool TransferDataFromEditor();
   ExportProcessor::Parameters GetParameters() const;
   ExportOptionsEditor::SampleRateList GetSampleRateList() const;

   void OnExportOptionChangeBegin() override;
   void OnExportOptionChangeEnd() override;
   void OnExportOptionChange(const ExportOption& option) override;
   void OnExtensionChange(const wxString& extension) override;
   void OnSampleRateListChange() override;
   
private:
   
   void PopulateEmpty(ShuttleGui& S);
   void PopulateOptions(ShuttleGui& S);
   
   void UpdateSampleRateList();

   wxWindow* mParent { nullptr };
   std::unique_ptr<wxWindowUpdateLocker> mUpdateLocker;
   std::unique_ptr<ExportOptionsEditor> mEditor;
   std::vector<std::tuple<wxStaticText*, wxControl*>> mRows;
   std::unordered_map<int, int> mIDRowIndexMap;
};
