/**********************************************************************

  Audacity: A Digital Audio Editor

  PlainExportOptionsEditor.h

  Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include "ExportOptionsEditor.h"
#include <wx/string.h>
#include <wx/arrstr.h>
#include <vector>
#include <unordered_map>

class PlainExportOptionsEditor : public ExportOptionsEditor
{
   std::vector<ExportOption> mOptions;
   wxArrayString mConfigKeys;
   std::unordered_map<int, ExportValue> mValues;
public:
   struct OptionDesc
   {
      ExportOption option;
      wxString configKey;
   };

   explicit PlainExportOptionsEditor(std::initializer_list<OptionDesc> options);

   int GetOptionsCount() const override;
   bool GetOption(int index, ExportOption& option) const override;

   bool GetValue(int id, ExportValue& value) const override;
   bool SetValue(int id, const ExportValue& value) override;

   void Load(const wxConfigBase& config) override;
   void Store(wxConfigBase& config) const override;
};
