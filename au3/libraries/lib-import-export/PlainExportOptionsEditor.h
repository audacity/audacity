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

class IMPORT_EXPORT_API PlainExportOptionsEditor final : public ExportOptionsEditor
{
    std::vector<ExportOption> mOptions;
    wxArrayString mConfigKeys;
    std::unordered_map<int, ExportValue> mValues;
    SampleRateList mRates;
    Listener* mOptionsListener{};

public:
    struct OptionDesc
    {
        ExportOption option;
        wxString configKey;
    };

    explicit PlainExportOptionsEditor(std::initializer_list<OptionDesc> options, Listener* listener = nullptr);
    explicit PlainExportOptionsEditor(std::initializer_list<OptionDesc> options, SampleRateList samplerates, Listener* listener = nullptr);

    int GetOptionsCount() const override;
    bool GetOption(int index, ExportOption& option) const override;

    bool GetValue(int id, ExportValue& value) const override;
    bool SetValue(int id, const ExportValue& value) override;

    void Load(const audacity::BasicSettings& config) override;
    void Store(audacity::BasicSettings&) const override;

    SampleRateList GetSampleRateList() const override;

    void SetSampleRateList(SampleRateList rates);

private:
    void InitOptions(std::initializer_list<OptionDesc> options);
};
