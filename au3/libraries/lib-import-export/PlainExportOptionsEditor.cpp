/**********************************************************************

  Audacity: A Digital Audio Editor

  PlainExportOptionsEditor.cpp

  Vitaly Sverchinsky

**********************************************************************/

#include "PlainExportOptionsEditor.h"
#include <wx/config.h>

#include "BasicSettings.h"

PlainExportOptionsEditor::PlainExportOptionsEditor(std::initializer_list<OptionDesc> options,
                                                   Listener* listener)
    : mOptionsListener(listener)
{
    InitOptions(options);
}

PlainExportOptionsEditor::PlainExportOptionsEditor(std::initializer_list<OptionDesc> options,
                                                   SampleRateList sampleRates,
                                                   Listener* listener)
    : mRates(std::move(sampleRates))
    , mOptionsListener(listener)
{
    InitOptions(options);
}

int PlainExportOptionsEditor::GetOptionsCount() const
{
    return static_cast<int>(mOptions.size());
}

bool PlainExportOptionsEditor::GetOption(int index, ExportOption& option) const
{
    if (index >= 0 && index < static_cast<int>(mOptions.size())) {
        option = mOptions[index];
        return true;
    }
    return false;
}

bool PlainExportOptionsEditor::GetValue(int id, ExportValue& value) const
{
    const auto it = mValues.find(id);
    if (it != mValues.end()) {
        value = it->second;
        return true;
    }
    return false;
}

bool PlainExportOptionsEditor::SetValue(int id, const ExportValue& value)
{
    const auto it = mValues.find(id);
    if (it != mValues.end() && it->second.index() == value.index()) {
        it->second = value;
        return true;
    }
    return false;
}

void PlainExportOptionsEditor::Load(const audacity::BasicSettings& config)
{
    auto index = 0;
    for (const auto& option : mOptions) {
        auto it = mValues.find(option.id);
        assert(it != mValues.end());
        if (auto val = std::get_if<bool>(&it->second)) {
            config.Read(mConfigKeys[index], val);
        } else if (auto val = std::get_if<int>(&it->second)) {
            config.Read(mConfigKeys[index], val);
        } else if (auto val = std::get_if<double>(&it->second)) {
            config.Read(mConfigKeys[index], val);
        } else if (auto val = std::get_if<std::string>(&it->second)) {
            wxString wxstr;
            if (config.Read(mConfigKeys[index], &wxstr)) {
                *val = wxstr.ToStdString();
            }
        }
        ++index;
    }
}

void PlainExportOptionsEditor::Store(audacity::BasicSettings& config) const
{
    auto index = 0;
    for (const auto& option : mOptions) {
        auto it = mValues.find(option.id);
        assert(it != mValues.end());
        if (auto val = std::get_if<bool>(&it->second)) {
            config.Write(mConfigKeys[index], *val);
        } else if (auto val = std::get_if<int>(&it->second)) {
            config.Write(mConfigKeys[index], *val);
        } else if (auto val = std::get_if<double>(&it->second)) {
            config.Write(mConfigKeys[index], *val);
        } else if (auto val = std::get_if<std::string>(&it->second)) {
            config.Write(mConfigKeys[index], wxString(*val));
        }

        ++index;
    }
}

ExportOptionsEditor::SampleRateList PlainExportOptionsEditor::GetSampleRateList() const
{
    return mRates;
}

void PlainExportOptionsEditor::SetSampleRateList(SampleRateList rates)
{
    mRates = std::move(rates);
    if (mOptionsListener) {
        mOptionsListener->OnSampleRateListChange();
    }
}

void PlainExportOptionsEditor::InitOptions(std::initializer_list<OptionDesc> options)
{
    assert(mOptions.empty());

    mOptions.reserve(options.size());
    mValues.reserve(options.size());
    for (auto& desc : options) {
        mValues[desc.option.id] = desc.option.defaultValue;
        mOptions.emplace_back(desc.option);
        mConfigKeys.push_back(desc.configKey);
    }
}
