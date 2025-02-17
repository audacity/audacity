/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3Utils.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "VST3Utils.h"
#include "Base64.h"

#include <wx/string.h>
#include <wx/filename.h>

#include <pluginterfaces/vst/ivsteditcontroller.h>
#include <pluginterfaces/vst/ivstparameterchanges.h>
#include <wx/regex.h>

#include "MemoryX.h"

#ifdef __WXMSW__
#include <shlobj.h>
#endif

namespace {
wxString GetFactoryPresetsBasePath()
{
#ifdef __WXMSW__
    PWSTR commonFolderPath { nullptr };
    auto cleanup = finally([&](){ CoTaskMemFree(commonFolderPath); });
    if (SHGetKnownFolderPath(FOLDERID_ProgramData, KF_FLAG_DEFAULT, NULL, &commonFolderPath) == S_OK) {
        return wxString(commonFolderPath) + "\\VST3 Presets\\";
    }
    return {};
#elif __WXMAC__
    return wxString("/Library/Audio/Presets/");
#elif __WXGTK__
    return wxString("/usr/local/share/vst3/presets/");
#endif
}

wxString GetPresetsPath(const wxString& basePath, const VST3::Hosting::ClassInfo& effectClassInfo)
{
    wxRegEx fixName(R"([\\*?/:<>|])");
    wxString companyName = wxString(effectClassInfo.vendor()).Trim();
    wxString pluginName = wxString(effectClassInfo.name()).Trim();

    fixName.ReplaceAll(&companyName, { "_" });
    fixName.ReplaceAll(&pluginName, { "_" });

    wxFileName result;
    result.SetPath(basePath);
    result.AppendDir(companyName);
    result.AppendDir(pluginName);
    auto path = result.GetPath();

    return path;
}
}

wxString VST3Utils::MakePluginPathString(const wxString& modulePath, const std::string& effectUIDString)
{
    return wxString::Format("%s;%s", modulePath, effectUIDString);
}

bool VST3Utils::ParsePluginPath(const wxString& pluginPath, wxString* modulePath,
                                std::string* effectUIDString)
{
    const auto sep = pluginPath.Find(';', true);
    if (sep != wxNOT_FOUND
        &&//modulePath not empty
        sep > 0
        &&//effectUIDString not empty
        static_cast<size_t>(sep) < pluginPath.Length() - 1) {
        if (modulePath != nullptr) {
            *modulePath = pluginPath.Left(sep);
        }
        if (effectUIDString != nullptr) {
            *effectUIDString = pluginPath.Mid(static_cast<size_t>(sep) + 1);
        }
        return true;
    }
    return false;
}

wxString VST3Utils::ToWxString(const Steinberg::Vst::TChar* str)
{
    static const wxCSConv csConv { wxFONTENCODING_UTF16 };
    return { reinterpret_cast<const char*>(str), csConv };
}

wxString VST3Utils::MakeAutomationParameterKey(const Steinberg::Vst::ParameterInfo& parameterInfo)
{
    auto suffix = ToWxString(parameterInfo.shortTitle);
    if (suffix.empty()) {
        suffix = ToWxString(parameterInfo.title);
    }

    if (!suffix.empty()) {
        return wxString::Format("%lu_", static_cast<unsigned long>(parameterInfo.id)) + suffix;
    }

    return wxString::Format("%lu", static_cast<unsigned long>(parameterInfo.id));
}

bool VST3Utils::ParseAutomationParameterKey(const wxString& key, Steinberg::Vst::ParamID& paramId)
{
    const auto pos = key.Find('_');
    const auto idStr = pos == wxNOT_FOUND ? key : key.Left(pos);
    unsigned long value { };
    if (idStr.ToULong(&value)) {
        paramId = static_cast<Steinberg::Vst::ParamID>(value);
        return true;
    }
    return false;
}

wxString VST3Utils::MakeFactoryPresetID(Steinberg::Vst::UnitID unitId, Steinberg::int32 programIndex)
{
    return wxString::Format("%d:%d",
                            static_cast<int>(unitId),
                            static_cast<int>(programIndex));
}

bool VST3Utils::ParseFactoryPresetID(const wxString& presetId, Steinberg::Vst::UnitID& unitId, Steinberg::int32& programIndex)
{
    auto parts = wxSplit(presetId, ':');
    long nums[2]{};
    if (parts.size() == 2 && parts[0].ToLong(&nums[0]) && parts[1].ToLong(&nums[1])) {
        unitId = static_cast<Steinberg::Vst::UnitID>(nums[0]);
        programIndex = static_cast<Steinberg::int32>(nums[1]);
        return true;
    }
    return false;
}

wxString VST3Utils::GetFactoryPresetsPath(const VST3::Hosting::ClassInfo& effectClassInfo)
{
    return GetPresetsPath(
        GetFactoryPresetsBasePath(),
        effectClassInfo
        );
}

Steinberg::IPtr<PresetsBufferStream> PresetsBufferStream::fromString(const wxString& str)
{
    Steinberg::Buffer buffer(str.length() / 4 * 3);
    const auto numBytes = Base64::Decode(str, buffer);
    //BufferStream uses fill size as a cursor position and size as a stream end position
    //To prevent plugins from fetching bytes past the meaningful data we need to truncate
    //end position
    buffer.setSize(numBytes);

    auto result = owned(safenew PresetsBufferStream);
    result->mBuffer.take(buffer);
    return result;
}

wxString PresetsBufferStream::toString() const
{
    auto str = Base64::Encode(mBuffer, mBuffer.getFillSize());
    return str;
}
