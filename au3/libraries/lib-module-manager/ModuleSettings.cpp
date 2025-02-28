/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ModuleSettings.cpp

  Paul Licameli split from ModulePrefs.cpp

**********************************************************************/

#include "ModuleSettings.h"

#include "Prefs.h"

#include <unordered_set>
#include <wx/filename.h>

#include "MemoryX.h"

class ModuleSettingsResetHandler final : public PreferencesResetHandler
{
    using KeyValueStorage = std::vector<std::pair<wxString, wxString> >;
    std::optional<KeyValueStorage> mStorage;
public:

    ~ModuleSettingsResetHandler() override
    {
        assert(!mStorage.has_value());
    }

    void OnSettingResetBegin() override
    {
        assert(!mStorage.has_value());

        static const wxString modulePrefsGroups[] = {
            "/ModulePath/",
            "/Module/",
            "/ModuleDateTime/"
        };
        KeyValueStorage storage;
        for (const auto& group : modulePrefsGroups) {
            if (!gPrefs->HasGroup(group)) {
                continue;
            }

            const auto groupScope = gPrefs->BeginGroup(group);
            for (const auto& key : gPrefs->GetChildKeys()) {
                wxString value;
                if (gPrefs->Read(key, &value)) {
                    storage.emplace_back(group + key, value);
                }
            }
        }
        mStorage = std::move(storage);
    }

    void OnSettingResetEnd() override
    {
        if (!mStorage.has_value()) {
            return;
        }
        const auto Do = finally([=]{ mStorage = std::nullopt; });
        for (const auto& [key, value] : *mStorage) {
            gPrefs->Write(key, value);
        }
    }
};

static PreferencesResetHandler::Registration<ModuleSettingsResetHandler> preserveModuleSettings;

static const std::unordered_set<wxString>& autoEnabledModules()
{
    // Add names to this list, of modules that are expected to ship
    // with Audacity and enable automatically.
    static std::unordered_set<wxString> modules{
        "mod-ogg",
        "mod-flac",
        "mod-mp2",
        "mod-wavpack",
        "mod-mp3",
        "mod-mpg123",
        "mod-pcm",
        "mod-ffmpeg",
        "mod-cl",
        "mod-lof",
        "mod-aup",
        "mod-opus",
        "mod-midi-import-export",
        "mod-cloud-audiocom"
    };
    return modules;
}

// static function that tells us about a module.
int ModuleSettings::GetModuleStatus(const FilePath& fname)
{
    // Default status is NEW module, and we will ask once.
    int iStatus = kModuleNew;

    wxFileName FileName(fname);
    wxString ShortName = FileName.GetName().Lower();

    wxString PathPref = wxString(wxT("/ModulePath/")) + ShortName;
    wxString StatusPref = wxString(wxT("/Module/")) + ShortName;
    wxString DateTimePref = wxString(wxT("/ModuleDateTime/")) + ShortName;

    wxString ModulePath = gPrefs->Read(PathPref, wxEmptyString);
    if (ModulePath.IsSameAs(fname)) {
        gPrefs->Read(StatusPref, &iStatus, static_cast<int>(kModuleNew));

        wxDateTime DateTime = FileName.GetModificationTime();
        wxDateTime OldDateTime;
        OldDateTime.ParseISOCombined(gPrefs->Read(DateTimePref, wxEmptyString));

        // Some platforms return milliseconds, some do not...level the playing field
        DateTime.SetMillisecond(0);
        OldDateTime.SetMillisecond(0);

        // fix up a bad status or reset for newer module
        if (iStatus > kModuleNew || !OldDateTime.IsEqualTo(DateTime)) {
            iStatus=kModuleNew;
        }
    } else {
        // Remove previously saved since it's no longer valid
        gPrefs->DeleteEntry(PathPref);
        gPrefs->DeleteEntry(StatusPref);
        gPrefs->DeleteEntry(DateTimePref);
    }

    if (iStatus == kModuleNew) {
        if (autoEnabledModules().count(ShortName)) {
            iStatus = kModuleEnabled;
        }
    }

    return iStatus;
}

void ModuleSettings::SetModuleStatus(const FilePath& fname, int iStatus)
{
    wxFileName FileName(fname);
    wxDateTime DateTime = FileName.GetModificationTime();
    wxString ShortName = FileName.GetName().Lower();

    wxString PrefName = wxString(wxT("/Module/")) + ShortName;
    gPrefs->Write(PrefName, iStatus);

    PrefName = wxString(wxT("/ModulePath/")) + ShortName;
    gPrefs->Write(PrefName, fname);

    PrefName = wxString(wxT("/ModuleDateTime/")) + ShortName;
    gPrefs->Write(PrefName, DateTime.FormatISOCombined());

    gPrefs->Flush();
}
