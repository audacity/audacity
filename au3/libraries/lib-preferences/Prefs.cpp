/**********************************************************************

  Audacity: A Digital Audio Editor

  Prefs.cpp

  Dominic Mazzoni

*******************************************************************//*!

\file Prefs.cpp
\brief Utility functions for working with our wxConf (gPrefs)


  Audacity uses wxWidgets' wxConfig class to handle preferences.
  See Prefs.h for more information on how it works...

\verbatim
  Note: The info below is very outdated and incomplete

  Preference field specification:
   /
      Version					- Audacity Version that created these prefs
      DefaultOpenPath			- Default directory for NEW file selector
   /FileFormats
      ExportFormat_SF1		   - Format to export PCM data in
                             (this number is a libsndfile1.0 format)
   /SamplingRate
      DefaultProjectSampleRate- New projects will have this rate
         [ 8000, 11025, 16000, 22050, 44100, 48000 ]
   /AudioIO
      PlaybackDevice			- device to use for playback
      RecordingDevice			- device to use for recording
         (these are device names understood by PortAudio)
   /Display
      WaveformColor			- 0xRRGGBB  --since it will be stored in
      ShadowColor				-             decimal, it will be somewhat
      SpectrumLowColor		-             non-intuitive to edit, but
      SpectrumHighColor		-             much easier to parse.
   /Locale
      Language				- two-letter language code for translations

   (*): wxGTK
   (+): wxWin
   ($): wxMac
\endverbatim

*//*******************************************************************/

#include "Prefs.h"

#include <wx/defs.h>
#include <wx/app.h>
#include <wx/filename.h>
#include <wx/stdpaths.h>

#include "BasicUI.h"
#include "Internat.h"
#include "IteratorX.h"
#include "Observer.h"

StickySetting<BoolSetting> DefaultUpdatesCheckingFlag{
    L"/Update/DefaultUpdatesChecking", true };

std::unique_ptr<audacity::BasicSettings> ugPrefs {};

audacity::BasicSettings* gPrefs = nullptr;
int gMenusDirty = 0;

int gVersionMajorKeyInit{};
int gVersionMinorKeyInit{};
int gVersionMicroKeyInit{};

struct PrefsListener::Impl
{
    Impl(PrefsListener& owner);
    ~Impl();
    void OnEvent(int id);
    PrefsListener& mOwner;
    Observer::Subscription mSubscription;
};

namespace {
class PreferencesResetHandlerRegistry
{
    std::vector<std::unique_ptr<PreferencesResetHandler> > mHandlers;
public:
    static PreferencesResetHandlerRegistry& Get()
    {
        static PreferencesResetHandlerRegistry registry;
        return registry;
    }

    void Register(std::unique_ptr<PreferencesResetHandler> handler)
    {
        mHandlers.push_back(std::move(handler));
    }

    void BeginReset()
    {
        for (auto& handler : mHandlers) {
            handler->OnSettingResetBegin();
        }
    }

    void EndReset()
    {
        for (auto& handler : mHandlers) {
            handler->OnSettingResetEnd();
        }
    }
};

struct Hub : Observer::Publisher<int>
{
    using Publisher::Publish;
};

static Hub& hub()
{
    static Hub theHub;
    return theHub;
}
}

void PrefsListener::Broadcast(int id)
{
    BasicUI::CallAfter([id]{
        hub().Publish(id);
    });
}

PrefsListener::Impl::Impl(PrefsListener& owner)
    : mOwner{owner}
{
    mSubscription = hub().Subscribe(*this, &Impl::OnEvent);
}

PrefsListener::Impl::~Impl()
{
}

PrefsListener::PrefsListener()
    : mpImpl{std::make_unique<Impl>(*this)}
{
}

PrefsListener::~PrefsListener()
{
}

void PrefsListener::UpdatePrefs()
{
}

void PrefsListener::UpdateSelectedPrefs(int)
{
}

void PrefsListener::Impl::OnEvent(int id)
{
    if (id <= 0) {
        mOwner.UpdatePrefs();
    } else {
        mOwner.UpdateSelectedPrefs(id);
    }
}

#if 0
// Copy one entry from one wxConfig object to another
static void CopyEntry(wxString path, wxConfigBase* src, wxConfigBase* dst, wxString entry)
{
    switch (src->GetEntryType(entry)) {
    case wxConfigBase::Type_Unknown:
    case wxConfigBase::Type_String: {
        wxString value = src->Read(entry, wxT(""));
        dst->Write(path + entry, value);
        break;
    }
    case wxConfigBase::Type_Boolean: {
        bool value = false;
        src->Read(entry, &value, value);
        dst->Write(path + entry, value);
        break;
    }
    case wxConfigBase::Type_Integer: {
        long value = false;
        src->Read(entry, &value, value);
        dst->Write(path + entry, value);
        break;
    }
    case wxConfigBase::Type_Float: {
        double value = false;
        src->Read(entry, &value, value);
        dst->Write(path + entry, value);
        break;
    }
    }
}

// Recursive routine to copy all groups and entries from one wxConfig object to another
static void CopyEntriesRecursive(wxString path, wxConfigBase* src, wxConfigBase* dst)
{
    wxString entryName;
    long entryIndex;
    bool entryKeepGoing;

    entryKeepGoing = src->GetFirstEntry(entryName, entryIndex);
    while (entryKeepGoing) {
        CopyEntry(path, src, dst, entryName);
        entryKeepGoing = src->GetNextEntry(entryName, entryIndex);
    }

    wxString groupName;
    long groupIndex;
    bool groupKeepGoing;

    groupKeepGoing = src->GetFirstGroup(groupName, groupIndex);
    while (groupKeepGoing) {
        wxString subPath = path + groupName + wxT("/");
        src->SetPath(subPath);
        CopyEntriesRecursive(subPath, src, dst);
        src->SetPath(path);
        groupKeepGoing = src->GetNextGroup(groupName, groupIndex);
    }
}

#endif

void InitPreferences(std::unique_ptr<audacity::BasicSettings> uPrefs)
{
    gPrefs = uPrefs.get();
    ugPrefs = std::move(uPrefs);
    //wxConfigBase::Set(gPrefs);
    PrefsListener::Broadcast();
}

void GetPreferencesVersion(int& vMajor, int& vMinor, int& vMicro)
{
    vMajor = gVersionMajorKeyInit;
    vMinor = gVersionMinorKeyInit;
    vMicro = gVersionMicroKeyInit;
}

void SetPreferencesVersion(int vMajor, int vMinor, int vMicro)
{
    gVersionMajorKeyInit = vMajor;
    gVersionMinorKeyInit = vMinor;
    gVersionMicroKeyInit = vMicro;
}

void ResetPreferences()
{
    PreferencesResetHandlerRegistry::Get().BeginReset();

    gPrefs->Clear();

    PreferencesResetHandlerRegistry::Get().EndReset();
}

void FinishPreferences()
{
    if (gPrefs) {
        ugPrefs.reset();
        gPrefs = nullptr;
    }
}

namespace {
std::vector<SettingScope*> sScopes;
}

SettingScope::SettingScope()
{
    sScopes.push_back(this);
}

SettingScope::~SettingScope() noexcept
{
    // Settings can be scoped only on stack
    // so it should be safe to assume that sScopes.top() == this;
    assert(!sScopes.empty() && sScopes.back() == this);

    if (sScopes.empty() || sScopes.back() != this) {
        return;
    }

    if (!mCommitted) {
        for (auto pSetting : mPending) {
            pSetting->Rollback();
        }
    }

    sScopes.pop_back();
}

// static
auto SettingScope::Add(TransactionalSettingBase& setting) -> AddResult
{
    if (sScopes.empty() || sScopes.back()->mCommitted) {
        return NotAdded;
    }

    const bool inserted = sScopes.back()->mPending.insert(&setting).second;

    if (inserted) {
        setting.EnterTransaction(sScopes.size());

        // We need to introduce this setting into all
        // previous scopes that do not yet contain it.
        for (auto it = sScopes.rbegin() + 1; it != sScopes.rend(); ++it) {
            if ((*it)->mPending.find(&setting) != (*it)->mPending.end()) {
                break;
            }

            (*it)->mPending.insert(&setting);
        }
    }

    return inserted ? Added : PreviouslyAdded;
}

bool SettingTransaction::Commit()
{
    if (sScopes.empty() || sScopes.back() != this) {
        return false;
    }

    if (!mCommitted) {
        for ( auto pSetting : mPending ) {
            if (!pSetting->Commit()) {
                return false;
            }
        }

        if (sScopes.size() > 1 || gPrefs->Flush()) {
            mPending.clear();
            mCommitted = true;
            return true;
        }
    }

    return false;
}

//////////
EnumValueSymbols::EnumValueSymbols(
    ByColumns_t,
    const TranslatableStrings& msgids,
    wxArrayStringEx internals)
    : mInternals(std::move(internals))
{
    auto size = mInternals.size(), size2 = msgids.size();
    if (size != size2) {
        wxASSERT(false);
        size = std::min(size, size2);
    }
    reserve(size);
    auto iter1 = mInternals.begin();
    auto iter2 = msgids.begin();
    while (size--) {
        emplace_back(*iter1++, *iter2++);
    }
}

const TranslatableStrings& EnumValueSymbols::GetMsgids() const
{
    if (mMsgids.empty()) {
        mMsgids = transform_container<TranslatableStrings>(*this,
                                                           std::mem_fn(&EnumValueSymbol::Msgid));
    }
    return mMsgids;
}

const wxArrayStringEx& EnumValueSymbols::GetInternals() const
{
    if (mInternals.empty()) {
        mInternals = transform_container<wxArrayStringEx>(*this,
                                                          std::mem_fn(&EnumValueSymbol::Internal));
    }
    return mInternals;
}

//////////
const EnumValueSymbol& ChoiceSetting::Default() const
{
    if (mDefaultSymbol >= 0 && mDefaultSymbol < (long)mSymbols.size()) {
        return mSymbols[ mDefaultSymbol ];
    }
    static EnumValueSymbol empty;
    return empty;
}

wxString ChoiceSetting::Read() const
{
    const auto& defaultValue = Default().Internal();
    return ReadWithDefault(defaultValue);
}

wxString ChoiceSetting::ReadWithDefault(const wxString& defaultValue) const
{
    wxString value;
    if (!gPrefs->Read(mKey, &value, defaultValue)) {
        if (!mMigrated) {
            const_cast<ChoiceSetting*>(this)->Migrate(value);
            mMigrated = true;
        }
    }

    // Remap to default if the string is not known -- this avoids surprises
    // in case we try to interpret config files from future versions
    auto index = Find(value);
    if (index >= mSymbols.size()) {
        value = defaultValue;
    }
    return value;
}

size_t ChoiceSetting::Find(const wxString& value) const
{
    auto start = GetSymbols().begin();
    return size_t(
        std::find(start, GetSymbols().end(), EnumValueSymbol { value, {} })
        - start);
}

void ChoiceSetting::Migrate(wxString& value)
{
    (void)value;// Compiler food
}

bool ChoiceSetting::Write(const wxString& value)
{
    auto index = Find(value);
    if (index >= mSymbols.size()) {
        return false;
    }

    auto result = gPrefs->Write(mKey, value);
    mMigrated = true;

    if (mpOtherSettings) {
        mpOtherSettings->Invalidate();
    }

    return result;
}

void ChoiceSetting::SetDefault(long value)
{
    mDefaultSymbol = value;
}

int EnumSettingBase::ReadInt() const
{
    auto index = Find(Read());

    wxASSERT(index < mIntValues.size());
    return mIntValues[ index ];
}

int EnumSettingBase::ReadIntWithDefault(int defaultValue) const
{
    wxString defaultString;
    auto index0 = FindInt(defaultValue);
    if (index0 < mSymbols.size()) {
        defaultString = mSymbols[ index0 ].Internal();
    } else {
        wxASSERT(false);
    }

    auto index = Find(ReadWithDefault(defaultString));

    wxASSERT(index < mSymbols.size());
    return mIntValues[ index ];
}

size_t EnumSettingBase::FindInt(int code) const
{
    const auto start = mIntValues.begin();
    return size_t(
        std::find(start, mIntValues.end(), code)
        - start);
}

void EnumSettingBase::Migrate(wxString& value)
{
    int intValue = 0;
    if (!mOldKey.empty()
        && gPrefs->Read(mOldKey, &intValue, 0)) {
        // Make the migration, only once and persistently.
        // Do not DELETE the old key -- let that be read if user downgrades
        // Audacity.  But further changes will be stored only to the NEW key
        // and won't be seen then.
        auto index = (long)FindInt(intValue);
        if (index >= (long)mSymbols.size()) {
            index = mDefaultSymbol;
        }
        if (index >= 0 && index < (long)mSymbols.size()) {
            value = mSymbols[index].Internal();
            Write(value);
            gPrefs->Flush();
        }
    }
}

void PreferencesResetHandler::Register(std::unique_ptr<PreferencesResetHandler> handler)
{
    PreferencesResetHandlerRegistry::Get().Register(std::move(handler));
}

PreferencesResetHandler::~PreferencesResetHandler() = default;

bool EnumSettingBase::WriteInt(int code)   // you flush gPrefs afterward
{
    auto index = FindInt(code);
    if (index >= mSymbols.size()) {
        return false;
    }
    return Write(mSymbols[index].Internal());
}

wxString WarningDialogKey(const wxString& internalDialogName)
{
    return wxT("/Warnings/") + internalDialogName;
}

ByColumns_t ByColumns{};

#include <set>

namespace {
using PreferenceInitializers = std::set< PreferenceInitializer* >;
PreferenceInitializers& allInitializers()
{
    static PreferenceInitializers theSet;
    return theSet;
}
}

PreferenceInitializer::PreferenceInitializer()
{
    allInitializers().insert(this);
}

PreferenceInitializer::~PreferenceInitializer()
{
    allInitializers().erase(this);
}

void PreferenceInitializer::ReinitializeAll()
{
    for ( auto pInitializer : allInitializers()) {
        (*pInitializer)();
    }
}

audacity::BasicSettings* SettingBase::GetConfig() const
{
    return gPrefs;
}

bool SettingBase::Delete()
{
    auto config = GetConfig();
    return config && config->DeleteEntry(GetPath());
}

bool BoolSetting::Toggle()
{
    bool value = Read();
    return Write(!value);
}
