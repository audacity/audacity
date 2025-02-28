#include "SettingsWX.h"

#include <wx/confbase.h>
#include <wx/fileconf.h>

void SettingsWX::DoBeginGroup(const wxString& prefix)
{
    if (prefix.StartsWith("/")) {
        mGroupStack.push_back(prefix);
    } else {
        if (mGroupStack.size() > 1) {
            mGroupStack.push_back(mGroupStack.Last() + "/" + prefix);
        } else {
            mGroupStack.push_back("/" + prefix);
        }
    }
    //This creates group if it didn't exist
    mConfig->SetPath(mGroupStack.Last());
}

void SettingsWX::DoEndGroup() noexcept
{
    assert(mGroupStack.size() > 1);// "No matching DoBeginGroup"

    if (mGroupStack.size() > 1) {
        mGroupStack.pop_back();
    }

    mConfig->SetPath(mGroupStack.Last());
}

SettingsWX::SettingsWX(std::shared_ptr<wxConfigBase> config)
    : mConfig{std::move(config)}
{
    mGroupStack.push_back("/");
}

SettingsWX::SettingsWX(const wxString& filepath)
{
    mConfig = std::make_shared<wxFileConfig>(wxEmptyString, wxEmptyString, filepath);
    mGroupStack.push_back("/");
}

SettingsWX::~SettingsWX()
{
    mConfig->Flush();
}

wxString SettingsWX::GetGroup() const
{
    assert(!mGroupStack.empty());
    if (mGroupStack.size() > 1) {
        const auto& path = mGroupStack.Last();
        return path.Right(path.Length() - 1);
    }
    return {};
}

wxArrayString SettingsWX::GetChildGroups() const
{
    long index;
    wxString group;

    if (mConfig->GetFirstGroup(group, index)) {
        wxArrayString groups;
        groups.push_back(group);
        while (mConfig->GetNextGroup(group, index)) {
            groups.push_back(group);
        }
        return groups;
    }
    return {};
}

wxArrayString SettingsWX::GetChildKeys() const
{
    long index;
    wxString key;
    if (mConfig->GetFirstEntry(key, index)) {
        wxArrayString keys;
        keys.push_back(key);
        while (mConfig->GetNextEntry(key, index)) {
            keys.push_back(key);
        }
        return keys;
    }
    return {};
}

bool SettingsWX::HasEntry(const wxString& key) const
{
    return mConfig->HasEntry(MakePath(key));
}

bool SettingsWX::HasGroup(const wxString& key) const
{
    return mConfig->HasGroup(MakePath(key));
}

bool SettingsWX::Remove(const wxString& key)
{
    if (key.empty()) {
        for (auto& group : GetChildGroups()) {
            mConfig->DeleteGroup(group);
        }
        for (auto& entry : GetChildKeys()) {
            mConfig->DeleteEntry(entry, false);
        }
        return true;
    }
    const auto path = MakePath(key);
    if (mConfig->HasEntry(path)) {
        return mConfig->DeleteEntry(path, false);
    }
    if (mConfig->HasGroup(path)) {
        return mConfig->DeleteGroup(path);
    }
    return false;
}

void SettingsWX::Clear()
{
    mConfig->DeleteAll();
}

bool SettingsWX::Read(const wxString& key, bool* value) const
{
    return mConfig->Read(MakePath(key), value);
}

bool SettingsWX::Read(const wxString& key, int* value) const
{
    return mConfig->Read(MakePath(key), value);
}

bool SettingsWX::Read(const wxString& key, long* value) const
{
    return mConfig->Read(MakePath(key), value);
}

bool SettingsWX::Read(const wxString& key, long long* value) const
{
    wxString str;
    if (mConfig->Read(MakePath(key), &str)) {
        if (str.ToLongLong(value)) {
            return true;
        }
    }
    return false;
}

bool SettingsWX::Read(const wxString& key, double* value) const
{
    return mConfig->Read(MakePath(key), value);
}

bool SettingsWX::Read(const wxString& key, wxString* value) const
{
    return mConfig->Read(MakePath(key), value);
}

bool SettingsWX::Write(const wxString& key, bool value)
{
    return mConfig->Write(MakePath(key), value);
}

bool SettingsWX::Write(const wxString& key, int value)
{
    return mConfig->Write(MakePath(key), value);
}

bool SettingsWX::Write(const wxString& key, long value)
{
    return mConfig->Write(MakePath(key), value);
}

bool SettingsWX::Write(const wxString& key, long long value)
{
    return mConfig->Write(MakePath(key), wxString::Format("%lld", value));
}

bool SettingsWX::Write(const wxString& key, double value)
{
    return mConfig->Write(MakePath(key), value);
}

bool SettingsWX::Write(const wxString& key, const wxString& value)
{
    return mConfig->Write(MakePath(key), value);
}

bool SettingsWX::Flush() noexcept
{
    try
    {
        return mConfig->Flush();
    }
    catch (...)
    {
        //TODO: log error
    }
    return false;
}

wxString SettingsWX::MakePath(const wxString& key) const
{
    if (key.StartsWith("/")) {
        return key;
    }
    if (mGroupStack.size() > 1) {
        return mGroupStack.Last() + "/" + key;
    }
    return "/" + key;
}
