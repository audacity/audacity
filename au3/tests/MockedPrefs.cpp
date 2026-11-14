/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MockedPrefs.cpp

  Dmitry Vedenko

**********************************************************************/
#include "MockedPrefs.h"

#include "Prefs.h"

#include <unordered_map>
#include <variant>

class MockedSettings final : public audacity::BasicSettings
{
    std::vector<wxString> mGroupStack { "/" };

    using DataType = std::variant<
        wxString,
        bool,
        int,
        long,
        long long,
        double>;

    template<typename T>
    bool DoRead(const wxString& key, T* value) const
    {
        auto it = mStorage.find(MakePath(key));
        if (it == mStorage.end()) {
            return false;
        }
        if (auto ptr = std::get_if<T>(&it->second)) {
            *value = *ptr;
            return true;
        }
        return false;
    }

    bool DoWrite(const wxString& key, DataType value)
    {
        mStorage[key] = std::move(value);
        return true;
    }

protected:

    void DoBeginGroup(const wxString& prefix) override
    {
        mGroupStack.push_back(prefix);
    }

    void DoEndGroup() noexcept override
    {
        mGroupStack.pop_back();
    }

    bool HasGroup(const wxString& key) const override
    {
        return true;
    }

    bool HasEntry(const wxString& key) const override
    {
        return mStorage.find(MakePath(key)) != mStorage.end();
    }

    bool Flush() noexcept override
    {
        return true;
    }

    wxString GetGroup() const override
    {
        return mGroupStack.back();
    }

    wxArrayString GetChildGroups() const override
    {
        return { };
    }

    wxArrayString GetChildKeys() const override
    {
        return { };
    }

    bool Remove(const wxString& key) override
    {
        if (key.empty()) {
            for (auto it = mStorage.begin(); it != mStorage.end(); ++it) {
                if (it->first.StartsWith(key)) {
                    it = mStorage.erase(it);
                }
            }
        } else {
            auto it = mStorage.find(MakePath(key));
            if (it != mStorage.end()) {
                mStorage.erase(it);
                return true;
            }
        }
        return false;
    }

    void Clear() override
    {
        mStorage.clear();
    }

    bool Read(const wxString& key, bool* value) const override { return DoRead(key, value); }

    bool Read(const wxString& key, int* value) const override { return DoRead(key, value); }
    bool Read(const wxString& key, long* value) const override { return DoRead(key, value); }
    bool Read(const wxString& key, long long* value) const override { return DoRead(key, value); }
    bool Read(const wxString& key, double* value) const override { return DoRead(key, value); }
    bool Read(const wxString& key, wxString* value) const override { return DoRead(key, value); }

    bool Write(const wxString& key, bool value) override { return DoWrite(key, { value }); }
    bool Write(const wxString& key, int value) override { return DoWrite(key, { value }); }
    bool Write(const wxString& key, long value) override { return DoWrite(key, { value }); }
    bool Write(const wxString& key, long long value) override { return DoWrite(key, { value }); }
    bool Write(const wxString& key, double value) override { return DoWrite(key, { value }); }
    bool Write(const wxString& key, const wxString& value) override { return DoWrite(key, { value }); }

private:

    wxString MakePath(const wxString& key) const
    {
        return mGroupStack.back() + "/" + key;
    }

    std::unordered_map<wxString, DataType> mStorage;
};

MockedPrefs::MockedPrefs()
{
    InitPreferences(std::make_unique<MockedSettings>());
}

MockedPrefs::~MockedPrefs ()
{
    FinishPreferences();
}
