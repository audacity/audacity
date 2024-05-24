#ifndef AU_AU3WRAP_AU3SETTINGSMOCK_H
#define AU_AU3WRAP_AU3SETTINGSMOCK_H

#include "libraries/lib-preferences/BasicSettings.h"

#include "types/val.h"

namespace au::au3 {
//! TODO
//! * Maybe add memory store
//! * Add access logging
class Au3SettingsMock : public audacity::BasicSettings
{
public:
    Au3SettingsMock();

    wxString GetGroup() const override { return wxString("mock_group"); }
    wxArrayString GetChildGroups() const override { return {}; }
    wxArrayString GetChildKeys() const override { return {}; }

    bool HasEntry(const wxString& /*key*/) const override { return false; }
    bool HasGroup(const wxString& /*key*/) const override { return false; }
    bool Remove(const wxString& /*key*/) override { return true; }
    void Clear() override {}

    bool Read(const wxString& key, bool* value) const override;
    bool Read(const wxString& key, int* value) const override;
    bool Read(const wxString& key, long* value) const override;
    bool Read(const wxString& key, long long* value) const override;
    bool Read(const wxString& key, double* value) const override;
    bool Read(const wxString& key, wxString* value) const override;

    bool Write(const wxString& key, bool value) override;
    bool Write(const wxString& key, int value) override;
    bool Write(const wxString& key, long value) override;
    bool Write(const wxString& key, long long value) override;
    bool Write(const wxString& key, double value) override;
    bool Write(const wxString& key, const wxString& value) override;

    bool Flush() noexcept override { return true; }

protected:

    void DoBeginGroup(const wxString& /*prefix*/) override {}
    void DoEndGroup() noexcept override {}

private:
    mutable std::map<wxString, muse::Val> m_values;
};
}

#endif // AU_AU3WRAP_AU3SETTINGSMOCK_H
