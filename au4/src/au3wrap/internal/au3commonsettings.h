/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "libraries/lib-preferences/BasicSettings.h"

namespace au::au3 {
class Au3CommonSettings : public audacity::BasicSettings
{
public:
    Au3CommonSettings();

    wxString GetGroup() const override;
    wxArrayString GetChildGroups() const override;
    wxArrayString GetChildKeys() const override;

    bool HasEntry(const wxString& /*key*/) const override;
    bool HasGroup(const wxString& /*key*/) const override;
    bool Remove(const wxString& /*key*/) override;
    void Clear() override;

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

    bool Flush() noexcept override;

protected:

    void DoBeginGroup(const wxString& /*prefix*/) override;
    void DoEndGroup() noexcept override;
};
}
