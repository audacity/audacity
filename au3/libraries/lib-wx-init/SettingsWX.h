#pragma once

#include "BasicSettings.h"

#include <memory>
#include <wx/string.h>
#include <wx/arrstr.h>

class wxConfigBase;

class WX_INIT_API SettingsWX final : public audacity::BasicSettings
{
    wxArrayString mGroupStack;
    std::shared_ptr<wxConfigBase> mConfig;
protected:
    void DoBeginGroup(const wxString& prefix) override;
    void DoEndGroup() noexcept override;

public:
    explicit SettingsWX(std::shared_ptr<wxConfigBase> config);
    ///@brief Constructs BasicSettings object to access BasicSettings stored in the file,
    ///using platform-specific format
    explicit SettingsWX(const wxString& filepath);
    ~SettingsWX() override;

    wxString GetGroup() const override;
    wxArrayString GetChildGroups() const override;
    wxArrayString GetChildKeys() const override;

    bool HasEntry(const wxString& key) const override;
    bool HasGroup(const wxString& key) const override;
    bool Remove(const wxString& key) override;
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

private:
    wxString MakePath(const wxString& key) const;
};
