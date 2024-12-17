/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>
#include <map>
#include <variant>

#include "libraries/lib-preferences/BasicSettings.h"

namespace au::au3 {
class EffectConfigSettings final : public audacity::BasicSettings
{
public:
    EffectConfigSettings(const std::string& filename);
    ~EffectConfigSettings();

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

protected:
    void DoBeginGroup(const wxString& prefix) override;
    void DoEndGroup() noexcept override;

private:

    using Val = std::variant<std::monostate, bool, int, long, long long, double, wxString>;

    void Load();
    bool Save();

    std::string fullKey(const wxString& key) const;

    template<typename T>
    bool ReadValue(const wxString& key, T* value) const
    {
        std::string full = fullKey(key);
        auto it = m_vals.find(full);
        if (it == m_vals.end()) {
            return false;
        }
        *value = std::get<T>(it->second);
        return true;
    }

    template<typename T>
    bool WriteValue(const wxString& key, T value)
    {
        std::string full = fullKey(key);
        m_vals[full] = value;
        return true;
    }

    std::string m_filename;
    std::string m_currentGroup;
    std::map<std::string /* full key*/, Val> m_vals;
};
} // namespace au::au3
