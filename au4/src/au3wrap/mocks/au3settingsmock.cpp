#include "au3settingsmock.h"

#include "../internal/wxtypes_convert.h"

using namespace au::au3;

Au3SettingsMock::Au3SettingsMock()
{
}

bool Au3SettingsMock::Read(const wxString& key, bool* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toBool();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Read(const wxString& key, int* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toInt();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Read(const wxString& key, long* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toInt64();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Read(const wxString& key, long long* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toInt64();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Read(const wxString& key, double* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toDouble();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Read(const wxString& key, wxString* value) const
{
    auto it = m_values.find(key);
    if (it != m_values.end()) {
        *value = it->second.toString();
        return true;
    }
    return false;
}

bool Au3SettingsMock::Write(const wxString& key, bool value)
{
    m_values.insert({ key, muse::Val(value) });
    return true;
}

bool Au3SettingsMock::Write(const wxString& key, int value)
{
    m_values.insert({ key, muse::Val(value) });
    return true;
}

bool Au3SettingsMock::Write(const wxString& key, long value)
{
    m_values.insert({ key, muse::Val(static_cast<int64_t>(value)) });
    return true;
}

bool Au3SettingsMock::Write(const wxString& key, long long value)
{
    m_values.insert({ key, muse::Val(static_cast<int64_t>(value)) });
    return true;
}

bool Au3SettingsMock::Write(const wxString& key, double value)
{
    m_values.insert({ key, muse::Val(value) });
    return true;
}

bool Au3SettingsMock::Write(const wxString& key, const wxString& value)
{
    m_values.insert({ key, muse::Val(wxToStdSting(value)) });
    return true;
}
