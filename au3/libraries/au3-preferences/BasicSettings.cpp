/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  BasicSettings.h

  Vitaly Sverchinsky

**********************************************************************/

#include "BasicSettings.h"

using namespace audacity;

BasicSettings::BasicSettings() = default;

BasicSettings::~BasicSettings() = default;

bool BasicSettings::Exists(const wxString& key) const
{
    return HasEntry(key) || HasGroup(key);
}

bool BasicSettings::DeleteGroup(const wxString& key)
{
    if (HasGroup(key)) {
        return Remove(key);
    }
    return false;
}

bool BasicSettings::DeleteEntry(const wxString& key)
{
    if (HasEntry(key)) {
        return Remove(key);
    }
    return false;
}

auto BasicSettings::BeginGroup(const wxString& prefix) -> GroupScope
{
    DoBeginGroup(prefix);
    return { *this };
}

bool BasicSettings::Read(const wxString& key, float* value) const
{
    double d;
    if (Read(key, &d)) {
        *value = static_cast<float>(d);
        return true;
    }
    return false;
}

wxString BasicSettings::Read(const wxString& key, const wxString& defaultValue) const
{
    wxString value;
    if (!Read(key, &value)) {
        return defaultValue;
    }
    return value;
}

wxString BasicSettings::Read(const wxString& key, const char* defaultValue) const
{
    wxString value;
    if (!Read(key, &value)) {
        return { defaultValue };
    }
    return value;
}

wxString BasicSettings::Read(const wxString& key, const wchar_t* defaultValue) const
{
    wxString value;
    if (!Read(key, &value)) {
        return { defaultValue };
    }
    return value;
}

bool BasicSettings::ReadBool(const wxString& key, bool defaultValue) const
{
    return Read(key, defaultValue);
}

long BasicSettings::ReadLong(const wxString& key, long defaultValue) const
{
    return Read(key, defaultValue);
}

double BasicSettings::ReadDouble(const wxString& key, double defaultValue) const
{
    return Read(key, defaultValue);
}

bool BasicSettings::Write(const wxString& key, float value)
{
    return Write(key, static_cast<double>(value));
}

bool BasicSettings::Write(const wxString& key, const char* value)
{
    return Write(key, wxString(value));
}

bool BasicSettings::Write(const wxString& key, const wchar_t* value)
{
    return Write(key, wxString(value));
}

BasicSettings::GroupScope::GroupScope(BasicSettings& settings)
    : mSettings(settings)
{
}

void BasicSettings::GroupScope::Reset() noexcept
{
    if (mSettings) {
        mSettings->get().DoEndGroup();
    }
    mSettings.reset();
}

BasicSettings::GroupScope::~GroupScope() { Reset(); }
