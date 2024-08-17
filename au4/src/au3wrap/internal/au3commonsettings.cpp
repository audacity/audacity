/*
* Audacity: A Digital Audio Editor
*/
#include "au3commonsettings.h"

#include "../internal/wxtypes_convert.h"

#include "global/settings.h"
#include "log.h"

using namespace au::au3;

static muse::Settings::Key make_key(const wxString& key)
{
    wxString _key = key;
    if (!_key.starts_with("/")) {
        _key += "/";
    }

    return muse::Settings::Key("au3wrap", wxToStdSting(_key));
}

Au3CommonSettings::Au3CommonSettings()
{
}

wxString Au3CommonSettings::GetGroup() const
{
    NOT_IMPLEMENTED;
    return wxString("mock_group");
}

wxArrayString Au3CommonSettings::GetChildGroups() const
{
    NOT_IMPLEMENTED;
    return {};
}

wxArrayString Au3CommonSettings::GetChildKeys() const
{
    NOT_IMPLEMENTED;
    return {};
}

bool Au3CommonSettings::HasEntry(const wxString& key) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }
    return true;
}

bool Au3CommonSettings::HasGroup(const wxString& key) const
{
    NOT_IMPLEMENTED << ", key: " << wxToStdSting(key);
    return false;
}

bool Au3CommonSettings::Remove(const wxString& key)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val());
    return true;
}

void Au3CommonSettings::Clear()
{
    muse::settings()->reset(false);
}

void Au3CommonSettings::DoBeginGroup(const wxString& prefix)
{
    NOT_IMPLEMENTED << ", prefix: " << wxToStdSting(prefix);
}

void Au3CommonSettings::DoEndGroup() noexcept
{
    NOT_IMPLEMENTED;
}

bool Au3CommonSettings::Flush() noexcept
{
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, bool* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toBool();
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, int* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toInt();
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, long* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toInt64();
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, long long* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toInt64();
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, double* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toDouble();
    return true;
}

bool Au3CommonSettings::Read(const wxString& key, wxString* value) const
{
    muse::Val val = muse::settings()->value(make_key(key));
    if (val.isNull()) {
        return false;
    }

    *value = val.toString();
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, bool value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(value));
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, int value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(value));
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, long value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(static_cast<int64_t>(value)));
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, long long value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(static_cast<int64_t>(value)));
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, double value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(value));
    return true;
}

bool Au3CommonSettings::Write(const wxString& key, const wxString& value)
{
    muse::settings()->setLocalValue(make_key(key), muse::Val(wxToStdSting(value)));
    return true;
}
