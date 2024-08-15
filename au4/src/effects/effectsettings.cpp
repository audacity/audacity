/*
 * Audacity: A Digital Audio Editor
 */
#include "effectsettings.h"

#include <algorithm>

using namespace au::au3;

namespace {
auto ToQString(const wxString& s)
{
    return QString::fromStdString(s.ToStdString());
}

template<typename T>
bool ReadValue(
    const QSettings& settings, const wxString& key, T* value,
    std::function<T(const QVariant&)> converter)
{
    const QVariant val = settings.value(ToQString(key));
    if (!val.isValid()) {
        return false;
    }
    *value = converter(val);
    return true;
}

auto WoSlashes(const wxString& s)
{
    std::string group = s.ToStdString();
    if (group.front() == '/') {
        group.erase(0, 1);
    }
    if (group.back() == '/') {
        group.pop_back();
    }
    return QString::fromStdString(group);
}
} // namespace

EffectSettings::EffectSettings(const std::string& filename)
    : m_QSettings{QString::fromStdString(filename),
                  QSettings::defaultFormat()}
{
}

wxString EffectSettings::GetGroup() const
{
    return m_QSettings.group().toStdString();
}

wxArrayString EffectSettings::GetChildGroups() const
{
    const QStringList list = m_QSettings.childGroups();
    wxArrayString groups;
    std::transform(
        list.begin(), list.end(), std::back_inserter(groups),
        [](const QString& s) { return s.toStdString(); });
    return groups;
}

wxArrayString EffectSettings::GetChildKeys() const
{
    const QStringList list = m_QSettings.childKeys();
    wxArrayString keys;
    std::transform(
        list.begin(), list.end(), std::back_inserter(keys),
        [](const QString& s) { return s.toStdString(); });
    return keys;
}

bool EffectSettings::HasEntry(const wxString& key) const
{
    return m_QSettings.contains(ToQString(key));
}

bool EffectSettings::HasGroup(const wxString& group) const
{
    return m_QSettings.childGroups().contains(WoSlashes(group));
}

bool EffectSettings::Remove(const wxString& key)
{
    m_QSettings.remove(ToQString(key));
    return true;
}

void EffectSettings::Clear()
{
    m_QSettings.clear();
}

bool EffectSettings::Read(const wxString& key, bool* value) const
{
    return ReadValue<bool>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toBool(); });
}

bool EffectSettings::Read(const wxString& key, int* value) const
{
    return ReadValue<int>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toInt(); });
}

bool EffectSettings::Read(const wxString& key, long* value) const
{
    return ReadValue<long>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toLongLong();
    });
}

bool EffectSettings::Read(const wxString& key, long long* value) const
{
    return ReadValue<long long>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toLongLong();
    });
}

bool EffectSettings::Read(const wxString& key, double* value) const
{
    return ReadValue<double>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toDouble(); });
}

bool EffectSettings::Read(const wxString& key, wxString* value) const
{
    return ReadValue<wxString>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toString().toStdString();
    });
}

bool EffectSettings::Write(const wxString& key, bool value)
{
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectSettings::Write(const wxString& key, int value)
{
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectSettings::Write(const wxString& key, long value)
{
    m_QSettings.setValue(ToQString(key), QVariant::fromValue(value));
    return true;
}

bool EffectSettings::Write(const wxString& key, long long value)
{
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectSettings::Write(const wxString& key, double value)
{
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectSettings::Write(const wxString& key, const wxString& value)
{
    m_QSettings.setValue(ToQString(key), ToQString(value));
    return true;
}

bool EffectSettings::Flush() noexcept
{
    m_QSettings.sync();
    return true;
}

void EffectSettings::DoBeginGroup(const wxString& prefix)
{
    m_QSettings.beginGroup(WoSlashes(prefix));
}

void EffectSettings::DoEndGroup() noexcept
{
    m_QSettings.endGroup();
}
