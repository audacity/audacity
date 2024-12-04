/*
 * Audacity: A Digital Audio Editor
 */
#include "effectconfigsettings.h"

#include <algorithm>

#include "log.h"
#include "au3wrap/internal/wxtypes_convert.h"

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

EffectConfigSettings::EffectConfigSettings(const std::string& filename)
    : m_QSettings{QString::fromStdString(filename),
                  QSettings::defaultFormat()}
{
}

wxString EffectConfigSettings::GetGroup() const
{
    LOGDA() << "";
    return m_QSettings.group().toStdString();
}

wxArrayString EffectConfigSettings::GetChildGroups() const
{
    LOGDA() << "";
    const QStringList list = m_QSettings.childGroups();
    wxArrayString groups;
    std::transform(
        list.begin(), list.end(), std::back_inserter(groups),
        [](const QString& s) { return s.toStdString(); });
    return groups;
}

wxArrayString EffectConfigSettings::GetChildKeys() const
{
    LOGDA() << "";
    const QStringList list = m_QSettings.childKeys();
    wxArrayString keys;
    std::transform(
        list.begin(), list.end(), std::back_inserter(keys),
        [](const QString& s) { return s.toStdString(); });
    return keys;
}

bool EffectConfigSettings::HasEntry(const wxString& key) const
{
    LOGDA() << "key: " << au3::wxToStdSting(key);
    return m_QSettings.contains(ToQString(key));
}

bool EffectConfigSettings::HasGroup(const wxString& group) const
{
    LOGDA() << "group: " << au3::wxToStdSting(group);
    return m_QSettings.childGroups().contains(WoSlashes(group));
}

bool EffectConfigSettings::Remove(const wxString& key)
{
    m_QSettings.remove(ToQString(key));
    return true;
}

void EffectConfigSettings::Clear()
{
    m_QSettings.clear();
}

bool EffectConfigSettings::Read(const wxString& key, bool* value) const
{
    return ReadValue<bool>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toBool(); });
}

bool EffectConfigSettings::Read(const wxString& key, int* value) const
{
    return ReadValue<int>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toInt(); });
}

bool EffectConfigSettings::Read(const wxString& key, long* value) const
{
    return ReadValue<long>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toLongLong();
    });
}

bool EffectConfigSettings::Read(const wxString& key, long long* value) const
{
    return ReadValue<long long>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toLongLong();
    });
}

bool EffectConfigSettings::Read(const wxString& key, double* value) const
{
    return ReadValue<double>(
        m_QSettings, key, value, [](const QVariant& v) { return v.toDouble(); });
}

bool EffectConfigSettings::Read(const wxString& key, wxString* value) const
{
    return ReadValue<wxString>(m_QSettings, key, value, [](const QVariant& v) {
        return v.toString().toStdString();
    });
}

bool EffectConfigSettings::Write(const wxString& key, bool value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << value;
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectConfigSettings::Write(const wxString& key, int value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << value;
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectConfigSettings::Write(const wxString& key, long value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << value;
    m_QSettings.setValue(ToQString(key), QVariant::fromValue(value));
    return true;
}

bool EffectConfigSettings::Write(const wxString& key, long long value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << value;
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectConfigSettings::Write(const wxString& key, double value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << value;
    m_QSettings.setValue(ToQString(key), value);
    return true;
}

bool EffectConfigSettings::Write(const wxString& key, const wxString& value)
{
    LOGDA() << "key: " << au3::wxToStdSting(key) << ", val: " << au3::wxToStdSting(value);
    m_QSettings.setValue(ToQString(key), ToQString(value));
    return true;
}

bool EffectConfigSettings::Flush() noexcept
{
    m_QSettings.sync();
    return true;
}

void EffectConfigSettings::DoBeginGroup(const wxString& prefix)
{
    LOGDA() << au3::wxToStdSting(prefix);
    m_QSettings.beginGroup(WoSlashes(prefix));
}

void EffectConfigSettings::DoEndGroup() noexcept
{
    m_QSettings.endGroup();
}
