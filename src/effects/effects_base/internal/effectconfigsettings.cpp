/*
 * Audacity: A Digital Audio Editor
 */
#include "effectconfigsettings.h"

#include "global/serialization/json.h"
#include "global/io/file.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "log.h"

using namespace muse;
using namespace au::au3;

static const std::string GENERAL("General");

EffectConfigSettings::EffectConfigSettings(const std::string& filename)
    : m_filename(filename)
{
    Load();
}

EffectConfigSettings::~EffectConfigSettings()
{
    Save();
}

void EffectConfigSettings::Load()
{
    ByteArray json;
    Ret ret = io::File::readFile(m_filename, json);
    if (!ret) {
        LOGE() << "failed read file: " << m_filename << ", err: " << ret.toString();
        return;
    }

    m_vals.clear();

    std::string err;
    JsonArray arr = JsonDocument::fromJson(json, &err).rootArray();

    if (!err.empty()) {
        LOGE() << "failed parse json from file: " << m_filename << ", err: " << err;
        return;
    }

    for (size_t i = 0; i < arr.size(); ++i) {
        JsonObject obj = arr.at(i).toObject();
        if (obj.empty()) {
            continue;
        }

        std::string key = obj.value("key").toStdString();
        std::string type = obj.value("type").toStdString();
        JsonValue val = obj.value("val");
        if (type == "bool") {
            m_vals.insert({ key, val.toBool() });
        } else if (type == "int") {
            m_vals.insert({ key, val.toInt() });
        } else if (type == "long") {
            m_vals.insert({ key, std::stol(val.toStdString()) });
        } else if (type == "long long") {
            m_vals.insert({ key, std::stoll(val.toStdString()) });
        } else if (type == "double") {
            m_vals.insert({ key, val.toDouble() });
        } else if (type == "string") {
            m_vals.insert({ key, val.toStdString() });
        }
    }
}

bool EffectConfigSettings::Save()
{
    JsonArray arr;
    for (const auto& p : m_vals) {
        JsonObject obj;
        const std::string& key = p.first;
        std::visit([&key, &obj](auto&& v) {
            using T = std::decay_t<decltype(v)>;
            // std::monostate, bool, int, long, long long, double, wxString
            if constexpr (std::is_same_v<T, std::monostate>) {
                return;
            } else if constexpr (std::is_same_v<T, bool>) {
                obj["key"] = key;
                obj["val"] = v;
                obj["type"] = "bool";
            } else if constexpr (std::is_same_v<T, int>) {
                obj["key"] = key;
                obj["val"] = v;
                obj["type"] = "int";
            } else if constexpr (std::is_same_v<T, long>) {
                obj["key"] = key;
                obj["val"] = std::to_string(v);
                obj["type"] = "long";
            } else if constexpr (std::is_same_v<T, long long>) {
                obj["key"] = key;
                obj["val"] = std::to_string(v);
                obj["type"] = "long long";
            } else if constexpr (std::is_same_v<T, double>) {
                obj["key"] = key;
                obj["val"] = v;
                obj["type"] = "double";
            } else if constexpr (std::is_same_v<T, wxString>) {
                obj["key"] = key;
                obj["val"] = au::au3::wxToStdSting(v);
                obj["type"] = "string";
            }
        }, p.second);

        arr.append(obj);
    }

    ByteArray json = JsonDocument(arr).toJson();

    Ret ret = io::File::writeFile(m_filename, json);
    if (!ret) {
        LOGE() << "failed write to file: " << m_filename << ", err: " << ret.toString();
    }

    return ret;
}

std::string EffectConfigSettings::fullKey(const wxString& key) const
{
    std::string path = au3::wxToStdSting(key);
    if (!m_currentGroup.empty()) {
        return m_currentGroup + "/" + path;
    }

    if (path.find_first_of('/') != std::string::npos) {
        return path;
    }

    return GENERAL + "/" + path;
}

wxString EffectConfigSettings::GetGroup() const
{
    if (m_currentGroup.empty()) {
        return GENERAL;
    } else {
        return m_currentGroup;
    }
}

wxArrayString EffectConfigSettings::GetChildGroups() const
{
    wxArrayString child;
    std::string group = m_currentGroup;
    for (const auto& p : m_vals) {
        const std::string& fullKey = p.first;

        std::string subgroup;
        if (group.empty()) {
            size_t sep = fullKey.find_last_of('/');
            if (sep == std::string::npos) {
                continue;
            }

            subgroup = fullKey.substr(0, sep);
        } else {
            if (fullKey.find(group) == std::string::npos) {
                continue;
            }

            std::string fullSub = fullKey.substr(group.size() + 1);
            size_t sep = fullSub.find_last_of('/');
            if (sep == std::string::npos) {
                continue;
            }

            subgroup = fullSub.substr(0, sep);
        }

        child.push_back(subgroup);
    }

    return child;
}

wxArrayString EffectConfigSettings::GetChildKeys() const
{
    wxArrayString child;
    std::string group = au3::wxToStdSting(GetGroup());
    for (const auto& p : m_vals) {
        const std::string& fullKey = p.first;
        if (fullKey.find(group) == std::string::npos) {
            continue;
        }

        std::string fullSub = fullKey.substr(group.size() + 1);
        size_t sep = fullSub.find_last_of('/');
        if (sep != std::string::npos) {
            continue;
        }

        child.push_back(fullSub);
    }

    return child;
}

bool EffectConfigSettings::HasEntry(const wxString& key) const
{
    std::string full = fullKey(key);
    return m_vals.find(full) != m_vals.end();
}

bool EffectConfigSettings::HasGroup(const wxString& group) const
{
    std::string full = fullKey(group);
    for (const auto& p : m_vals) {
        if (p.first.find(full) != std::string::npos) {
            return true;
        }
    }
    return false;
}

bool EffectConfigSettings::Remove(const wxString& key)
{
    std::string full = fullKey(key);
    std::vector<std::string> toRemoveKeys;
    for (const auto& p : m_vals) {
        if (p.first.find(full) != std::string::npos) {
            toRemoveKeys.push_back(p.first);
        }
    }

    for (const std::string& k : toRemoveKeys) {
        m_vals.erase(k);
    }

    return true;
}

void EffectConfigSettings::Clear()
{
    m_vals.clear();
}

bool EffectConfigSettings::Read(const wxString& key, bool* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Read(const wxString& key, int* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Read(const wxString& key, long* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Read(const wxString& key, long long* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Read(const wxString& key, double* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Read(const wxString& key, wxString* value) const
{
    return ReadValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, bool value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, int value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, long value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, long long value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, double value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Write(const wxString& key, const wxString& value)
{
    return WriteValue(key, value);
}

bool EffectConfigSettings::Flush() noexcept
{
    return Save();
}

void EffectConfigSettings::DoBeginGroup(const wxString& prefix)
{
    m_currentGroup = au3::wxToStdSting(prefix);
    if (!m_currentGroup.empty() && m_currentGroup.back() == '/') {
        m_currentGroup.pop_back();
    }
}

void EffectConfigSettings::DoEndGroup() noexcept
{
    m_currentGroup.clear();
}
