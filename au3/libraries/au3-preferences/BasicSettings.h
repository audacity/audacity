/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  BasicSettings.h

  Vitaly Sverchinsky

**********************************************************************/
#pragma once

#include <memory>
#include <wx/string.h>
#include <wx/arrstr.h>
#include "GlobalVariable.h"

namespace audacity {
/**
    * @brief Base class for objects that provide facility to store
    * data persistently, and access it with string keys that are formed
    * similarly to how paths in tree are formed.
    *
    * @details Each group (node) in path separated using '/'(slash) symbol. Path could be empty,
    * in that case value belongs to the root group. If path starts with slash then it's
    * considered to be absolute, otherwise it's relative to the current group, which
    * could be changed using `BeginGroup` method.
    */
class PREFERENCES_API BasicSettings
{
public:
    class PREFERENCES_API GroupScope final
    {
        friend class BasicSettings;

        std::optional<std::reference_wrapper<BasicSettings> > mSettings;

        GroupScope(BasicSettings& settings);

    public:
        GroupScope(const GroupScope&) = delete;
        GroupScope(GroupScope&&) = delete;
        GroupScope& operator=(const GroupScope&) = delete;
        GroupScope& operator=(GroupScope&&) = delete;

        void Reset() noexcept;

        ~GroupScope();
    };

    BasicSettings();
    virtual ~BasicSettings();

    BasicSettings(const BasicSettings&) = delete;
    BasicSettings(BasicSettings&&) = default;
    BasicSettings& operator=(const BasicSettings&) = delete;
    BasicSettings& operator=(BasicSettings&&) = default;

    ///@brief Returns current group prefix
    virtual wxString GetGroup() const = 0;
    ///@brief Returns all child groups within the current group
    virtual wxArrayString GetChildGroups() const = 0;
    ///@brief Returns all child keys within the current group
    virtual wxArrayString GetChildKeys() const = 0;

    ///@brief Checks whether specified key exists within the current group
    virtual bool HasEntry(const wxString& key) const = 0;
    ///@brief Checks whether specified group exists relative to the current group
    virtual bool HasGroup(const wxString& key) const = 0;
    ///@brief Returns true if group or entry exists
    virtual bool Exists(const wxString& key) const;

    /// @brief Removes group or entry within the current group, if exists.
    /// Pass empty string to remove each entry in the current group
    virtual bool Remove(const wxString& key) = 0;
    ///@brief Remove all groups and keys
    virtual void Clear() = 0;

    /// @brief Deletes specified group if exists
    bool DeleteGroup(const wxString& key);
    /// @brief Deletes specified entry if exists
    bool DeleteEntry(const wxString& key);

    /// @brief Appends a prefix to the current group or sets a new
    /// absolute path. Group that was set as current before `BeginGroup`
    /// is called, will be restored once `GroupScope` is destroyed.
    GroupScope BeginGroup(const wxString& prefix);

    virtual bool Read(const wxString& key, bool* value) const = 0;
    virtual bool Read(const wxString& key, int* value) const = 0;
    virtual bool Read(const wxString& key, long* value) const = 0;
    virtual bool Read(const wxString& key, long long* value) const = 0;
    virtual bool Read(const wxString& key, double* value) const = 0;
    virtual bool Read(const wxString& key, wxString* value) const = 0;
    virtual bool Read(const wxString& key, float* value) const;

    /// @brief Uses wxFromString to read object
    template<typename T>
    bool Read(const wxString& key, T* value) const
    {
        wxString str;
        if (!Read(key, &str)) {
            return false;
        }
        return wxFromString(str, value);
    }

    template<typename T>
    std::enable_if_t<std::is_scalar_v<T>, bool>
    Read(const wxString& key, T* value, T defaultValue) const
    {
        if (!Read(key, value)) {
            *value = defaultValue;
            return false;
        }
        return true;
    }

    template<typename T>
    std::enable_if_t<!std::is_scalar_v<T>, bool>
    Read(const wxString& key, T* value, const T& defaultValue)
    {
        if (!Read(key, value)) {
            *value = defaultValue;
            return false;
        }
        return true;
    }

    wxString Read(const wxString& key, const wxString& defaultValue = wxEmptyString) const;
    wxString Read(const wxString& key, const char* defaultValue) const;
    wxString Read(const wxString& key, const wchar_t* defaultValue) const;

    template<typename T>
    std::enable_if_t<std::is_scalar_v<T>, T>
    Read(const wxString& key, T defaultValue) const
    {
        T value;
        if (!Read(key, &value)) {
            return defaultValue;
        }
        return value;
    }

    template<typename T>
    std::enable_if_t<!std::is_scalar_v<T>, T>
    Read(const wxString& key, const T& defaultValue) const
    {
        T value;
        if (!Read(key, &value)) {
            return defaultValue;
        }
        return value;
    }

    virtual bool Write(const wxString& key, bool value) = 0;
    virtual bool Write(const wxString& key, int value) = 0;
    virtual bool Write(const wxString& key, long value) = 0;
    virtual bool Write(const wxString& key, long long value) = 0;
    virtual bool Write(const wxString& key, double value) = 0;
    virtual bool Write(const wxString& key, const wxString& value) = 0;

    virtual bool Write(const wxString& key, float value);
    virtual bool Write(const wxString& key, const char* value);
    virtual bool Write(const wxString& key, const wchar_t* value);

    /// @brief Uses wxToString to convert object into string
    template<typename T>
    bool Write(const wxString& key, const T& value)
    {
        return Write(key, wxToString(value));
    }

    virtual bool Flush() noexcept = 0;

    bool ReadBool(const wxString& key, bool defaultValue) const;
    long ReadLong(const wxString& key, long defaultValue) const;
    double ReadDouble(const wxString& key, double defaultValue) const;

    template<typename T>
    T ReadObject(const wxString& key, const T& defaultValue) const
    {
        return Read(key, defaultValue);
    }

protected:

    virtual void DoBeginGroup(const wxString& prefix) = 0;
    virtual void DoEndGroup() noexcept = 0;
};

/// @brief Provides an access to application-wise settings
struct PREFERENCES_API ApplicationSettings final : GlobalHook<ApplicationSettings, std::unique_ptr<BasicSettings>()> { };
}
