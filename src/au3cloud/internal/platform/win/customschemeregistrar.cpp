/*
 * Audacity: A Digital Audio Editor
 */
#include "../../customschemeregistrar.h"

#include <windows.h>

#include <stdexcept>
#include <string>

#include "framework/global/log.h"

//! Windows scheme registration writes these HKCU keys:
//!
//!   HKCU\Software\Classes\<scheme>
//!     (Default)    = "URL:<scheme>"
//!     URL Protocol = ""
//!     CurVer\(Default) = "<scheme>.Url.Audacity.4"
//!   HKCU\Software\Classes\<scheme>.Url.Audacity.4
//!     DefaultIcon\(Default)       = "<audacity.exe>,1"
//!     shell\open\command\(Default) = "<audacity.exe> -u "%1""
//!
//! Rewritten on every launch so that whichever Audacity installation (both AU3 and AU4) started
//! last owns the handler.

namespace {
class RegistryKey final
{
public:
    RegistryKey()
    {
        if (RegOpenCurrentUser(KEY_ALL_ACCESS, &m_handle) != ERROR_SUCCESS) {
            throw std::runtime_error("RegOpenCurrentUser failed");
        }
    }

    RegistryKey(HKEY parent, const std::wstring& name)
    {
        LONG result = RegOpenKeyExW(parent, name.c_str(), 0, KEY_ALL_ACCESS, &m_handle);
        if (result == ERROR_FILE_NOT_FOUND) {
            result = RegCreateKeyW(parent, name.c_str(), &m_handle);
        }
        if (result != ERROR_SUCCESS) {
            throw std::runtime_error("RegOpenKeyExW/RegCreateKeyW failed");
        }
    }

    RegistryKey(const RegistryKey& parent, const std::wstring& name)
        : RegistryKey(parent.m_handle, name) {}

    RegistryKey(const RegistryKey&) = delete;
    RegistryKey& operator=(const RegistryKey&) = delete;

    ~RegistryKey() { RegCloseKey(m_handle); }

    void setValue(const wchar_t* subKey, const std::wstring& value)
    {
        const auto bytes = static_cast<DWORD>((value.size() + 1) * sizeof(wchar_t));
        if (RegSetValueExW(m_handle, subKey, 0, REG_SZ,
                           reinterpret_cast<const BYTE*>(value.c_str()), bytes) != ERROR_SUCCESS) {
            throw std::runtime_error("RegSetValueExW failed");
        }
    }

    HKEY handle() const { return m_handle; }

private:
    HKEY m_handle {};
};
}

namespace au::au3cloud {
bool registerCustomScheme(const QString& scheme)
{
    constexpr size_t kBufferSize = MAX_PATH + 1;
    wchar_t filenameBuffer[kBufferSize] = { 0 };
    const DWORD len = GetModuleFileNameW(nullptr, filenameBuffer, kBufferSize);
    if (len == 0 || len == kBufferSize) {
        LOGE() << "registerCustomScheme: failed to resolve executable path";
        return false;
    }

    const std::wstring schemeW = scheme.toStdWString();
    const std::wstring filename(filenameBuffer);
    const std::wstring progId = schemeW + L".Url.Audacity.4";

    try {
        RegistryKey hkcu;

        //! Cleanup: early AU4 alpha installers wrote a UrlAssociations
        //! entry under "Software\Audacity 4 \Capabilities" (trailing space from an
        //! empty release-channel) that would otherwise prevent the scheme from
        //! being re-claimed
        HKEY capabilities = nullptr;
        if (RegOpenKeyExW(hkcu.handle(), L"Software\\Audacity 4 \\Capabilities",
                          0, KEY_ALL_ACCESS, &capabilities) == ERROR_SUCCESS) {
            RegDeleteKeyW(capabilities, L"UrlAssociations");
            RegDeleteKeyW(capabilities, L"URLAssociations");
            RegCloseKey(capabilities);
        }

        RegistryKey root(hkcu, L"Software\\Classes");

        RegistryKey schemeKey(root, schemeW);
        schemeKey.setValue(L"", L"URL:" + schemeW);
        schemeKey.setValue(L"URL Protocol", L"");

        RegistryKey curVer(schemeKey, L"CurVer");
        curVer.setValue(L"", progId);

        RegistryKey handler(root, progId);

        RegistryKey icon(handler, L"DefaultIcon");
        icon.setValue(L"", filename + L",1");

        RegistryKey shell(handler, L"shell");
        RegistryKey open(shell, L"open");
        RegistryKey command(open, L"command");
        command.setValue(L"", L"\"" + filename + L"\" -u \"%1\"");
    } catch (const std::exception& e) {
        LOGE() << "registerCustomScheme failed: " << e.what();
        return false;
    }

    return true;
}
}
