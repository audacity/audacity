/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SchemeRegistrar.cpp

  Dmitry Vedenko

**********************************************************************/

#include <string_view>
#include <functional>

#include <windows.h>

#include <wx/log.h>
#include <exception>

#include "CodeConversions.h"
#include "URLSchemesRegistry.h"

/*
*
* On Windows, the following registry structure is expected by the OS:
* HKEY_CLASSES_ROOT
  %schema%
    (Default) = "URL:%schema% Protocol"
    URL Protocol = ""
    DefaultIcon
      (Default) = "%audacity.exe%,1"
    shell
      open
        command
          (Default) = "%audacity.exe" -u "%1"

   We will update the values every time to ensure popper operation
   for users with multiple Audacity installations
*/

class RegistryKey final
{
public:
   RegistryKey()
   {
      auto result = RegOpenCurrentUser(KEY_ALL_ACCESS, &mKeyHandle);

      if (result != ERROR_SUCCESS)
      {
         wxLogDebug(
            "Failed to create registry key: %s", wxSysErrorMsgStr(result));
         throw std::exception("Failed to create registry key");
      }
   }

   RegistryKey(HKEY parent, const std::wstring& name)
   {
      auto result =
         RegOpenKeyExW(parent, name.c_str(), 0, KEY_ALL_ACCESS, &mKeyHandle);

      if (result == ERROR_FILE_NOT_FOUND)
         result = RegCreateKeyW(parent, name.c_str(), &mKeyHandle);

      if (result != ERROR_SUCCESS)
      {
         wxLogDebug("Failed to open registry key: %s", wxSysErrorMsgStr(result));
         throw std::exception("Failed to open registry key");
      }
   }

   RegistryKey(const RegistryKey& parent, const std::wstring& name)
       : RegistryKey(parent.mKeyHandle, name)
   {
   }

   ~RegistryKey()
   {
      RegCloseKey(mKeyHandle);
   }

   RegistryKey(const RegistryKey&) = delete;
   RegistryKey& operator = (const RegistryKey&) = delete;

   void SetValue(const wchar_t* subKey, const std::wstring& value)
   {
      auto result = RegSetValueExW(
         mKeyHandle, subKey, 0, REG_SZ,
         reinterpret_cast<const BYTE*>(value.c_str()),
         (value.size() + 1) * sizeof(wchar_t));

      if (result != ERROR_SUCCESS)
      {
         wxLogDebug(
            "Failed to set value to a registry key: %s", wxSysErrorMsgStr(result));
         throw std::exception("Failed to set value to a registry key");
      }
   }

   HKEY GetHandle() const
   {
      return mKeyHandle;
   }

private:
   HKEY mKeyHandle {};
};

void SetSchemaRegistrar(std::function<bool(std::string_view)> registrar);

auto registrar = ([]() {
   URLSchemesRegistry::Get().SetRegistrar([](std::string_view schemaView) {
      constexpr size_t fileNameBufferSize = MAX_PATH + 1;
      wchar_t filenameBuffer[fileNameBufferSize] = { 0 };

      const size_t fileNameLength =
         GetModuleFileNameW(nullptr, filenameBuffer, fileNameBufferSize);

      // According to MSND - fileNameBufferSize is only returned on buffer overflow
      if (fileNameLength == fileNameBufferSize)
         return false;

      const std::wstring schema = audacity::ToWString(schemaView);

      try
      {
         RegistryKey hkcu;

         //! Bug fix: Clean up conflicting registry entries from Audacity 4 alphas
         try
         {
            RegistryKey capabilitiesKey(hkcu, L"Software\\Audacity 4 \\Capabilities");
            RegDeleteKeyW(capabilitiesKey.GetHandle(), L"URLAssociations");
         }
         catch (...) {  }

         RegistryKey rootKey(hkcu, L"Software\\Classes");

         RegistryKey schemaKey(rootKey, schema);
         schemaKey.SetValue(L"", L"URL:" + schema);
         schemaKey.SetValue(L"URL Protocol", L"");

         RegistryKey curVerKey(schemaKey, L"CurVer");
         curVerKey.SetValue(L"", schema + L".Url.Audacity.3");

         RegistryKey handlerKey(rootKey, schema + L".Url.Audacity.3");

         const std::wstring filename(filenameBuffer);

         RegistryKey iconKey(handlerKey, L"DefaultIcon");
         iconKey.SetValue(L"", filename + std::wstring(L",1"));

         RegistryKey shellKey(handlerKey, L"shell");
         RegistryKey openKey(shellKey, L"open");
         RegistryKey commandKey(openKey, L"command");

         commandKey.SetValue(L"", L"\"" + filename + L"\" -u \"%1\"");
      }
      catch (...)
      {
         return false;
      }

      return true;
   });

   return true;
})();
