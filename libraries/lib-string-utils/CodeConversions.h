/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CodeConversions.h
 @brief Declare functions to preform UTF-8 to std::wstring conversions.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <wx/string.h>

namespace audacity
{

STRING_UTILS_API std::string ToUTF8 (const std::wstring& wstr);
STRING_UTILS_API std::string ToUTF8 (const wchar_t* wstr);
STRING_UTILS_API std::string ToUTF8 (const wxString& wstr);

// std::wstring is UTF16 on windows and UTF32 elsewhere.
STRING_UTILS_API std::wstring ToWString (const std::string& str);
STRING_UTILS_API std::wstring ToWString (const char* str);
STRING_UTILS_API std::wstring ToWString (const wxString& str);

STRING_UTILS_API wxString ToWXString (const std::string& str);
STRING_UTILS_API wxString ToWXString (const std::wstring& str);

}
