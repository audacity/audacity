/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file CodeConversions.cpp
 @brief Define functions to perform UTF-8 to std::wstring conversions.

 Dmitry Vedenko
 **********************************************************************/

#include "CodeConversions.h"

#include <locale>
#include <codecvt>

#if QT_CORE_LIB
#  if wxUSE_UNICODE_UTF8
#     error UTF8 wxString/QString conversion is not implemented
#  endif
#endif

namespace audacity
{

std::string ToUTF8 (const std::wstring& wstr)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>> ().to_bytes (wstr);
}

std::string ToUTF8 (const wchar_t* wstr)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>> ().to_bytes (wstr);
}

std::string ToUTF8 (const wxString& wstr)
{
    return wstr.ToStdString (wxGet_wxConvUTF8 ());
}

std::wstring ToWString (const std::string& str)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>> ().from_bytes (str);
}

STRING_UTILS_API std::wstring ToWString(std::string_view str)
{
   return std::wstring_convert<std::codecvt_utf8<wchar_t>>().from_bytes(
      str.data(), str.data() + str.length());
}

std::wstring ToWString (const char* str)
{
    return std::wstring_convert<std::codecvt_utf8<wchar_t>> ().from_bytes (str);
}

std::wstring ToWString (const wxString& str)
{
    return str.ToStdWstring ();
}

wxString ToWXString (const std::string& str)
{
    return wxString::FromUTF8 (str);
}

STRING_UTILS_API wxString ToWXString(std::string_view str)
{
   return wxString::FromUTF8(str.data(), str.length());
}

STRING_UTILS_API wxString ToWXString(const char* str)
{
   return wxString::FromUTF8(str);
}

wxString ToWXString (const std::wstring& str)
{
   return { str };
}

#if QT_CORE_LIB

std::string ToUTF8 (const QString& str)
{
   return str.toStdString();
}

std::wstring ToWString (const QString& str)
{
   return str.toStdWString();
}
wxString ToWXString (const QString& str)
{
   if constexpr (sizeof(wchar_t) == sizeof(QChar))
      return { reinterpret_cast<const wchar_t*>(str.constData()), static_cast<size_t>(str.size()) };
   else
      return { str.toStdWString() };
}

QString ToQString (const char* str)
{
   return QString::fromUtf8(str, -1);
}

QString ToQString (const std::string& str)
{
   return QString::fromStdString(str);
}

QString ToQString (std::string_view str)
{
   return QString::fromUtf8(str.data(), static_cast<qsizetype>(str.length()));
}

QString ToQString (const std::wstring& str)
{
   return QString::fromStdWString(str);
}

QString ToQString (const wxString& str)
{
   return QString::fromWCharArray(str.wx_str(), static_cast<qsizetype>(str.length()));
}

#endif

wxString ToWXString(std::wstring_view str)
{
    return wxString(str.data(), str.size());
}

wxString ToWXString(const wchar_t* str)
{
    return wxString(str);
}

}
