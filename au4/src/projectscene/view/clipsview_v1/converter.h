#ifndef CONVERTER_H
#define CONVERTER_H

#include <string>
#include <string_view>
#include <wx/string.h>
#include <QString>

namespace audacity {
std::string ToUTF8(const QString& str);
std::wstring ToWString(const QString& str);
wxString ToWXString(const QString& str);

QString ToQString(const char* str);
QString ToQString(const std::string& str);
QString ToQString(std::string_view str);
QString ToQString(const std::wstring& str);
QString ToQString(const wxString& str);
}

#endif // CONVERTER_H
