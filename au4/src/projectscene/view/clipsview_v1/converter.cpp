#include "converter.h"

namespace audacity {
std::string ToUTF8(const QString& str)
{
    return str.toStdString();
}

std::wstring ToWString(const QString& str)
{
    return str.toStdWString();
}

wxString ToWXString(const QString& str)
{
    if constexpr (sizeof(wchar_t) == sizeof(QChar)) {
        return { reinterpret_cast<const wchar_t*>(str.constData()), static_cast<size_t>(str.size()) };
    } else {
        return { str.toStdWString() };
    }
}

QString ToQString(const char* str)
{
    return QString::fromUtf8(str, -1);
}

QString ToQString(const std::string& str)
{
    return QString::fromStdString(str);
}

QString ToQString(std::string_view str)
{
    return QString::fromUtf8(str.data(), static_cast<qsizetype>(str.length()));
}

QString ToQString(const std::wstring& str)
{
    return QString::fromStdWString(str);
}

QString ToQString(const wxString& str)
{
    return QString::fromWCharArray(str.wx_str(), static_cast<qsizetype>(str.length()));
}
}
