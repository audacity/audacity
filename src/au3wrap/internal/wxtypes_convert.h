#ifndef AU_AU3WRAP_WXTYPES_CONVERT_H
#define AU_AU3WRAP_WXTYPES_CONVERT_H

#include <string>
#include "framework/global/types/string.h"
#include "wx/string.h"

#ifndef NO_QT_SUPPORT
#include <QString>
#endif

namespace au::au3 {
inline std::string wxToStdSting(const wxString& s)
{
    return s.ToStdString(wxConvUTF8);
}

inline muse::String wxToString(const wxString& s)
{
    return muse::String::fromStdString(wxToStdSting(s));
}

inline wxString wxFromString(const muse::String& s)
{
    return wxString(s.toStdWString());
}

inline wxString wxFromStdString(const std::string& s)
{
    return wxString(s);
}

#ifndef NO_QT_SUPPORT
inline QString wxToQString(const wxString& s)
{
    return QString::fromStdWString(s.ToStdWstring());
}

#endif
}

#endif // AU_AU3WRAP_WXTYPES_CONVERT_H
