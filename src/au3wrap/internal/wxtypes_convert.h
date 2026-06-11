#ifndef AU_AU3WRAP_WXTYPES_CONVERT_H
#define AU_AU3WRAP_WXTYPES_CONVERT_H

#include <string>
#include "framework/global/types/string.h"
#include "wx/string.h"

#ifndef NO_QT_SUPPORT
#include <QString>
#endif

namespace au::au3 {
inline std::string wxToStdString(const wxString& s)
{
    return s.ToStdString(wxConvUTF8);
}

inline muse::String wxToString(const wxString& s)
{
    return muse::String::fromStdString(wxToStdString(s));
}

inline wxString wxFromString(const muse::String& s)
{
    return wxString::FromUTF8(s.toStdString());
}

inline wxString wxFromStdString(const std::string& s)
{
    return wxString::FromUTF8(s);
}
}

#endif // AU_AU3WRAP_WXTYPES_CONVERT_H
