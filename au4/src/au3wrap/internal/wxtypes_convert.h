#ifndef AU_AU3WRAP_WXTYPES_CONVERT_H
#define AU_AU3WRAP_WXTYPES_CONVERT_H

#include <string>
#include "global/types/string.h"
#include "wx/string.h"

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
}

#endif // AU_AU3WRAP_WXTYPES_CONVERT_H
