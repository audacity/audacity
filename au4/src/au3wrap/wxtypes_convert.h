#ifndef AU_AU3WRAP_WXTYPES_CONVERT_H
#define AU_AU3WRAP_WXTYPES_CONVERT_H

#include <string>

#include "wx/string.h"

namespace au::au3 {
inline std::string wxToStdSting(const wxString& s)
{
    return std::string(s.mb_str());
}
}

#endif // AU_AU3WRAP_WXTYPES_CONVERT_H
