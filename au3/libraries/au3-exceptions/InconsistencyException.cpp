/*!
  @file InconsistencyException.cpp
  @brief Implements InconsistencyException


  Created by Paul Licameli on 11/27/16.

*/

#include "InconsistencyException.h"
#include <wx/filename.h>

InconsistencyException::~InconsistencyException()
{
}

TranslatableString InconsistencyException::ErrorMessage() const
{
    // Shorten the path
    wxString path { file };
    auto sub = wxString{ wxFILE_SEP_PATH } + "src" + wxFILE_SEP_PATH;
    auto index = path.Find(sub);
    if (index != wxNOT_FOUND) {
        path = path.Mid(index + sub.size());
    }

#ifdef __func__
    return
        //: %1 is the function name, %2 is the source file path, %3 is the line number in that file
        TranslatableString("exceptions", "Internal error in %1 at %2 line %3.\nPlease inform the Audacity team at https://forum.audacityteam.org/.")
        .Format(func, path, line);
#else
    return
        //: %1 is the source file path, %2 is the line number in that file
        TranslatableString("exceptions", "Internal error at %1 line %2.\nPlease inform the Audacity team at https://forum.audacityteam.org/.")
        .Format(path, line);
#endif
}
