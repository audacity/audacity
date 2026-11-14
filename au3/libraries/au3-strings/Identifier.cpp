/**********************************************************************

 Audacity: A Digital Audio Editor

 Identifier.cpp

 Paul Licameli split from Internat.cpp

 **********************************************************************/

#include "Identifier.h"
#include <wx/arrstr.h> // for wxSplit

Identifier::Identifier(
    std::initializer_list<Identifier> components, wxChar separator)
{
    if (components.size() < 2) {
        wxASSERT(false);
        return;
    }
    auto iter = components.begin(), end = components.end();
    value = (*iter++).value;
    while (iter != end) {
        value += separator + (*iter++).value;
    }
}

std::vector< Identifier > Identifier::split(wxChar separator) const
{
    auto strings = ::wxSplit(value, separator);
    return { strings.begin(), strings.end() };
}
