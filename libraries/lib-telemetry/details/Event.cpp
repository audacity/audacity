#include "Event.h"

#include <wx/tokenzr.h>

#include <array>

#include "lib-string-utils/UrlEncode.h"

namespace audacity
{
namespace telemetry
{
namespace details
{

const std::array<wxString, 4> EventTypeNames = {
    "ScreenView",
    "Exception",
    "FatalException",
    "Event",
};


wxString Event::serialize () const
{
    wxString line =
        EventTypeNames[size_t (Type)] + ";" +
        wxString::FromUTF8 (UrlEncode (Value)) + ";" +
        wxString::FromUTF8 (UrlEncode (Category)) + ";" +
        wxString::FromUTF8 (UrlEncode (Action));

    for (const auto& param : Params)
    {
        line +=
            ";" + wxString::FromUTF8 (UrlEncode (param.first)) +
            "=" + wxString::FromUTF8 (UrlEncode (param.second));
    }

    return line;
}

bool Event::deserialize (const wxString& data)
{
    wxStringTokenizer tokenizer (data, ";");

    if (!tokenizer.HasMoreTokens ())
        return false;

    const wxString typeString = tokenizer.GetNextToken ();

    const auto typeIndex = std::find (EventTypeNames.begin (), EventTypeNames.end (), typeString);

    if (typeIndex == EventTypeNames.end ())
        return false;

    Type = EventType (std::distance (EventTypeNames.begin (), typeIndex));

    if (!tokenizer.HasMoreTokens ())
        return false;

    Value = tokenizer.GetNextToken ().ToUTF8 ();

    if (!tokenizer.HasMoreTokens ())
        return false;

    Category = tokenizer.GetNextToken ().ToUTF8 ();

    if (!tokenizer.HasMoreTokens ())
        return false;

    Action = tokenizer.GetNextToken ().ToUTF8 ();

    while (tokenizer.HasMoreTokens ())
    {
        const wxString customParam = tokenizer.GetNextToken ();

        const size_t equalsPosition = customParam.find ('=');

        if (equalsPosition == wxString::npos)
            return false;

        Params.insert (std::make_pair (
            customParam.substr (0, equalsPosition).ToUTF8 (),
            customParam.substr (equalsPosition + 1).ToUTF8 ()
        ));
    }

    return true;
}

}
}
}