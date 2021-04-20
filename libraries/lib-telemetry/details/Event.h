#pragma once

#include <string>
#include <unordered_map>

#include <wx/string.h>

namespace audacity
{
namespace telemetry
{
namespace details
{
using CustomParams = std::unordered_map<std::string, std::string>;

enum class EventType
{
    ScreenView,
    Exception,
    FatalException,
    Event
};

struct Event final
{
    EventType Type { EventType::Event };

    std::string Value;

    std::string Category;
    std::string Action;

    CustomParams Params;

    size_t SumbmissionIndex { 0 };
    bool Submitted { false };

    wxString serialize () const;
    bool deserialize (const wxString& data);
};

}
}
}