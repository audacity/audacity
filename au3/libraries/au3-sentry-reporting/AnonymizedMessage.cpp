/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file AnonymizedMessage.cpp
 @brief Define a class to store anonymized messages.

 Dmitry Vedenko
 **********************************************************************/

#include "AnonymizedMessage.h"

#include <regex>

#include "CodeConversions.h"

namespace audacity {
namespace sentry {
AnonymizedMessage::AnonymizedMessage(std::string message)
    : mMessage(std::move(message))
{
    CleanupPaths();
}

AnonymizedMessage::AnonymizedMessage(const std::wstring& message)
    : AnonymizedMessage(ToUTF8(message))
{
}

AnonymizedMessage::AnonymizedMessage(const wxString& message)
    : AnonymizedMessage(ToUTF8(message))
{
}

AnonymizedMessage::AnonymizedMessage(const char* message)
    : AnonymizedMessage(std::string(message))
{
}

AnonymizedMessage::AnonymizedMessage(const wchar_t* message)
    : AnonymizedMessage(ToUTF8(message))
{
}

bool AnonymizedMessage::Empty() const noexcept
{
    return mMessage.empty();
}

size_t AnonymizedMessage::Length() const noexcept
{
    return mMessage.size();
}

const std::string& AnonymizedMessage::GetString() const noexcept
{
    return mMessage;
}

wxString AnonymizedMessage::ToWXString() const noexcept
{
    return audacity::ToWXString(mMessage);
}

const char* AnonymizedMessage::c_str() const noexcept
{
    return mMessage.c_str();
}

size_t AnonymizedMessage::length() const noexcept
{
    return mMessage.length();
}

void AnonymizedMessage::CleanupPaths()
{
    // Finding the path boundary in the arbitrary text is a hard task.
    // We assume that spaces cannot be a part of the path.
    // In the worst case - we will get <path> <path>
    static const std::regex re(
        R"(\b(?:(?:[a-zA-Z]:)?[\\/]?)?(?:[^<>:"/|\\/?\s*]+[\\/]+)*(?:[^<>:"/|\\/?*\s]+\.\w+)?)");

    mMessage = std::regex_replace(
        mMessage, re, "<path>", std::regex_constants::match_not_null);
}
} // namespace sentry
} // namespace audacity
