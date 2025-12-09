/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file AnonymizedMessage.h
 @brief Declare a class to store anonymized messages.

 Dmitry Vedenko
 **********************************************************************/

#include <string>
#include <wx/string.h>

#pragma once

namespace audacity {
namespace sentry {
//! A class, that stores anonymized message.
/*!
    Input message is anonymized by looking for path-like patterns.
    Messages are stored in UTF8 format.
*/
class SENTRY_REPORTING_API AnonymizedMessage final
{
public:
    //! Creates an empty message
    AnonymizedMessage() = default;

    AnonymizedMessage(const AnonymizedMessage&) = default;
    AnonymizedMessage(AnonymizedMessage&&) = default;

    AnonymizedMessage& operator=(const AnonymizedMessage&) = default;
    AnonymizedMessage& operator=(AnonymizedMessage&&) = default;

    //! Creates a message from std::string
    AnonymizedMessage(std::string message);
    //! Creates a message from std::wstring
    AnonymizedMessage(const std::wstring& message);
    //! Creates a message from wxString
    AnonymizedMessage(const wxString& message);
    //! Creates a message from const char*
    AnonymizedMessage(const char* message);
    //! Creates a message from const wchar_t*
    AnonymizedMessage(const wchar_t* message);

    //! Checks, if the message is empty
    bool Empty() const noexcept;
    //! Returns the length of the message
    size_t Length() const noexcept;

    //! Returns the UTF8 representation of the message
    const std::string& GetString() const noexcept;
    //! Convert the message to wxString
    wxString ToWXString() const noexcept;

    // Imitate std::string interface
    //! Checks, if the message is empty
    const char* c_str() const noexcept;
    //! Returns the length of the message
    size_t length() const noexcept;
private:
    void CleanupPaths();

    std::string mMessage;
};
} // namespace sentry
} // namespace audacity
