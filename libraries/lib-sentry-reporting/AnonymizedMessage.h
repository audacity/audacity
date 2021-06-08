/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file AnonymizedMessage.h
 @brief Declare a class to store anonymized messages.

 Dmitry Vedenko
 **********************************************************************/

#include <string>
#include <wx/string.h>

#pragma once

namespace audacity
{
namespace sentry
{

class SENTRY_REPORTING_API AnonymizedMessage final
{
public:
   AnonymizedMessage() = default;

   AnonymizedMessage(const AnonymizedMessage&) = default;
   AnonymizedMessage(AnonymizedMessage&&) = default;

   AnonymizedMessage& operator=(const AnonymizedMessage&) = default;
   AnonymizedMessage& operator=(AnonymizedMessage&&) = default;

   AnonymizedMessage(std::string message);
   AnonymizedMessage(const std::wstring& message);
   AnonymizedMessage(const wxString& message);

   AnonymizedMessage(const char* message);
   AnonymizedMessage(const wchar_t* message);

   bool Empty() const noexcept;
   size_t Length() const noexcept;

   const std::string& GetString() const noexcept;
   wxString ToWXString() const noexcept;

   // Immitate std::string interface
   const char* c_str() const noexcept;
   size_t length() const noexcept;
private:
   void CleanupPaths();

   std::string mMessage;
};

} // namespace sentry
} // namespace audacity
