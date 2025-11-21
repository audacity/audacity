/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file SentryReport.h
 @brief Declare a class to report errors to Sentry.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <string>
#include <utility>
#include <vector>
#include <memory>
#include <functional>

#include "AnonymizedMessage.h"

namespace audacity {
namespace sentry {
//! Additional payload to the exception
using ExceptionData = std::pair<std::string, AnonymizedMessage>;

//! A DTO for the Sentry Exception interface
struct SENTRY_REPORTING_API Exception final
{
    //! Exception type. Should not have spaces.
    std::string Type;
    //! Message, associated with the Exception
    AnonymizedMessage Value;
    //! Arbitrary payload
    std::vector<ExceptionData> Data;

    //! Create a new exception
    static Exception Create(std::string type, AnonymizedMessage value);
    //! Create a new exception with type runtime_error
    static Exception Create(AnonymizedMessage value);
    //! Add a payload to the exception
    Exception& AddData(std::string key, AnonymizedMessage value);
};

//! A DTO for the Sentry Message interface
struct SENTRY_REPORTING_API Message final
{
    //! A string, possibly with %s placeholders, containing the message
    AnonymizedMessage Value;
    //! Values for the placeholders
    std::vector<AnonymizedMessage> Params;
    //! Create a new Message
    static Message Create(AnonymizedMessage message);
    //! Add a parameter to the Message
    Message& AddParam(AnonymizedMessage value);
};

//! Saves a parameter, that will be appended to the next Exception report
SENTRY_REPORTING_API void AddExceptionContext(
    std::string parameterName, AnonymizedMessage parameterValue);

//! A report to Sentry
class SENTRY_REPORTING_API Report final
{
public:
    //! A callback, that will be called when Send completes
    using CompletionHandler = std::function<void (int httpCode, std::string responseBody)>;

    //! Create a report from the exception and previously added exception context
    explicit Report(const Exception& exception);

    //! Create a report with a single log message
    explicit Report(const Message& message);

    ~Report();

    //! Adds a user comment to the exception report
    void AddUserComment(const std::string& comment);

    //! Get a pretty printed report preview
    std::string GetReportPreview() const;

    //! Send the report to Sentry
    void Send(CompletionHandler completionHandler) const;

private:
    class ReportImpl;

    std::unique_ptr<ReportImpl> mImpl;
};
} // namespace sentry
} // namespace audacity
