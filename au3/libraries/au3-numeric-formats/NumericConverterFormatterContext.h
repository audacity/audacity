/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatterContext.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include <memory>
#include <optional>

class AudacityProject;

//! A context in which formatter operates
/*!
 * This class is used to pass additional information to the formatter,
 * allowing AudacityProject to be an optional dependency
 */
class NUMERIC_FORMATS_API FormatterContext final
{
    explicit FormatterContext(const AudacityProject& project);
    explicit FormatterContext(double sampleRate);

public:
    static FormatterContext EmptyContext();
    static FormatterContext ProjectContext(const AudacityProject& project);
    static FormatterContext SampleRateContext(double sampleRate);

    FormatterContext() = default;
    FormatterContext(const FormatterContext&) = default;
    FormatterContext(FormatterContext&&) = default;
    FormatterContext& operator=(const FormatterContext&) = default;
    FormatterContext& operator=(FormatterContext&&) = default;

    ~FormatterContext();

    //! Returns true if the reference to the project is valid at this moment.
    /*!
     * This operation is not thread-safe.The project isn't locked.
     * Generally it is safe to assume that project outlives any of the
     * formatters, so `HasProject()` can be used as a hint when building the UI.
     */
    bool HasProject() const;
    //! Returns a potentially null pointer to the project
    std::shared_ptr<const AudacityProject> GetProject() const;
    //! Returns true if it is possible to get a sample rate from this context
    bool HasSampleRate() const;
    //! Returns a sample rate from this context
    double GetSampleRate(double defaultSampleRate = 44100.0) const;

private:
    std::weak_ptr<const AudacityProject> mProject;
    std::optional<double> mProjectRate;
};
