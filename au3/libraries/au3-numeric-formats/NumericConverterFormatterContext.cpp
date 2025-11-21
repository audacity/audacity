/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file NumericConverterFormatterContext.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "NumericConverterFormatterContext.h"

#include "Project.h"
#include "ProjectRate.h"

FormatterContext::FormatterContext(const AudacityProject& project)
    : mProject(project.weak_from_this())
{
}

FormatterContext::FormatterContext(double sampleRate)
    : mProjectRate{sampleRate}
{
}

FormatterContext FormatterContext::EmptyContext()
{
    return {};
}

FormatterContext
FormatterContext::ProjectContext(const AudacityProject& project)
{
    return FormatterContext { project };
}

FormatterContext FormatterContext::SampleRateContext(double sampleRate)
{
    return FormatterContext { sampleRate };
}

FormatterContext::~FormatterContext()
{
}

bool FormatterContext::HasProject() const
{
    return !mProject.expired();
}

std::shared_ptr<const AudacityProject> FormatterContext::GetProject() const
{
    return mProject.lock();
}

bool FormatterContext::HasSampleRate() const
{
    return HasProject() || mProjectRate.has_value();
}

double FormatterContext::GetSampleRate(double defaultSampleRate) const
{
    auto project = GetProject();

    if (project) {
        return ProjectRate::Get(*project).GetRate();
    }

    if (mProjectRate.has_value()) {
        return *mProjectRate;
    }

    return defaultSampleRate;
}
