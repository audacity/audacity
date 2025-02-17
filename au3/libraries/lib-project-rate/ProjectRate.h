/*!********************************************************************

Audacity: A Digital Audio Editor

@file ProjectRate.h
@brief an object holding per-project preferred sample rate

Paul Licameli split from ProjectSettings.h

**********************************************************************/

#ifndef __AUDACITY_PROJECT_RATE__
#define __AUDACITY_PROJECT_RATE__

class AudacityProject;

#include "ClientData.h"
#include "Observer.h"

///\brief Holds project sample rate
class PROJECT_RATE_API ProjectRate final : public ClientData::Base, public Observer::Publisher<double>
{
public:
    static ProjectRate& Get(AudacityProject& project);
    static const ProjectRate& Get(const AudacityProject& project);

    explicit ProjectRate(AudacityProject& project);
    ProjectRate(const ProjectRate&) = delete;
    ProjectRate& operator=(const ProjectRate&) = delete;

    void SetRate(double rate);
    double GetRate() const;

private:
    double mRate;
};

#endif
