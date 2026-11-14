/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectTimeRuler.h

  Dmitry Vedenko

**********************************************************************/
#include "ProjectTimeRuler.h"

#include <cassert>

#include "Observer.h"
#include "Project.h"
#include "ProjectTimeSignature.h"
#include "TimeDisplayMode.h"

#include "widgets/BeatsFormat.h"
#include "widgets/LinearUpdater.h"
#include "widgets/Ruler.h"
#include "widgets/TimeFormat.h"

static const AttachedProjectObjects::RegisteredFactory key {
    [](AudacityProject& project) { return std::make_shared<ProjectTimeRuler>(project); }
};

ProjectTimeRuler& ProjectTimeRuler::Get(AudacityProject& project)
{
    return project.AttachedObjects::Get<ProjectTimeRuler&>(key);
}

const ProjectTimeRuler& ProjectTimeRuler::Get(const AudacityProject& project)
{
    return Get(const_cast<AudacityProject&>(project));
}

struct ProjectTimeRuler::Impl final
{
    explicit Impl(AudacityProject& project)
        : beatsFormat{ProjectTimeSignature::Get(project)}
        , projectTimeSignatureChanged{ProjectTimeSignature::Get(project)
                                      .Subscribe([this](const auto& msg) {
            beatsFormat.SetData(
                msg.newTempo,
                msg.newUpperTimeSignature,
                msg.newLowerTimeSignature);

            ruler.Invalidate();
        })},
        timeDisplayModeChanged { TimeDisplayModePreference.Subscribe(
                                     [this](TimeDisplayMode newMode) {
            switch (newMode) {
                case TimeDisplayMode::BeatsAndMeasures:
                    ruler.SetFormat(&beatsFormat);
                    break;
                case TimeDisplayMode::MinutesAndSeconds:
                    ruler.SetFormat(&TimeFormat::Instance());
                    break;
                default:
                    assert(false);
                    break;
            }
        }) }
    {
        if (mode == TimeDisplayMode::BeatsAndMeasures) {
            ruler.SetFormat(&beatsFormat);
        }
    }

    TimeDisplayMode mode { TimeDisplayModePreference.ReadEnum() };

    BeatsFormat beatsFormat;
    LinearUpdater updater;

    Ruler ruler { updater, TimeFormat::Instance() };

private:
    Observer::Subscription projectTimeSignatureChanged;
    Observer::Subscription timeDisplayModeChanged;
};

ProjectTimeRuler::ProjectTimeRuler(AudacityProject& project)
    : mImpl{std::make_unique<Impl>(project)}
{
}

LinearUpdater& ProjectTimeRuler::GetUpdater()
{
    return mImpl->updater;
}

const LinearUpdater& ProjectTimeRuler::GetUpdater() const
{
    return mImpl->updater;
}

BeatsFormat& ProjectTimeRuler::GetBeatsFormat()
{
    return mImpl->beatsFormat;
}

const BeatsFormat& ProjectTimeRuler::GetBeatsFormat() const
{
    return mImpl->beatsFormat;
}

Ruler& ProjectTimeRuler::GetRuler()
{
    return mImpl->ruler;
}

const Ruler& ProjectTimeRuler::GetRuler() const
{
    return mImpl->ruler;
}
