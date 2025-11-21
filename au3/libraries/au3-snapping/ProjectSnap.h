/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectSnap.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include "ClientData.h"
#include "Observer.h"
#include "SnapUtils.h"

class AudacityProject;

struct SnapChangedMessage final
{
    SnapMode newSnapMode { SnapMode::SNAP_OFF };
    Identifier newSnapTo { "seconds" };
};

//! Project snapping settings
class SNAPPING_API ProjectSnap final : public ClientData::Base, public Observer::Publisher<SnapChangedMessage>
{
public:
    static ProjectSnap& Get(AudacityProject& project);
    static const ProjectSnap& Get(const AudacityProject& project);

    explicit ProjectSnap(const AudacityProject& project);
    ProjectSnap(const ProjectSnap&) = delete;
    ProjectSnap& operator=(const ProjectSnap&) = delete;

    void SetSnapMode(SnapMode mode);
    SnapMode GetSnapMode() const;

    void SetSnapTo(Identifier snap);
    Identifier GetSnapTo() const;

    SnapResult SnapTime(double time) const;
    SnapResult SingleStep(double time, bool upwards) const;

private:
    const AudacityProject& mProject;

    SnapMode mSnapMode { ReadSnapMode() };
    Identifier mSnapTo { ReadSnapTo() };
};
