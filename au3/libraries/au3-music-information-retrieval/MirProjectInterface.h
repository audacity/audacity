/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirProjectInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirTypes.h"
#include <optional>

namespace MIR {
class ProjectInterface
{
public:
    virtual ~ProjectInterface() = default;
    virtual bool ViewIsBeatsAndMeasures() const = 0;
    virtual void ReconfigureMusicGrid(
        double newTempo, std::optional<MIR::TimeSignature> timeSignature) = 0;
    virtual double GetTempo() const = 0;
    virtual bool
    ShouldBeReconfigured(double newTempo, bool isSingleFileImport) = 0;
    virtual void OnClipsSynchronized() = 0;
};
} // namespace MIR
