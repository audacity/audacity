/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudacityMirProject.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirProjectInterface.h"

class AudacityProject;

class AudacityMirProject final : public MIR::ProjectInterface
{
public:
    explicit AudacityMirProject(AudacityProject& project);
    ~AudacityMirProject() override;

    bool ViewIsBeatsAndMeasures() const override;

    void ReconfigureMusicGrid(
        double newTempo, std::optional<MIR::TimeSignature> timeSignature) override;

    double GetTempo() const override;

    bool ShouldBeReconfigured(double newTempo, bool isSingleFileImport) override;

    void OnClipsSynchronized() override;

private:
    enum class ModificationType
    {
        Automatic,
        Manual,
    };

    AudacityProject& mProject;
    const double mProjectTempo;
    const bool mImportedOnEmptyProject;
    std::optional<ModificationType> mMostSignificantModification;
    bool mProjectWasModified = false;
};
