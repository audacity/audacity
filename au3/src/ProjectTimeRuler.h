/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectTimeRuler.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <memory>

#include "ClientData.h"

class AudacityProject;
class LinearUpdater;
class BeatsFormat;
class Ruler;

class ProjectTimeRuler final : public ClientData::Base
{
public:
    explicit ProjectTimeRuler(AudacityProject& project);

    static ProjectTimeRuler& Get(AudacityProject& project);
    static const ProjectTimeRuler& Get(const AudacityProject& project);

    LinearUpdater& GetUpdater();
    const LinearUpdater& GetUpdater() const;

    BeatsFormat& GetBeatsFormat();
    const BeatsFormat& GetBeatsFormat() const;

    Ruler& GetRuler();
    const Ruler& GetRuler() const;

private:
    struct Impl;
    std::unique_ptr<Impl> mImpl;
};
