/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TempoChange.cpp

  PaulLicameli

**********************************************************************/
#include "TempoChange.h"
#include "Channel.h"

namespace {
struct ProjectTempo : ClientData::Cloneable<> {
    ~ProjectTempo() = default;
    std::unique_ptr<ClientData::Cloneable<> > Clone() const override
    { return std::make_unique<ProjectTempo>(*this); }

    static ProjectTempo& Get(ChannelGroup& group);
    static const ProjectTempo& Get(const ChannelGroup& group);

    std::optional<double> mProjectTempo;
};

const ChannelGroup::Attachments::RegisteredFactory
    projectTempoDataFactory{
    [](auto&) { return std::make_unique<ProjectTempo>(); } };

ProjectTempo& ProjectTempo::Get(ChannelGroup& group)
{
    return group.Attachments::Get<ProjectTempo>(projectTempoDataFactory);
}

const ProjectTempo& ProjectTempo::Get(const ChannelGroup& group)
{
    return Get(const_cast<ChannelGroup&>(group));
}
}

void DoProjectTempoChange(ChannelGroup& group, double newTempo)
{
    auto& oldTempo = ProjectTempo::Get(group).mProjectTempo;
    OnProjectTempoChange::Call(group, oldTempo, newTempo);
    oldTempo = newTempo;
}

const std::optional<double>& GetProjectTempo(const ChannelGroup& group)
{
    return ProjectTempo::Get(group).mProjectTempo;
}

DEFINE_ATTACHED_VIRTUAL(OnProjectTempoChange) {
    return [](auto&, auto&, auto){ };
}
