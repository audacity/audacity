/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "framework/global/modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iprojecthistory.h"

#include "au3wrap/au3types.h"

#include "automation/iclipgaininteraction.h"

namespace au::automation {
class Au3ClipGainInteraction : public IClipGainInteraction, public muse::Contextable
{
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<au::trackedit::IProjectHistory> projectHistory{ this };

public:
    Au3ClipGainInteraction(const muse::modularity::ContextPtr& ctx);

    std::optional<AutomationInfo> clipGainInfo(const trackedit::ClipKey& key) const override;
    AutomationPoints clipGainPoints(const trackedit::ClipKey& key) const override;
    bool setClipGainPoint(const trackedit::ClipKey& key, double tAbs, double value, bool completed) override;
    bool removeClipGainPoint(const trackedit::ClipKey& key, int index, bool completed) override;
    bool setClipGainPointAtIndex(const trackedit::ClipKey& key, int index, double tAbs, double value, bool completed) override;
    bool beginClipGainPointDrag(const trackedit::ClipKey& clip, int pointIndex) override;
    bool updateClipGainPointDrag(const trackedit::ClipKey& clip, double tAbs, double value) override;
    bool endClipGainPointDrag(const trackedit::ClipKey& clip, bool commit) override;
    muse::async::Channel<trackedit::ClipKey, bool> clipGainChanged() const override;

private:
    au3::Au3Project& projectRef() const;

    muse::async::Channel<au::trackedit::ClipKey, bool> m_clipEnvelopeChanged;
    std::optional<AutomationDragSession> m_envDrag;
};
}
