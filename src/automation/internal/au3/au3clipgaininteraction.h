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
class Au3ClipGainInteraction : public IClipGainInteraction, public muse::Injectable
{
    muse::Inject<au::context::IGlobalContext> globalContext{ this };
    muse::Inject<au::trackedit::IProjectHistory> projectHistory{ this };

public:
    Au3ClipGainInteraction(const muse::modularity::ContextPtr& ctx);

    std::optional<ClipEnvelopeInfo> clipEnvelopeInfo(const trackedit::ClipKey& key) const override;
    ClipEnvelopePoints clipEnvelopePoints(const trackedit::ClipKey& key) const override;
    bool setClipEnvelopePoint(const trackedit::ClipKey& key, double tAbs, double value, bool completed) override;
    bool removeClipEnvelopePoint(const trackedit::ClipKey& key, int index, bool completed) override;
    bool flattenClipEnvelope(const trackedit::ClipKey& key, double value, bool completed) override;
    bool setClipEnvelopePointAtIndex(const trackedit::ClipKey& key, int index, double tAbs, double value, bool completed) override;
    bool beginClipEnvelopePointDrag(const trackedit::ClipKey& clip, int pointIndex) override;
    bool updateClipEnvelopePointDrag(const trackedit::ClipKey& clip, double tAbs, double value) override;
    bool endClipEnvelopePointDrag(const trackedit::ClipKey& clip, bool commit) override;
    muse::async::Channel<trackedit::ClipKey, bool> clipEnvelopeChanged() const override;

private:
    au3::Au3Project& projectRef() const;

    muse::async::Channel<au::trackedit::ClipKey, bool> m_clipEnvelopeChanged;
    std::optional<EnvelopeDragSession> m_envDrag;
};
}
