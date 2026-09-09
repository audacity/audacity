/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "framework/global/modularity/ioc.h"
#include "framework/global/async/asyncable.h"

#include "../itracksviewrequestsservice.h"

namespace au::trackedit {
class TracksViewRequestsService : public ITracksViewRequestsService, public muse::async::Asyncable, public muse::Contextable
{
public:
    TracksViewRequestsService(const muse::modularity::ContextPtr& ctx)
        : muse::Contextable(ctx) {}

    void requestItemMove(secs_t timeOffset, int trackOffset) override;
    muse::async::Channel<secs_t, int> itemMoveRequested() const override;

    void requestLabelTitleEdit(const LabelKey& labelKey) override;
    std::optional<LabelKey> pendingLabelTitleEdit() const override;
    void labelTitleEditRequestHandled(const LabelKey& labelKey) override;
    muse::async::Channel<LabelKey> labelTitleEditRequested() const override;

private:
    muse::async::Channel<secs_t, int> m_itemMoveRequested;
    std::optional<LabelKey> m_pendingLabelTitleEdit;
    muse::async::Channel<LabelKey> m_labelTitleEditRequested;
};
}
