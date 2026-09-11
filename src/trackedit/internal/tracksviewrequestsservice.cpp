/*
 * Audacity: A Digital Audio Editor
 */
#include "tracksviewrequestsservice.h"

using namespace au::trackedit;

void TracksViewRequestsService::requestItemMove(secs_t timeOffset, int trackOffset)
{
    m_itemMoveRequested.send(timeOffset, trackOffset);
}

muse::async::Channel<secs_t, int> TracksViewRequestsService::itemMoveRequested() const
{
    return m_itemMoveRequested;
}

void TracksViewRequestsService::requestLabelTitleEdit(const LabelKey& labelKey)
{
    m_pendingLabelTitleEdit = labelKey;
    m_labelTitleEditRequested.send(labelKey);
}

std::optional<LabelKey> TracksViewRequestsService::pendingLabelTitleEdit() const
{
    return m_pendingLabelTitleEdit;
}

void TracksViewRequestsService::labelTitleEditRequestHandled(const LabelKey& labelKey)
{
    if (m_pendingLabelTitleEdit == labelKey) {
        m_pendingLabelTitleEdit.reset();
    }
}

muse::async::Channel<LabelKey> TracksViewRequestsService::labelTitleEditRequested() const
{
    return m_labelTitleEditRequested;
}
