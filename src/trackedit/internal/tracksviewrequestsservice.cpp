/*
 * Audacity: A Digital Audio Editor
 */
#include "tracksviewrequestsservice.h"

using namespace au::trackedit;

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
