/*
 * Audacity: A Digital Audio Editor
 */
#include "tracksviewcontroller.h"

#include "global/containers.h"

#include "../itrackeditproject.h"

using namespace muse;
using namespace muse::actions;
using namespace au::trackedit;

static const ActionCode RENAME_ITEM_CODE("rename-item");

void TracksViewController::init()
{
    dispatcher()->reg(this, RENAME_ITEM_CODE, this, &TracksViewController::renameSelectedItem);
}

void TracksViewController::renameSelectedItem()
{
    LabelKeyList selected = selectionController()->selectedLabels();

    const TrackItemKey focused = trackNavigationController()->focusedItem();
    if (focused.isValid() && !muse::contains(selected, focused)) {
        const ITrackeditProjectPtr prj = globalContext()->currentTrackeditProject();
        if (prj) {
            const std::optional<Track> track = prj->track(focused.trackId);
            if (track.has_value() && track->type == TrackType::Label) {
                selected.insert(selected.cbegin(), focused);
            }
        }
    }

    if (selected.size() != 1 || !selected.front().isValid()) {
        return;
    }

    requestLabelTitleEdit(selected.front());
}

void TracksViewController::requestLabelTitleEdit(const LabelKey& labelKey)
{
    m_pendingLabelTitleEdit = labelKey;
    m_labelTitleEditRequested.send(labelKey);
}

std::optional<LabelKey> TracksViewController::pendingLabelTitleEdit() const
{
    return m_pendingLabelTitleEdit;
}

void TracksViewController::labelTitleEditRequestHandled(const LabelKey& labelKey)
{
    if (m_pendingLabelTitleEdit == labelKey) {
        m_pendingLabelTitleEdit.reset();
    }
}

muse::async::Channel<LabelKey> TracksViewController::labelTitleEditRequested() const
{
    return m_labelTitleEditRequested;
}
