/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/types/string.h"
#include "global/progress.h"

#include "trackedittypes.h"
#include "itrackdata.h"

#include "modularity/imoduleinterface.h"

namespace au::trackedit {
class ILabelsInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsInteraction)

public:
    virtual ~ILabelsInteraction() = default;

    virtual bool addLabelToSelection() = 0;
    virtual bool changeLabelTitle(const LabelKey& labelKey, const muse::String& title) = 0;

    virtual bool removeLabel(const LabelKey& labelKey) = 0;
    virtual bool removeLabels(const LabelKeyList& labelKeys, bool moveLabels) = 0;

    virtual ITrackDataPtr cutLabel(const LabelKey& labelKey) = 0;
    virtual ITrackDataPtr copyLabel(const LabelKey& labelKey) = 0;

    virtual bool moveLabels(secs_t timePositionOffset) = 0;
    virtual muse::RetVal<LabelKeyList> moveLabels(const LabelKeyList& labelKeys, const trackedit::TrackId& toTrackId) = 0;

    virtual bool stretchLabelLeft(const LabelKey& labelKey, secs_t newStartTime, bool completed) = 0;
    virtual bool stretchLabelRight(const LabelKey& labelKey, secs_t newEndTime, bool completed) = 0;

    virtual std::optional<secs_t> getLeftmostLabelStartTime(const LabelKeyList& labelKeys) const = 0;

    virtual muse::Progress progress() const = 0;
};
}
