/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "global/types/string.h"
#include "global/progress.h"

#include "trackedittypes.h"

#include "modularity/imoduleinterface.h"

namespace au::trackedit {
class ILabelsInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ILabelsInteraction)

public:
    virtual ~ILabelsInteraction() = default;

    virtual bool addLabelToSelection() = 0;
    virtual bool changeLabelTitle(const LabelKey& labelKey, const muse::String& title) = 0;

    virtual muse::Progress progress() const = 0;
};
}
