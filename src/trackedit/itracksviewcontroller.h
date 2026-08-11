/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <optional>

#include "modularity/imoduleinterface.h"
#include "async/channel.h"

#include "trackedittypes.h"

namespace au::trackedit {
class ITracksViewController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITracksViewController)

public:
    virtual ~ITracksViewController() = default;

    //! NOTE Set when a label should enter title edit mode; the view consumes
    //! the request once the label item exists, whenever that happens
    virtual void requestLabelTitleEdit(const LabelKey& labelKey) = 0;
    virtual std::optional<LabelKey> pendingLabelTitleEdit() const = 0;
    virtual void labelTitleEditRequestHandled(const LabelKey& labelKey) = 0;
    virtual muse::async::Channel<LabelKey> labelTitleEditRequested() const = 0;
};
}
