/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROCESSING_IPROCESSINGCONTROLLER_H
#define AU_PROCESSING_IPROCESSINGCONTROLLER_H

#include "modularity/imoduleinterface.h"
#include "async/channel.h"
#include "global/progress.h"
#include "actions/actiontypes.h"

namespace au::processing {
class IProcessingController : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProcessingController)

public:
    virtual ~IProcessingController() = default;

    virtual bool actionChecked(const muse::actions::ActionCode& actionCode) const = 0;
    virtual muse::async::Channel<muse::actions::ActionCode> actionCheckedChanged() const = 0;
};
}

#endif // AU_PROCESSING_IPROCESSINGCONTROLLER_H
