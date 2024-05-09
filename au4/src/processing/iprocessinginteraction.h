#pragma once

#include "modularity/imoduleinterface.h"

#include "processingtypes.h"

namespace au::processing {
//! NOTE Interface for interacting with the project
//! When it gets big, maybe we’ll divide it into several
//! Currently implemented in the au3wrap module
class IProcessingInteraction : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IProcessingInteraction)

public:
    virtual ~IProcessingInteraction() = default;

    virtual bool changeClipStartTime(const ClipKey& clipKey, double sec) = 0;
};
}
