/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/imoduleinterface.h"

#include "draw/types/color.h"

namespace au::record {
class IRecordConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecordConfiguration)

public:
    virtual ~IRecordConfiguration() = default;

    virtual muse::draw::Color recordColor() const = 0;
};
}
