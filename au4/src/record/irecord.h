/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <memory>

#include "modularity/imoduleinterface.h"

#include "iaudioinput.h"

namespace au::record {
class IRecord : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IRecord)
public:
    virtual ~IRecord() = default;

    virtual void start() = 0;
    virtual void pause() = 0;
    virtual void stop() = 0;

    virtual IAudioInputPtr audioInput() const = 0;
};

using IAudioRecordPtr = std::shared_ptr<IRecord>;
}
