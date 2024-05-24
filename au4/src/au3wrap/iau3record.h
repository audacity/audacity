/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_IAU3RECORD_H
#define AU_AU3WRAP_IAU3RECORD_H

#include <memory>

#include "modularity/imoduleinterface.h"

#include "iau3audioinput.h"

namespace au::au3 {
class IAu3Record : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAu3Record)
public:
    virtual ~IAu3Record() = default;

    virtual void start() = 0;
    virtual void stop() = 0;

    virtual IAu3AudioInputPtr audioInput() const = 0;
};

using IAu3AudioRecordPtr = std::shared_ptr<IAu3Record>;
}

#endif // AU_AU3WRAP_IAU3RECORD_H
