/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

#include "framework/global/modularity/imoduleinterface.h"

namespace au::au3cloud {
class IUserData : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IUserData)

public:
    virtual ~IUserData() = default;

    virtual std::string getAvatarPath() const = 0;
    virtual std::string getDisplayName() const = 0;
};
}
