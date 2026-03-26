/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <string>

namespace au::project {
struct CloudErrorDialogInfo {
    std::string title;
    std::string message;
};

CloudErrorDialogInfo getCloudErrorDialogInfo(int code);
}
