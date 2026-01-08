/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

namespace au::toast {
struct ToastAction {
    std::string text;
    std::function<void()> callback;
};
}
