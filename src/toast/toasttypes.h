/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

namespace au::toast {
enum class ToastActionCode {
    None = 0,
    Action1,
    Action2,
    Action3
};
struct ToastAction {
    std::string text;
    ToastActionCode code = ToastActionCode::None;
};
}
