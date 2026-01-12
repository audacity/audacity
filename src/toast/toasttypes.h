/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <string>

namespace au::toast {
enum class ToastActionCode {
    None = 0,
    Custom,
};
struct ToastAction {
    std::string text;
    ToastActionCode code = ToastActionCode::None;
};
}
