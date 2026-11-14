/*
 * Audacity: A Digital Audio Editor
 */
#include "customschemeregistrar.h"

namespace au::au3cloud {
// Dummy impl for macOS and Linux
// macOS register scheme via .plist
// Linux reguster scheme via .desktop
bool registerCustomScheme(const QString& /*scheme*/)
{
    return true;
}
}
