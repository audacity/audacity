/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QString>

namespace au::au3cloud {
//! Register a custom URL scheme at runtime so the OS routes "<scheme>://..."
//! clicks to this app. Returns false on failure. No-op on macOS (Info.plist
//! CFBundleURLTypes) and Linux (.desktop files).
bool registerCustomScheme(const QString& scheme);
}
