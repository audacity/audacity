/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

#include <QVariant>

#include "framework/global/types/val.h"
#include "framework/global/settings.h"
#include "framework/global/types/uri.h"

namespace muse::extensions {
struct Manifest;
}

namespace au::extensions {
muse::Settings::Key extensionPreferenceKey(const muse::Uri& uri, const std::string& key);

QVariant extensionPreferenceDefault(const muse::ValMap& preference);
void registerExtensionPreferenceDefaults(const muse::extensions::Manifest& manifest);
} // namespace au::extensions
