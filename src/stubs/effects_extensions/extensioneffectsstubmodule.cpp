/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectsstubmodule.h"

namespace au::effects::extensions {
std::string ExtensionEffectsModule::moduleName() const
{
    return "effects_extensions";
}
} // namespace au::effects::extensions
