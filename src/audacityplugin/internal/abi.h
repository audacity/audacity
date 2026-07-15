/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <string>

#include "../audacityplugintypes.h"

#define AUP_NO_ENTRY_DECL
#define AUP_EXPORT
#include "audacity_plugin_v0.h"
#undef AUP_EXPORT
#undef AUP_NO_ENTRY_DECL

namespace au::audacityplugin::internal {
Status statusFromAbi(aup_status status) noexcept;
aup_str stringView(const std::string& value) noexcept;

bool validUtf8(const char* bytes, uint64_t length);
bool copyString(aup_str source, std::string& target);
bool copyValue(const aup_value& source, ParameterType expected, Value& target);
aup_value abiValue(const Value& source, ParameterType type) noexcept;
bool validateValue(const ParameterDescriptor& descriptor, const Value& value);
bool copyValueDescriptor(const aup_value_desc& source, ParameterDescriptor& target, std::string& error);
} // namespace au::audacityplugin::internal
