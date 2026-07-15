/*
 * Audacity: A Digital Audio Editor
 */
#include "abi.h"

#include <algorithm>
#include <cmath>
#include <optional>
#include <string_view>
#include <unordered_set>
#include <utility>

#include "global/types/string.h"

namespace au::audacityplugin::internal {
static_assert(static_cast<int32_t>(Status::Ok) == AUP_OK);
static_assert(static_cast<int32_t>(Status::InvalidArgument) == AUP_INVALID_ARGUMENT);
static_assert(static_cast<int32_t>(Status::InvalidState) == AUP_INVALID_STATE);
static_assert(static_cast<int32_t>(Status::ValidationFailed) == AUP_VALIDATION_FAILED);
static_assert(static_cast<int32_t>(Status::NotReady) == AUP_NOT_READY);
static_assert(static_cast<int32_t>(Status::Cancelled) == AUP_CANCELLED);
static_assert(static_cast<int32_t>(Status::HostError) == AUP_HOST_ERROR);
static_assert(static_cast<int32_t>(Status::PluginError) == AUP_PLUGIN_ERROR);
static_assert(static_cast<int32_t>(Status::OutOfMemory) == AUP_OUT_OF_MEMORY);
static_assert(static_cast<int32_t>(Status::BufferTooSmall) == AUP_BUFFER_TOO_SMALL);
static_assert(static_cast<uint32_t>(PresentationGroup::Generate) == AUP_EFFECT_GROUP_GENERATE);
static_assert(static_cast<uint32_t>(PresentationGroup::Tools) == AUP_EFFECT_GROUP_TOOLS);
static_assert(InputTrackAudio == AUP_EFFECT_INPUT_TRACK_AUDIO
              && InputTrackLabel == AUP_EFFECT_INPUT_TRACK_LABEL);
static_assert(static_cast<uint32_t>(ParameterType::Boolean) == AUP_VALUE_BOOL);
static_assert(static_cast<uint32_t>(ParameterType::Int64) == AUP_VALUE_INT64);
static_assert(static_cast<uint32_t>(ParameterType::Double) == AUP_VALUE_DOUBLE);
static_assert(static_cast<uint32_t>(ParameterType::String) == AUP_VALUE_STRING);
static_assert(static_cast<uint32_t>(ParameterType::Enumeration) == AUP_VALUE_ENUM);
static_assert(static_cast<uint32_t>(ParameterType::File) == AUP_VALUE_FILE);
static_assert(static_cast<uint32_t>(ParameterType::Directory) == AUP_VALUE_DIRECTORY);

Status statusFromAbi(aup_status status) noexcept
{
    if (status >= AUP_OK && status <= AUP_BUFFER_TOO_SMALL) {
        return static_cast<Status>(status);
    }
    return Status::PluginError;
}

aup_str stringView(const std::string& value) noexcept
{
    return { value.data(), static_cast<uint64_t>(value.size()) };
}

bool validUtf8(const char* bytes, uint64_t length)
{
    if (length != 0 && !bytes) {
        return false;
    }
    const std::string_view text(bytes ? bytes : "", static_cast<size_t>(length));
    return muse::UtfCodec::isValidUtf8(text)
           && text.find('\0') == std::string_view::npos;
}

bool copyString(aup_str source, std::string& target)
{
    if (!validUtf8(source.data, source.len)) {
        return false;
    }
    target.assign(source.data ? source.data : "", static_cast<size_t>(source.len));
    return true;
}

bool copyValue(const aup_value& source, ParameterType expected, Value& target)
{
    switch (expected) {
    case ParameterType::Boolean:
        target = source.as_bool;
        return true;
    case ParameterType::Int64:
        target = source.as_i64;
        return true;
    case ParameterType::Double:
        if (!std::isfinite(source.as_double)) {
            return false;
        }
        target = source.as_double;
        return true;
    case ParameterType::String: {
        std::string value;
        if (!copyString(source.as_string, value)) {
            return false;
        }
        target = std::move(value);
        return true;
    }
    case ParameterType::Enumeration:
    case ParameterType::File:
    case ParameterType::Directory: {
        std::string value;
        if (!copyString(source.as_string, value)) {
            return false;
        }
        target = std::move(value);
        return true;
    }
    }
    return false;
}

aup_value abiValue(const Value& source, ParameterType type) noexcept
{
    aup_value result {};
    switch (type) {
    case ParameterType::Boolean: result.as_bool = std::get<bool>(source);
        break;
    case ParameterType::Int64: result.as_i64 = std::get<int64_t>(source);
        break;
    case ParameterType::Double: result.as_double = std::get<double>(source);
        break;
    case ParameterType::String:
    case ParameterType::Enumeration:
    case ParameterType::File:
    case ParameterType::Directory:
        result.as_string = stringView(std::get<std::string>(source));
        break;
    }
    return result;
}

bool validateValue(const ParameterDescriptor& descriptor, const Value& value)
{
    if (value.index() != descriptor.defaultValue.index()) {
        return false;
    }
    if (descriptor.type == ParameterType::Double
        && !std::isfinite(std::get<double>(value))) {
        return false;
    }
    if (descriptor.type == ParameterType::String
        || descriptor.type == ParameterType::Enumeration
        || descriptor.type == ParameterType::File
        || descriptor.type == ParameterType::Directory) {
        const auto& text = std::get<std::string>(value);
        if (!validUtf8(text.data(), text.size())) {
            return false;
        }
    }
    if (descriptor.type == ParameterType::Enumeration) {
        const auto& token = std::get<std::string>(value);
        if (std::none_of(descriptor.enumChoices.begin(), descriptor.enumChoices.end(),
                         [&](const EnumChoice& choice) { return choice.token == token; })) {
            return false;
        }
    }
    if (descriptor.type == ParameterType::Int64) {
        const auto current = std::get<int64_t>(value);
        if (descriptor.minimum
            && current < std::get<int64_t>(*descriptor.minimum)) {
            return false;
        }
        if (descriptor.maximum
            && current > std::get<int64_t>(*descriptor.maximum)) {
            return false;
        }
    } else if (descriptor.type == ParameterType::Double) {
        const auto current = std::get<double>(value);
        if (descriptor.minimum
            && current < std::get<double>(*descriptor.minimum)) {
            return false;
        }
        if (descriptor.maximum
            && current > std::get<double>(*descriptor.maximum)) {
            return false;
        }
    }
    return true;
}

bool copyValueDescriptor(const aup_value_desc& source,
                         ParameterDescriptor& target,
                         std::string& error)
{
    if (!copyString(source.key, target.key) || target.key.empty()
        || target.key.rfind("audacity.", 0) == 0
        || !copyString(source.name, target.name) || target.name.empty()
        || !copyString(source.description, target.description)) {
        error = "value text or key is invalid";
        return false;
    }
    if (source.type < AUP_VALUE_BOOL || source.type > AUP_VALUE_DIRECTORY) {
        error = "value type is invalid";
        return false;
    }
    target.type = static_cast<ParameterType>(source.type);
    if (!copyString(source.unit, target.unit)) {
        error = "value unit is invalid";
        return false;
    }
    if (!copyValue(source.default_value, target.type, target.defaultValue)) {
        error = "value default is invalid";
        return false;
    }

    const bool numeric = target.type == ParameterType::Int64
                         || target.type == ParameterType::Double;
    if (!numeric && !target.unit.empty()) {
        error = "nonnumeric value has a unit";
        return false;
    }
    if (!numeric && (source.has_min || source.has_max || source.has_step)) {
        error = "nonnumeric value has numeric constraints";
        return false;
    }

    const auto copyOptional = [&](bool present, const aup_value& value,
                                  std::optional<Value>& output) {
        if (!present) {
            return true;
        }
        Value copied;
        if (!copyValue(value, target.type, copied)) {
            return false;
        }
        output = std::move(copied);
        return true;
    };
    if (!copyOptional(source.has_min, source.min, target.minimum)
        || !copyOptional(source.has_max, source.max, target.maximum)
        || !copyOptional(source.has_step, source.step, target.step)) {
        error = "numeric constraint is invalid";
        return false;
    }
    if (target.minimum && target.maximum) {
        const bool invalid = target.type == ParameterType::Int64
                             ? std::get<int64_t>(*target.minimum)
                             > std::get<int64_t>(*target.maximum)
                             : std::get<double>(*target.minimum)
                             > std::get<double>(*target.maximum);
        if (invalid) {
            error = "value minimum exceeds maximum";
            return false;
        }
    }
    if (target.step) {
        const bool invalid = target.type == ParameterType::Int64
                             ? std::get<int64_t>(*target.step) <= 0
                             : std::get<double>(*target.step) <= 0.0;
        if (invalid) {
            error = "value step is not positive";
            return false;
        }
    }

    if (target.type == ParameterType::Enumeration) {
        if (source.enum_choice_count == 0 || !source.enum_choices) {
            error = "enumeration has no choices";
            return false;
        }
        std::unordered_set<std::string> tokens;
        for (uint64_t i = 0; i < source.enum_choice_count; ++i) {
            EnumChoice choice;
            if (!copyString(source.enum_choices[i].token, choice.token) || choice.token.empty()
                || !copyString(source.enum_choices[i].name, choice.name) || choice.name.empty()
                || !tokens.insert(choice.token).second) {
                error = "enumeration choice is invalid or duplicated";
                return false;
            }
            target.enumChoices.push_back(std::move(choice));
        }
    } else if (source.enum_choice_count != 0 || source.enum_choices != nullptr) {
        error = "non-enumeration value publishes choices";
        return false;
    }
    if (!validateValue(target, target.defaultValue)) {
        error = "value default violates its schema";
        return false;
    }
    return true;
}
} // namespace au::audacityplugin::internal
