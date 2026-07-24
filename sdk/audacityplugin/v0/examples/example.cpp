/* SPDX-License-Identifier: GPL-3.0-or-later */
/* Example Audacity Plugin API v0 offline effect in C++11. */

#include "audacity_plugin_v0.h"
#include <new>

namespace {
struct Instance {
    aup_effect_v0 effect;
    aup_effect_offline_v0 offline;
};

aup_status AUP_CALL process(
    void*, const aup_effect_offline_args_v0*,
    const aup_effect_offline_host_v0* host) noexcept
{
    if (host->cancelled(host->context)) {
        return AUP_CANCELLED;
    }
    return host->progress(host->context, 1.0, AUP_STR("Complete"))
           ? AUP_OK : AUP_CANCELLED;
}

const void* AUP_CALL effect_query(void* context, aup_str id) noexcept
{
    auto* self = static_cast<Instance*>(context);
    return aup_str_eq(id, AUP_STR(AUP_CAP_EFFECT_OFFLINE_V0))
           ? &self->offline : nullptr;
}

const aup_effect_desc descriptor = {
    AUP_STR("example.offline"), AUP_STR("Example Offline Effect"),
    AUP_STR("Demonstrates API v0 discovery."), AUP_EFFECT_GROUP_TOOLS, 0u
};

aup_status AUP_CALL create(void*, aup_str id,
                           const aup_effect_create_info*,
                           const aup_effect_v0** out) noexcept
{
    *out = nullptr;
    if (!aup_str_eq(id, descriptor.id)) {
        return AUP_INVALID_ARGUMENT;
    }
    auto* instance = new (std::nothrow) Instance{};
    if (!instance) {
        return AUP_OUT_OF_MEMORY;
    }
    instance->effect = aup_effect_v0{ instance, effect_query };
    instance->offline = aup_effect_offline_v0{ instance, process };
    *out = &instance->effect;
    return AUP_OK;
}

void AUP_CALL destroy(void*, const aup_effect_v0* effect) noexcept
{
    delete static_cast<Instance*>(effect->context);
}

const aup_effects_v0 effects = { nullptr, 1u, &descriptor, create, destroy };
aup_status AUP_CALL initialize(void*, const aup_host_v0*, aup_str, aup_str) noexcept
{ return AUP_OK; }
const void* AUP_CALL query(void*, aup_str id) noexcept
{
    return aup_str_eq(id, AUP_STR(AUP_CAP_EFFECTS_V0)) ? &effects : nullptr;
}

void AUP_CALL shutdown(void*) noexcept {}
const aup_plugin_v0 plugin = { nullptr, initialize, query, shutdown };
} // namespace

extern "C" AUP_EXPORT const aup_plugin_v0 * AUP_CALL
audacity_plugin_entry_v0(void)
{
    return &plugin;
}
