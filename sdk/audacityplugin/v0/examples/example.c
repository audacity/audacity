/* SPDX-License-Identifier: GPL-3.0-or-later */
/* Example Audacity Plugin API v0 offline effect in C11. */

#include "audacity_plugin_v0.h"

#include <stdlib.h>

typedef struct example_effect {
    aup_effect_v0 effect;
    aup_effect_offline_v0 offline;
} example_effect;

static aup_status AUP_CALL example_process(
    void* context,
    const aup_effect_offline_args_v0* args,
    const aup_effect_offline_host_v0* host)
{
    (void)context;
    (void)args;
    if (host->cancelled(host->context)) {
        return AUP_CANCELLED;
    }
    return host->progress(host->context, 1.0, AUP_STR("Complete"))
           ? AUP_OK
           : AUP_CANCELLED;
}

static const void* AUP_CALL example_effect_query(
    void* context, aup_str capability_id)
{
    example_effect* self = (example_effect*)context;
    return aup_str_eq(capability_id, AUP_STR(AUP_CAP_EFFECT_OFFLINE_V0))
           ? &self->offline
           : NULL;
}

static const aup_effect_desc example_descriptor = {
    AUP_STR_INIT("example.offline"),
    AUP_STR_INIT("Example Offline Effect"),
    AUP_STR_INIT("Demonstrates API v0 discovery."),
    AUP_EFFECT_GROUP_TOOLS,
    0u
};

static aup_status AUP_CALL example_create(
    void* context,
    aup_str effect_id,
    const aup_effect_create_info* info,
    const aup_effect_v0** out_effect)
{
    example_effect* created;
    (void)context;
    (void)info;
    *out_effect = NULL;
    if (!aup_str_eq(effect_id, example_descriptor.id)) {
        return AUP_INVALID_ARGUMENT;
    }
    created = (example_effect*)calloc(1u, sizeof(*created));
    if (created == NULL) {
        return AUP_OUT_OF_MEMORY;
    }
    created->effect.context = created;
    created->effect.query = example_effect_query;
    created->offline.context = created;
    created->offline.process = example_process;
    *out_effect = &created->effect;
    return AUP_OK;
}

static void AUP_CALL example_destroy(
    void* context, const aup_effect_v0* effect)
{
    (void)context;
    free(effect->context);
}

static const aup_effects_v0 example_effects = {
    NULL, 1u, &example_descriptor, example_create, example_destroy
};

static aup_status AUP_CALL example_initialize(
    void* context, const aup_host_v0* host, aup_str bundle, aup_str data)
{
    (void)context;
    (void)host;
    (void)bundle;
    (void)data;
    return AUP_OK;
}

static const void* AUP_CALL example_query(void* context, aup_str capability_id)
{
    (void)context;
    return aup_str_eq(capability_id, AUP_STR(AUP_CAP_EFFECTS_V0))
           ? &example_effects
           : NULL;
}

static void AUP_CALL example_shutdown(void* context)
{
    (void)context;
}

static const aup_plugin_v0 example_plugin = {
    NULL, example_initialize, example_query, example_shutdown
};

AUP_EXPORT const aup_plugin_v0* AUP_CALL audacity_plugin_entry_v0(void)
{
    return &example_plugin;
}
