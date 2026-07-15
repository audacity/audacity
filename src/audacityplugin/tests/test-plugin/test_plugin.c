/*
 * Audacity: A Digital Audio Editor
 */
#include "audacity_plugin_v0.h"

#include <math.h>
#include <stdlib.h>

#define BLOCK_SAMPLES ((uint64_t)1024u)

typedef struct gain_effect {
    aup_effect_v0 effect;
    aup_effect_parameters_v0 parameters;
    aup_effect_offline_v0 offline;
    double gain;
} gain_effect;

static const aup_value_desc gain_parameters[] = {
    {
        .key = AUP_STR_INIT("gain"),
        .name = AUP_STR_INIT("Gain"),
        .description = AUP_STR_INIT("Linear amplitude multiplier"),
        .unit = AUP_STR_INIT(""),
        .type = AUP_VALUE_DOUBLE,
        .has_min = true,
        .has_max = true,
        .has_step = false,
        .default_value = { .as_double = 1.0 },
        .min = { .as_double = 0.0 },
        .max = { .as_double = 4.0 },
        .step = { .as_double = 0.0 },
        .enum_choice_count = 0,
        .enum_choices = NULL,
    },
};

static void AUP_CALL get_parameter(
    void* context, uint64_t index, aup_value* out_value)
{
    gain_effect* effect = (gain_effect*)context;
    if (effect != NULL && index == 0u && out_value != NULL) {
        out_value->as_double = effect->gain;
    }
}

static aup_status AUP_CALL set_parameter(
    void* context, uint64_t index, const aup_value* value)
{
    gain_effect* effect = (gain_effect*)context;
    if (effect == NULL || index != 0u || value == NULL) {
        return AUP_INVALID_ARGUMENT;
    }
    if (!isfinite(value->as_double)
        || value->as_double < 0.0 || value->as_double > 4.0) {
        return AUP_VALIDATION_FAILED;
    }
    effect->gain = value->as_double;
    return AUP_OK;
}

static aup_status AUP_CALL validate_parameters(void* context)
{
    const gain_effect* effect = (const gain_effect*)context;
    return effect != NULL && isfinite(effect->gain)
           && effect->gain >= 0.0 && effect->gain <= 4.0
           ? AUP_OK : AUP_VALIDATION_FAILED;
}

static aup_status AUP_CALL process(
    void* context,
    const aup_effect_offline_args_v0* args,
    const aup_effect_offline_host_v0* host)
{
    gain_effect* effect = (gain_effect*)context;
    float samples[2][BLOCK_SAMPLES];
    const float* buffers[2] = { samples[0], samples[1] };
    uint64_t track_index;

    if (effect == NULL || args == NULL || host == NULL) {
        return AUP_INVALID_ARGUMENT;
    }
    if (args->audio_track_count == 0u) {
        return AUP_NOT_READY;
    }

    for (track_index = 0; track_index < args->audio_track_count; ++track_index) {
        const aup_audio_track* track = &args->audio_tracks[track_index];
        aup_audio audio = AUP_INVALID_AUDIO;
        uint64_t offset = 0u;
        aup_status status;

        if (track->audio == AUP_INVALID_AUDIO || track->info.sample_count == 0u) {
            return AUP_NOT_READY;
        }

        status = host->create_audio(host->context, &track->info.format, &audio);
        if (status != AUP_OK) {
            return status;
        }

        {
            aup_audio_chunk empty = {
                NULL, track->info.format.channel_count, 0u
            };
            status = host->write_audio(host->context, audio, &empty);
            if (status != AUP_OK) {
                return status;
            }
        }

        while (offset < track->info.sample_count) {
            aup_audio_chunk input = { 0 };
            aup_audio_chunk gained;
            uint64_t requested = track->info.sample_count - offset;
            uint32_t channel;
            uint64_t sample;
            double track_progress;

            if (host->cancelled(host->context)) {
                return AUP_CANCELLED;
            }
            if (requested > BLOCK_SAMPLES) {
                requested = BLOCK_SAMPLES;
            }

            status = host->read_audio(
                host->context, track->audio, offset, requested, &input);
            if (status != AUP_OK) {
                return status;
            }
            if (input.sample_count == 0u
                || input.sample_count > requested
                || input.channel_count != track->info.format.channel_count) {
                return AUP_PLUGIN_ERROR;
            }

            for (channel = 0; channel < input.channel_count; ++channel) {
                for (sample = 0; sample < input.sample_count; ++sample) {
                    samples[channel][sample]
                        = (float)(input.channels[channel][sample] * effect->gain);
                }
            }

            gained.channels = buffers;
            gained.channel_count = input.channel_count;
            gained.sample_count = input.sample_count;
            status = host->write_audio(host->context, audio, &gained);
            if (status != AUP_OK) {
                return status;
            }

            offset += input.sample_count;
            track_progress = (double)offset / (double)track->info.sample_count;
            if (!host->progress(
                    host->context,
                    ((double)track_index + track_progress)
                    / (double)args->audio_track_count,
                    AUP_STR("Applying gain"))) {
                return AUP_CANCELLED;
            }
        }

        status = host->replace_audio_track(
            host->context, audio, track->track);
        if (status != AUP_OK) {
            return status;
        }
    }

    return AUP_OK;
}

static const void* AUP_CALL query_effect(void* context, aup_str capability_id)
{
    gain_effect* effect = (gain_effect*)context;
    if (effect == NULL) {
        return NULL;
    }
    if (aup_str_eq(capability_id, AUP_STR(AUP_CAP_EFFECT_PARAMETERS_V0))) {
        return &effect->parameters;
    }
    if (aup_str_eq(capability_id, AUP_STR(AUP_CAP_EFFECT_OFFLINE_V0))) {
        return &effect->offline;
    }
    return NULL;
}

static aup_status AUP_CALL create_effect(
    void* context,
    aup_str effect_id,
    const aup_effect_create_info* info,
    const aup_effect_v0** out_effect)
{
    gain_effect* effect;
    (void)context;

    if (out_effect == NULL) {
        return AUP_INVALID_ARGUMENT;
    }
    *out_effect = NULL;
    if (info == NULL || !aup_str_eq(effect_id, AUP_STR("gain"))) {
        return AUP_INVALID_ARGUMENT;
    }

    effect = (gain_effect*)calloc(1u, sizeof(*effect));
    if (effect == NULL) {
        return AUP_OUT_OF_MEMORY;
    }

    effect->gain = 1.0;
    effect->effect = (aup_effect_v0) {
        effect, query_effect
    };
    effect->parameters = (aup_effect_parameters_v0) {
        effect,
        1u,
        gain_parameters,
        get_parameter,
        set_parameter,
        validate_parameters,
    };
    effect->offline = (aup_effect_offline_v0) {
        effect, process
    };
    *out_effect = &effect->effect;
    return AUP_OK;
}

static void AUP_CALL destroy_effect(
    void* context, const aup_effect_v0* effect)
{
    (void)context;
    if (effect != NULL) {
        free(effect->context);
    }
}

static const aup_effect_desc effect_descriptors[] = {
    {
        AUP_STR_INIT("gain"),
        AUP_STR_INIT("Streaming Gain"),
        AUP_STR_INIT("Applies gain while streaming audio in bounded blocks."),
        AUP_EFFECT_GROUP_EFFECT,
        AUP_EFFECT_INPUT_TRACK_AUDIO,
    },
};

static const aup_effects_v0 effects = {
    NULL,
    1u,
    effect_descriptors,
    create_effect,
    destroy_effect,
};

static aup_status AUP_CALL initialize_plugin(
    void* context,
    const aup_host_v0* host,
    aup_str bundle_path,
    aup_str data_path)
{
    (void)context;
    (void)host;
    (void)bundle_path;
    (void)data_path;
    return AUP_OK;
}

static const void* AUP_CALL query_plugin(void* context, aup_str capability_id)
{
    (void)context;
    return aup_str_eq(capability_id, AUP_STR(AUP_CAP_EFFECTS_V0))
           ? &effects : NULL;
}

static void AUP_CALL shutdown_plugin(void* context)
{
    (void)context;
}

AUP_EXPORT const aup_plugin_v0* AUP_CALL audacity_plugin_entry_v0(void)
{
    static const aup_plugin_v0 plugin = {
        NULL,
        initialize_plugin,
        query_plugin,
        shutdown_plugin,
    };
    return &plugin;
}
