/* SPDX-License-Identifier: GPL-3.0-or-later */
/* Audacity Plugin API v0 */
/* Work in progress */

#ifndef AUP_PLUGIN_V0_H
#define AUP_PLUGIN_V0_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define AUP_API_MAJOR ((uint32_t)0u)
#define AUP_API_REVISION ((uint32_t)1u)

#if !defined(AUP_EXPORT) && defined(_WIN32)
#define AUP_EXPORT __declspec(dllexport)
#elif !defined(AUP_EXPORT) && (defined(__GNUC__) || defined(__clang__))
#define AUP_EXPORT __attribute__((visibility("default")))
#elif !defined(AUP_EXPORT)
#define AUP_EXPORT
#endif

#if defined(_WIN32)
#define AUP_CALL __cdecl
#else
#define AUP_CALL
#endif

#if defined(__GNUC__) || defined(__clang__)
#define AUP_INLINE static inline __attribute__((unused))
#else
#define AUP_INLINE static inline
#endif

#if defined(_MSC_VER) || defined(__GNUC__) || defined(__clang__)
#pragma pack(push, 8)
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef int32_t aup_status;

#define AUP_OK ((aup_status)0)
#define AUP_INVALID_ARGUMENT ((aup_status)1)
#define AUP_INVALID_STATE ((aup_status)2)
#define AUP_VALIDATION_FAILED ((aup_status)3)
#define AUP_NOT_READY ((aup_status)4)
#define AUP_CANCELLED ((aup_status)5)
#define AUP_HOST_ERROR ((aup_status)6)
#define AUP_PLUGIN_ERROR ((aup_status)7)
#define AUP_OUT_OF_MEMORY ((aup_status)8)
#define AUP_BUFFER_TOO_SMALL ((aup_status)9)

// The string type is a pointer to a UTF-8 string and its length
// It is not required to be null-terminated
// String views passed to callbacks are valid only for that call
// String views stored in returned tables remain valid while the owning plugin or effect is alive
typedef struct aup_str {
    const char* data;
    uint64_t len;
} aup_str;

#define AUP_STR_INIT(literal) \
    { (literal), (uint64_t)(sizeof(literal) - 1u) }
#ifdef __cplusplus
#define AUP_STR(literal) aup_str AUP_STR_INIT(literal)
#else
#define AUP_STR(literal) ((aup_str)AUP_STR_INIT(literal))
#endif

AUP_INLINE aup_str aup_str_make(const char* data, uint64_t len)
{
    aup_str result = { data, len };
    return result;
}

AUP_INLINE bool aup_str_eq(aup_str left, aup_str right)
{
    return left.len == right.len
           && (left.len == 0u
               || (left.data != NULL && right.data != NULL
                   && memcmp(left.data, right.data, (size_t)left.len) == 0));
}

// Value unions are used for preferences and effect parameters
#define AUP_VALUE_BOOL ((uint32_t)1u) // as_bool
#define AUP_VALUE_INT64 ((uint32_t)2u) // as_i64
#define AUP_VALUE_DOUBLE ((uint32_t)3u) // as_double
#define AUP_VALUE_STRING ((uint32_t)4u) // UTF-8 as_string
#define AUP_VALUE_ENUM ((uint32_t)5u) // UTF-8 token in as_string
#define AUP_VALUE_FILE ((uint32_t)6u) // UTF-8 path in as_string
#define AUP_VALUE_DIRECTORY ((uint32_t)7u) // UTF-8 path in as_string

typedef union aup_value {
    bool as_bool;
    int64_t as_i64;
    double as_double;
    aup_str as_string;
} aup_value;

typedef struct aup_enum_choice {
    aup_str token;
    aup_str name;
} aup_enum_choice;

typedef struct aup_value_desc {
    aup_str key;
    aup_str name;
    aup_str description;
    aup_str unit;
    uint32_t type;
    bool has_min;
    bool has_max;
    bool has_step;
    aup_value default_value;
    aup_value min;
    aup_value max;
    aup_value step;
    uint64_t enum_choice_count;
    const aup_enum_choice* enum_choices;
} aup_value_desc;

///
/// Plugin <-> Host discovery
///

typedef const void* (AUP_CALL* aup_query_fn)(
    void* context,
    aup_str capability_id);

// The host interface. A plugin uses this to query for host capabilities
typedef struct aup_host_v0 {
    void* context;
    aup_query_fn query;
} aup_host_v0;

// Plugin initialization. The host calls this once per plugin library, before any other calls
typedef aup_status (AUP_CALL* aup_plugin_initialize_fn)(
    void* context,
    const aup_host_v0* host,
    aup_str bundle_path,
    aup_str data_path);

// Plugin shutdown. The host calls this when the plugin library is unloaded (on shutdown, currently it is not possible to unload a plugin at runtime)
typedef void (AUP_CALL* aup_plugin_shutdown_fn)(void* context);

typedef struct aup_plugin_v0 {
    void* context;
    aup_plugin_initialize_fn initialize;
    aup_query_fn query;
    aup_plugin_shutdown_fn shutdown;
} aup_plugin_v0;

// The plugin entry point and the only exported API symbol. The host calls this to get the plugin interface
typedef const aup_plugin_v0* (AUP_CALL* aup_plugin_entry_fn)(void);

#if !defined(AUP_NO_ENTRY_DECL)
AUP_EXPORT const aup_plugin_v0 * AUP_CALL
audacity_plugin_entry_v0(void);
#endif

// Preferences capability allows plugins to expose a set of user-configurable values
#define AUP_CAP_PREFERENCES_V0 "audacity.preferences/0"

// Validate and apply always receive a complete set in the same order as returned by aup_preferences_v0.items
typedef aup_status (AUP_CALL* aup_preferences_validate_fn)(
    void* context,
    uint64_t count,
    const aup_value* values);

typedef aup_status (AUP_CALL* aup_preferences_apply_fn)(
    void* context,
    uint64_t count,
    const aup_value* values);

// Audacity stores and restores preferences in its own configuration
typedef struct aup_preferences_v0 {
    void* context;
    uint64_t count;
    const aup_value_desc* items;
    aup_preferences_validate_fn validate;
    aup_preferences_apply_fn apply;
} aup_preferences_v0;

// Effects capability allows plugins to expose a set of audio processing algorithms
#define AUP_CAP_EFFECTS_V0 "audacity.effects/0"

// Effect groups. Defines the general purpose of the effect, and affects the UI placement in the menu
#define AUP_EFFECT_GROUP_GENERATE ((uint32_t)1u)
#define AUP_EFFECT_GROUP_EFFECT ((uint32_t)2u)
#define AUP_EFFECT_GROUP_ANALYZE ((uint32_t)3u)
#define AUP_EFFECT_GROUP_TOOLS ((uint32_t)4u)

// Effect input track types
#define AUP_EFFECT_INPUT_TRACK_AUDIO ((uint32_t)1u << 0u)
#define AUP_EFFECT_INPUT_TRACK_LABEL ((uint32_t)1u << 1u)

// id should be unique within the plugin and should not change between plugin releases
typedef struct aup_effect_desc {
    aup_str id;
    aup_str name;
    aup_str description;
    uint32_t group;
    uint32_t input_track_types;
} aup_effect_desc;

typedef struct aup_effect_create_info {
    double selection_duration_seconds; // zero denotes a point selection at the cursor
    uint32_t project_sample_rate; // whole Hz
} aup_effect_create_info;

// Effect instance handle
typedef struct aup_effect_v0 {
    void* context;
    aup_query_fn query;
} aup_effect_v0;

typedef aup_status (AUP_CALL* aup_effect_create_fn)(
    void* context,
    aup_str effect_id,
    const aup_effect_create_info* info,
    const aup_effect_v0** out_effect);

typedef void (AUP_CALL* aup_effect_destroy_fn)(
    void* context,
    const aup_effect_v0* effect);

// Effects capability interface. The host uses this to discover effects, create and destroy effect instances
typedef struct aup_effects_v0 {
    void* context;
    uint64_t count;
    const aup_effect_desc* items;
    aup_effect_create_fn create;
    aup_effect_destroy_fn destroy;
} aup_effects_v0;

// Effect parameters capability allows effect instances to expose a set of user-configurable values
#define AUP_CAP_EFFECT_PARAMETERS_V0 "audacity.effect.parameters/0"

typedef void (AUP_CALL* aup_effect_parameters_get_fn)(
    void* context,
    uint64_t index,
    aup_value* out_value);

typedef aup_status (AUP_CALL* aup_effect_parameters_set_fn)(
    void* context,
    uint64_t index,
    const aup_value* value);

typedef aup_status (AUP_CALL* aup_effect_parameters_validate_fn)(void* context);

// Effect parameters capability interface. The host uses this to discover and get/set effect parameters
typedef struct aup_effect_parameters_v0 {
    void* context;
    uint64_t count;
    const aup_value_desc* items;
    aup_effect_parameters_get_fn get;
    aup_effect_parameters_set_fn set;
    aup_effect_parameters_validate_fn validate;
} aup_effect_parameters_v0;

// Effect should provide offline processing capability
#define AUP_CAP_EFFECT_OFFLINE_V0 "audacity.effect.offline/0"

// Handles are valid only within a single process call
typedef uint64_t aup_track;
typedef uint64_t aup_audio;
typedef uint64_t aup_label;

#define AUP_INVALID_TRACK ((aup_track)0u)
#define AUP_INVALID_AUDIO ((aup_audio)0u)
#define AUP_INVALID_LABEL ((aup_label)0u)

typedef struct aup_audio_format {
    uint32_t channel_count;
    uint32_t sample_rate;
} aup_audio_format;

typedef struct aup_audio_info {
    aup_audio_format format;
    uint64_t sample_count;
} aup_audio_info;

typedef struct aup_audio_track {
    aup_str name;
    aup_track track;
    aup_audio audio;
    aup_audio_info info;
} aup_audio_track;

// Label times are relative to the selection start and unclipped
// start may be negative and end may exceed the selection duration
typedef struct aup_label_info {
    aup_label label;
    double start_seconds;
    double end_seconds;
    aup_str text;
} aup_label_info;

typedef struct aup_label_track {
    aup_str name;
    aup_track track;
    uint64_t label_count;
    const aup_label_info* labels;
} aup_label_track;

typedef struct aup_effect_offline_args_v0 {
    double selection_duration_seconds;
    double generator_duration_seconds;
    uint64_t audio_track_count;
    const aup_audio_track* audio_tracks;
    uint64_t label_track_count;
    const aup_label_track* label_tracks;
} aup_effect_offline_args_v0;

// Audio data valid until the next read_audio, audio's release or return from the process callback
typedef struct aup_audio_chunk {
    const float* const* channels;
    uint32_t channel_count;
    uint64_t sample_count;
} aup_audio_chunk;

typedef aup_status (AUP_CALL* aup_convert_audio_fn)(
    void* context,
    aup_audio audio,
    const aup_audio_format* format,
    aup_audio* out_audio,
    aup_audio_info* out_info);

// May return fewer samples than requested, continue reading at the next offset
typedef aup_status (AUP_CALL* aup_read_audio_fn)(
    void* context,
    aup_audio audio,
    uint64_t sample_offset,
    uint64_t requested_samples,
    aup_audio_chunk* out_chunk);

typedef aup_status (AUP_CALL* aup_create_audio_fn)(
    void* context,
    const aup_audio_format* format,
    aup_audio* out_audio);

typedef aup_status (AUP_CALL* aup_write_audio_fn)(
    void* context,
    aup_audio audio,
    const aup_audio_chunk* chunk);

typedef aup_status (AUP_CALL* aup_replace_audio_track_fn)(
    void* context,
    aup_audio audio,
    aup_track track);

typedef aup_status (AUP_CALL* aup_add_audio_track_fn)(
    void* context,
    aup_audio audio,
    aup_str name);

typedef aup_status (AUP_CALL* aup_release_audio_fn)(
    void* context,
    aup_audio audio);

typedef aup_status (AUP_CALL* aup_create_label_track_fn)(
    void* context,
    aup_str name,
    aup_track* out_track);

typedef aup_status (AUP_CALL* aup_add_label_fn)(
    void* context,
    aup_track track,
    double start_seconds,
    double end_seconds,
    aup_str text,
    // May be null
    aup_label* out_label);

typedef aup_status (AUP_CALL* aup_update_label_fn)(
    void* context,
    aup_label label,
    double start_seconds,
    double end_seconds,
    aup_str text);

typedef aup_status (AUP_CALL* aup_delete_label_fn)(
    void* context,
    aup_label label);

// Allows the plugin to check if the user has requested cancellation of the effect
typedef bool (AUP_CALL* aup_cancelled_fn)(void* context);

// Allows the plugin to report progress to the user. Returns false if the user has requested cancellation
typedef bool (AUP_CALL* aup_progress_fn)(
    void* context,
    double fraction,
    aup_str message);

typedef struct aup_effect_offline_host_v0 {
    void* context;
    aup_query_fn query;
    aup_convert_audio_fn convert_audio;
    aup_read_audio_fn read_audio;
    aup_create_audio_fn create_audio;
    aup_write_audio_fn write_audio;
    aup_replace_audio_track_fn replace_audio_track;
    aup_add_audio_track_fn add_audio_track;
    aup_release_audio_fn release_audio;
    aup_create_label_track_fn create_label_track;
    aup_add_label_fn add_label;
    aup_update_label_fn update_label;
    aup_delete_label_fn delete_label;
    aup_progress_fn progress;
    aup_cancelled_fn cancelled;
} aup_effect_offline_host_v0;

// Main blocking process call, runs on a worker thread
// The host table and its callbacks are valid only during this call and only from this thread
// One `process` call is a single complete transaction
//
// A single effect instance may be used for multiple `process` calls, but not concurrently
typedef aup_status (AUP_CALL* aup_effect_offline_process_fn)(
    void* context,
    const aup_effect_offline_args_v0* args,
    const aup_effect_offline_host_v0* host);

typedef struct aup_effect_offline_v0 {
    void* context;
    aup_effect_offline_process_fn process;
} aup_effect_offline_v0;

#ifdef __cplusplus
} /* extern "C" */
#endif

#if defined(_MSC_VER) || defined(__GNUC__) || defined(__clang__)
#pragma pack(pop)
#endif

#endif /* AUP_PLUGIN_V0_H */
