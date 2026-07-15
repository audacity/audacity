/*
 * Audacity: A Digital Audio Editor
 */
#include <stddef.h>

#pragma pack(push, 1)
#define AUP_NO_ENTRY_DECL
#include "audacity_plugin_v0.h"
#pragma pack(pop)

#define AUP_ASSERT_SIZE(type_, size_) \
    _Static_assert(sizeof(type_) == (size_), #type_ " has an unexpected size")
#define AUP_ASSERT_ALIGNMENT(type_, alignment_) \
    _Static_assert(_Alignof(type_) == (alignment_), \
                   #type_ " has an unexpected alignment")
#define AUP_ASSERT_OFFSET(type_, field_, offset_) \
    _Static_assert(offsetof(type_, field_) == (offset_), \
                   #type_ "." #field_ " has an unexpected offset")

_Static_assert(sizeof(void*) == 8, "Plugin API v0 supports native 64-bit hosts only");
_Static_assert(AUP_API_MAJOR == 0u && AUP_API_REVISION == 1u,
               "Plugin API version constants changed");

AUP_ASSERT_SIZE(aup_str, 16);
AUP_ASSERT_ALIGNMENT(aup_str, 8);
AUP_ASSERT_OFFSET(aup_str, data, 0);
AUP_ASSERT_OFFSET(aup_str, len, 8);

AUP_ASSERT_SIZE(aup_value, 16);
AUP_ASSERT_ALIGNMENT(aup_value, 8);
AUP_ASSERT_OFFSET(aup_value, as_bool, 0);
AUP_ASSERT_OFFSET(aup_value, as_i64, 0);
AUP_ASSERT_OFFSET(aup_value, as_double, 0);
AUP_ASSERT_OFFSET(aup_value, as_string, 0);

AUP_ASSERT_SIZE(aup_enum_choice, 32);
AUP_ASSERT_ALIGNMENT(aup_enum_choice, 8);
AUP_ASSERT_OFFSET(aup_enum_choice, token, 0);
AUP_ASSERT_OFFSET(aup_enum_choice, name, 16);

AUP_ASSERT_SIZE(aup_value_desc, 152);
AUP_ASSERT_ALIGNMENT(aup_value_desc, 8);
AUP_ASSERT_OFFSET(aup_value_desc, key, 0);
AUP_ASSERT_OFFSET(aup_value_desc, name, 16);
AUP_ASSERT_OFFSET(aup_value_desc, description, 32);
AUP_ASSERT_OFFSET(aup_value_desc, unit, 48);
AUP_ASSERT_OFFSET(aup_value_desc, type, 64);
AUP_ASSERT_OFFSET(aup_value_desc, has_min, 68);
AUP_ASSERT_OFFSET(aup_value_desc, has_max, 69);
AUP_ASSERT_OFFSET(aup_value_desc, has_step, 70);
AUP_ASSERT_OFFSET(aup_value_desc, default_value, 72);
AUP_ASSERT_OFFSET(aup_value_desc, min, 88);
AUP_ASSERT_OFFSET(aup_value_desc, max, 104);
AUP_ASSERT_OFFSET(aup_value_desc, step, 120);
AUP_ASSERT_OFFSET(aup_value_desc, enum_choice_count, 136);
AUP_ASSERT_OFFSET(aup_value_desc, enum_choices, 144);

AUP_ASSERT_SIZE(aup_host_v0, 16);
AUP_ASSERT_ALIGNMENT(aup_host_v0, 8);
AUP_ASSERT_OFFSET(aup_host_v0, context, 0);
AUP_ASSERT_OFFSET(aup_host_v0, query, 8);

AUP_ASSERT_SIZE(aup_plugin_v0, 32);
AUP_ASSERT_ALIGNMENT(aup_plugin_v0, 8);
AUP_ASSERT_OFFSET(aup_plugin_v0, context, 0);
AUP_ASSERT_OFFSET(aup_plugin_v0, initialize, 8);
AUP_ASSERT_OFFSET(aup_plugin_v0, query, 16);
AUP_ASSERT_OFFSET(aup_plugin_v0, shutdown, 24);

AUP_ASSERT_SIZE(aup_preferences_v0, 40);
AUP_ASSERT_ALIGNMENT(aup_preferences_v0, 8);
AUP_ASSERT_OFFSET(aup_preferences_v0, context, 0);
AUP_ASSERT_OFFSET(aup_preferences_v0, count, 8);
AUP_ASSERT_OFFSET(aup_preferences_v0, items, 16);
AUP_ASSERT_OFFSET(aup_preferences_v0, validate, 24);
AUP_ASSERT_OFFSET(aup_preferences_v0, apply, 32);

AUP_ASSERT_SIZE(aup_effect_desc, 56);
AUP_ASSERT_ALIGNMENT(aup_effect_desc, 8);
AUP_ASSERT_OFFSET(aup_effect_desc, id, 0);
AUP_ASSERT_OFFSET(aup_effect_desc, name, 16);
AUP_ASSERT_OFFSET(aup_effect_desc, description, 32);
AUP_ASSERT_OFFSET(aup_effect_desc, group, 48);
AUP_ASSERT_OFFSET(aup_effect_desc, input_track_types, 52);

AUP_ASSERT_SIZE(aup_effect_create_info, 16);
AUP_ASSERT_ALIGNMENT(aup_effect_create_info, 8);
AUP_ASSERT_OFFSET(aup_effect_create_info, selection_duration_seconds, 0);
AUP_ASSERT_OFFSET(aup_effect_create_info, project_sample_rate, 8);

AUP_ASSERT_SIZE(aup_effect_v0, 16);
AUP_ASSERT_ALIGNMENT(aup_effect_v0, 8);
AUP_ASSERT_OFFSET(aup_effect_v0, context, 0);
AUP_ASSERT_OFFSET(aup_effect_v0, query, 8);

AUP_ASSERT_SIZE(aup_effects_v0, 40);
AUP_ASSERT_ALIGNMENT(aup_effects_v0, 8);
AUP_ASSERT_OFFSET(aup_effects_v0, context, 0);
AUP_ASSERT_OFFSET(aup_effects_v0, count, 8);
AUP_ASSERT_OFFSET(aup_effects_v0, items, 16);
AUP_ASSERT_OFFSET(aup_effects_v0, create, 24);
AUP_ASSERT_OFFSET(aup_effects_v0, destroy, 32);

AUP_ASSERT_SIZE(aup_effect_parameters_v0, 48);
AUP_ASSERT_ALIGNMENT(aup_effect_parameters_v0, 8);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, context, 0);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, count, 8);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, items, 16);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, get, 24);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, set, 32);
AUP_ASSERT_OFFSET(aup_effect_parameters_v0, validate, 40);

AUP_ASSERT_SIZE(aup_audio_format, 8);
AUP_ASSERT_ALIGNMENT(aup_audio_format, 4);
AUP_ASSERT_OFFSET(aup_audio_format, channel_count, 0);
AUP_ASSERT_OFFSET(aup_audio_format, sample_rate, 4);

AUP_ASSERT_SIZE(aup_audio_info, 16);
AUP_ASSERT_ALIGNMENT(aup_audio_info, 8);
AUP_ASSERT_OFFSET(aup_audio_info, format, 0);
AUP_ASSERT_OFFSET(aup_audio_info, sample_count, 8);

AUP_ASSERT_SIZE(aup_audio_track, 48);
AUP_ASSERT_ALIGNMENT(aup_audio_track, 8);
AUP_ASSERT_OFFSET(aup_audio_track, name, 0);
AUP_ASSERT_OFFSET(aup_audio_track, track, 16);
AUP_ASSERT_OFFSET(aup_audio_track, audio, 24);
AUP_ASSERT_OFFSET(aup_audio_track, info, 32);

AUP_ASSERT_SIZE(aup_audio_chunk, 24);
AUP_ASSERT_ALIGNMENT(aup_audio_chunk, 8);
AUP_ASSERT_OFFSET(aup_audio_chunk, channels, 0);
AUP_ASSERT_OFFSET(aup_audio_chunk, channel_count, 8);
AUP_ASSERT_OFFSET(aup_audio_chunk, sample_count, 16);

AUP_ASSERT_SIZE(aup_label_info, 40);
AUP_ASSERT_ALIGNMENT(aup_label_info, 8);
AUP_ASSERT_OFFSET(aup_label_info, label, 0);
AUP_ASSERT_OFFSET(aup_label_info, start_seconds, 8);
AUP_ASSERT_OFFSET(aup_label_info, end_seconds, 16);
AUP_ASSERT_OFFSET(aup_label_info, text, 24);

AUP_ASSERT_SIZE(aup_label_track, 40);
AUP_ASSERT_ALIGNMENT(aup_label_track, 8);
AUP_ASSERT_OFFSET(aup_label_track, name, 0);
AUP_ASSERT_OFFSET(aup_label_track, track, 16);
AUP_ASSERT_OFFSET(aup_label_track, label_count, 24);
AUP_ASSERT_OFFSET(aup_label_track, labels, 32);

AUP_ASSERT_SIZE(aup_effect_offline_args_v0, 48);
AUP_ASSERT_ALIGNMENT(aup_effect_offline_args_v0, 8);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, selection_duration_seconds, 0);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, generator_duration_seconds, 8);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, audio_track_count, 16);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, audio_tracks, 24);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, label_track_count, 32);
AUP_ASSERT_OFFSET(aup_effect_offline_args_v0, label_tracks, 40);

AUP_ASSERT_SIZE(aup_effect_offline_host_v0, 120);
AUP_ASSERT_ALIGNMENT(aup_effect_offline_host_v0, 8);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, context, 0);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, query, 8);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, convert_audio, 16);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, read_audio, 24);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, create_audio, 32);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, write_audio, 40);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, replace_audio_track, 48);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, add_audio_track, 56);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, release_audio, 64);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, create_label_track, 72);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, add_label, 80);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, update_label, 88);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, delete_label, 96);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, progress, 104);
AUP_ASSERT_OFFSET(aup_effect_offline_host_v0, cancelled, 112);

AUP_ASSERT_SIZE(aup_effect_offline_v0, 16);
AUP_ASSERT_ALIGNMENT(aup_effect_offline_v0, 8);
AUP_ASSERT_OFFSET(aup_effect_offline_v0, context, 0);
AUP_ASSERT_OFFSET(aup_effect_offline_v0, process, 8);
