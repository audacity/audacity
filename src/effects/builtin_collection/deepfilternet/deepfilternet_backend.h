#pragma once

#include <cstddef>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct dfn_handle dfn_handle;

dfn_handle* dfn_create_default(unsigned channels, float attenuation_limit_db);
void dfn_destroy(dfn_handle* handle);

unsigned dfn_sample_rate(const dfn_handle* handle);
std::size_t dfn_frame_length(const dfn_handle* handle);
std::size_t dfn_delay_samples(const dfn_handle* handle);

int dfn_set_attenuation_limit(dfn_handle* handle, float attenuation_limit_db);
int dfn_set_post_filter_beta(dfn_handle* handle, float beta);
int dfn_reset(dfn_handle* handle);

int dfn_process_frame(dfn_handle* handle, const float* input, float* output);
const char* dfn_last_error();

#ifdef __cplusplus
}
#endif
