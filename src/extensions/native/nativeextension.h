/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 */
#pragma once

#include <stdbool.h>
#include <stdint.h>

#define EXT_STATUS_OK ((int32_t)0)
#define EXT_STATUS_ERROR ((int32_t)1)
#define EXT_STATUS_UNKNOWN_CALL ((int32_t)2)
#define EXT_STATUS_INVALID_ARGUMENT ((int32_t)3)

#define EXT_VALUE_NONE ((uint32_t)0)
#define EXT_VALUE_BOOL ((uint32_t)1)
#define EXT_VALUE_NUMBER ((uint32_t)2)
#define EXT_VALUE_STRING ((uint32_t)3)
#define EXT_VALUE_OBJECT ((uint32_t)4)
#define EXT_VALUE_BUFFER ((uint32_t)5)

#if defined(_WIN32)
#define EXT_EXPORT __declspec(dllexport)
#else
#define EXT_EXPORT __attribute__((visibility("default")))
#endif

#pragma pack(push, 8)

typedef struct ext_buffer {
    void* data;
    // Bytes
    uint64_t size;
} ext_buffer;

typedef struct ext_value {
    uint32_t type;
    union {
        bool as_bool;
        double as_number;
        const char* as_string;
        void* as_object;
        ext_buffer as_buffer;
    };
} ext_value;

#pragma pack(pop)

// Input strings and buffers live for the call
// Returned strings live until the next call on this thread
typedef int32_t (* ext_dispatch_fn)(const char* call, const ext_value* args, uint32_t arg_count, ext_value* result);

#ifdef __cplusplus
extern "C" {
#endif

EXT_EXPORT int32_t extension_dispatch_v0(const char* call, const ext_value* args, uint32_t arg_count, ext_value* result);

#ifdef __cplusplus
}
#endif
