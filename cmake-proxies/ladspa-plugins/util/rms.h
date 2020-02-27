#ifndef _RMS_H
#define _RMS_H

#include <math.h>

#define RMS_BUF_SIZE 64

typedef struct {
	float        buffer[RMS_BUF_SIZE];
	unsigned int pos;
	float        sum;
} rms_env;

rms_env *rms_env_new();

static inline float rms_env_process(rms_env *r, float x);

void rms_env_reset(rms_env *r);

void rms_env_free(rms_env *r);

inline static float rms_env_process(rms_env *r, const float x)
{
	r->sum -= r->buffer[r->pos];
	r->sum += x;
	if (r->sum < 1.0e-6) {
		r->sum = 0.0f;
	}
	r->buffer[r->pos] = x;
	r->pos = (r->pos + 1) & (RMS_BUF_SIZE - 1);

	return sqrt(r->sum / (float)RMS_BUF_SIZE);
}

#endif
