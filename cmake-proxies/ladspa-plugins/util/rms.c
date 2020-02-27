#include <stdlib.h>
#include "rms.h"

rms_env *rms_env_new()
{
	rms_env *new = (rms_env *)calloc(1, sizeof(rms_env));

	return new;
}

void rms_env_reset(rms_env *r)
{
	unsigned int i;

	for (i=0; i<RMS_BUF_SIZE; i++) {
		r->buffer[i] = 0.0f;
	}
	r->pos = 0;
	r->sum = 0.0f;
}

void rms_env_free(rms_env *r)
{
	free(r);
}
