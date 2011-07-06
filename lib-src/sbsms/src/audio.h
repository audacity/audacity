#ifndef SBSMS_AUDIO_H
#define SBSMS_AUDIO_H

#include "sbsms.h"

namespace _sbsms_ {

audio *make_audio_buf(long);
void free_audio_buf(audio *);
long copy_audio_buf(audio *, long off1, audio *, long off2, long n);
long audio_convert_from(float *to, long off1, audio *from, long off2, long n);
long audio_convert_to(audio *to, long off1, float *from, long off2, long n);

}

#endif
