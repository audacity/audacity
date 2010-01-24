#ifndef CONVERT_H
#define CONVERT_H

#include "sbsms.h"

typedef bool (*progress_cb)(int progress, const char *msg, void *data);

bool sbsms_convert(const char *filenameIn, const char *filenameOut, bool bAnalyzeOnly, bool bSynthesizeOnly, bool bPreAnalyze, int quality, progress_cb progressCB, void *data, real stretch0, real stretch1, real ratio0, real ratio1, real volume);

#endif 
