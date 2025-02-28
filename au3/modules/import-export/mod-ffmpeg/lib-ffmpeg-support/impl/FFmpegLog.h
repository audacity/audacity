/**********************************************************************

  Audacity: A Digital Audio Editor

  FFmpegLog.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdarg>

#include <wx/string.h>

typedef struct AVClass AVClass;

struct FFmpegFunctions;

class FFmpegLog
{
public:
    virtual ~FFmpegLog() = default;
};
