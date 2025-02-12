/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.h

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2 or later.  See License.txt.

******************************************************************//**

Describes shared object that is used to access FFmpeg libraries.

*//*******************************************************************/

#if !defined(__AUDACITY_FFMPEG__)
#define __AUDACITY_FFMPEG__

#include "Prefs.h"

TranslatableString GetFFmpegVersion();

//----------------------------------------------------------------------------
// Attempt to load and enable/disable FFmpeg at startup
//----------------------------------------------------------------------------
void FFmpegStartup();

bool LoadFFmpeg(bool showerror);

bool FindFFmpegLibs();

//! TODO AU4:
//! bool FindFFmpegLibs(); - should open a window and allow to select FFmpeg path manually
//! implement NotFoundDialog

extern BoolSetting FFmpegNotFoundDontShow;

#endif // USE_FFMPEG
