/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2 or later.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/

// Store function pointers here when including FFmpeg.h
#define DEFINE_FFMPEG_POINTERS

#include "FFmpeg.h"
#include "lib-ffmpeg-support/FFmpegFunctions.h"
#include "ModuleConstants.h"

#include <wx/log.h>

static BoolSetting FFmpegEnabled{ L"/FFmpeg/Enabled", false };

bool LoadFFmpeg(bool showerror)
{
    auto ffmpeg = FFmpegFunctions::Load();

    if (!ffmpeg) {
        FFmpegEnabled.Write(false);
        gPrefs->Flush();
        return false;
    } else {
        FFmpegEnabled.Write(true);
        gPrefs->Flush();
        return true;
    }
}

/** Called during Audacity start-up to try and load the ffmpeg libraries */
void FFmpegStartup()
{
    bool enabled = FFmpegEnabled.Read();
    // 'false' means that no errors should be shown whatsoever
    if (!LoadFFmpeg(false)) {
        //! TODO AU4: display message about FFmpeg not found, here's original
        //! message (already translated)
        //!       XO(
        //! "FFmpeg was configured in Preferences and successfully loaded before, \
        //          \nbut this time Audacity failed to load it at startup. \
        //          \n\nYou may want to go back to Preferences > Libraries and re-configure it."),
        //!             XO("FFmpeg startup failed")
    }
}

TranslatableString GetFFmpegVersion()
{
    auto ffmpeg = FFmpegFunctions::Load();

    if (ffmpeg) {
        return Verbatim(
            wxString::Format(
                wxT("F(%d.%d.%d),C(%d.%d.%d),U(%d.%d.%d)"),
                ffmpeg->AVFormatVersion.Major, ffmpeg->AVFormatVersion.Minor, ffmpeg->AVFormatVersion.Micro,
                ffmpeg->AVCodecVersion.Major, ffmpeg->AVCodecVersion.Minor, ffmpeg->AVCodecVersion.Micro,
                ffmpeg->AVUtilVersion.Major, ffmpeg->AVUtilVersion.Minor, ffmpeg->AVUtilVersion.Micro
                ));
    }

    return XO("FFmpeg library not found");
}

bool FindFFmpegLibs()
{
    wxString path;

#if defined(__WXMSW__)
    const wxString name = wxT("avformat.dll");
#elif defined(__WXMAC__)
    const wxString name = wxT("libavformat.dylib");
#else
    const wxString name = wxT("libavformat.so");
#endif

    wxLogMessage(wxT("Looking for FFmpeg libraries..."));

    auto searchPaths = FFmpegFunctions::GetSearchPaths(false);

    if (!searchPaths.empty()) {
        path = searchPaths.front();
    }

    //! TODO AU4: Get user path from FFmpeg dialog

    const wxFileName fileName(path);
    if (fileName.FileExists()) {
        path = fileName.GetPath();
    }

    wxLogMessage(wxT("User-specified path = '%s'"), path);

    SettingTransaction transaction;
    AVFormatPath.Write(path);

    // Try to load FFmpeg from the user provided path
    if (!FFmpegFunctions::Load(true)) {
        wxLogError(wxT("User-specified path does not contain FFmpeg libraries."));
        return false;
    }

    transaction.Commit();

    wxLogMessage(wxT("User-specified FFmpeg file exists. Success."));

    return true;
}

BoolSetting FFmpegNotFoundDontShow{ L"/FFmpeg/NotFoundDontShow", false };

DEFINE_VERSION_CHECK

extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
    if (type == ModuleInitialize) {
        FFmpegStartup();
    }
    return 1;
}
