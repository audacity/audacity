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

#include "wxPanelWrapper.h" // to inherit
#include "Prefs.h"

class wxCheckBox;
class ShuttleGui;

TranslatableString GetFFmpegVersion();

//----------------------------------------------------------------------------
// Attempt to load and enable/disable FFmpeg at startup
//----------------------------------------------------------------------------
void FFmpegStartup();

bool LoadFFmpeg(bool showerror);

bool FindFFmpegLibs(wxWindow* parent = nullptr);

/// If Audacity failed to load libav*, this dialog
/// shows up and tells user about that. It will pop-up
/// again and again until it is disabled.
class FFmpegNotFoundDialog final : public wxDialogWrapper
{
public:

   FFmpegNotFoundDialog(wxWindow *parent);

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & WXUNUSED(event));

private:

   wxCheckBox *mDontShow;

   DECLARE_EVENT_TABLE()
};

extern BoolSetting FFmpegNotFoundDontShow;

#endif // USE_FFMPEG

