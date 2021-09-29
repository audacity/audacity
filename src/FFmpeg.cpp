/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/

// Store function pointers here when including FFmpeg.h
#define DEFINE_FFMPEG_POINTERS


#include "FFmpeg.h"

#include "FileNames.h"
#include "SelectFile.h"
#include "widgets/HelpSystem.h"
#include "widgets/AudacityMessageBox.h"

#include <wx/checkbox.h>
#include <wx/dynlib.h>
#include <wx/file.h>
#include <wx/log.h>
#include <wx/textctrl.h>

#if !defined(USE_FFMPEG)
/// FFmpeg support may or may not be compiled in,
/// but Preferences dialog requires this function nevertheless
TranslatableString GetFFmpegVersion()
{
   return XO("FFmpeg support not compiled in");
}

#else

static BoolSetting FFmpegEnabled{ L"/FFmpeg/Enabled", false };

bool LoadFFmpeg(bool showerror)
{
   auto ffmpeg = FFmpegFunctions::Load();

   if (!ffmpeg)
   {
      FFmpegEnabled.Write(false);
      gPrefs->Flush();
      return false;
   }
   else
   {
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
   if (!LoadFFmpeg(false))
   {
      if (enabled)
      {
         AudacityMessageBox(XO(
"FFmpeg was configured in Preferences and successfully loaded before, \
\nbut this time Audacity failed to load it at startup. \
\n\nYou may want to go back to Preferences > Libraries and re-configure it."),
            XO("FFmpeg startup failed"));
      }
   }
}

TranslatableString GetFFmpegVersion()
{
   auto ffmpeg = FFmpegFunctions::Load();

   if (ffmpeg)
   {
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

/*******************************************************/

class FFmpegNotFoundDialog;

//----------------------------------------------------------------------------
// FindFFmpegDialog
//----------------------------------------------------------------------------

#define ID_FFMPEG_BROWSE 5000
#define ID_FFMPEG_DLOAD  5001

/// Allows user to locate libav* libraries
class FindFFmpegDialog final : public wxDialogWrapper
{
public:

   FindFFmpegDialog(wxWindow *parent, const wxString &path, const wxString &name)
       : wxDialogWrapper(parent, wxID_ANY, XO("Locate FFmpeg"))
       , mName(name)
       , mFullPath(path, name)
   {
      SetName();

      ShuttleGui S(this, eIsCreating);
      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         S.AddTitle(
            XO(
"Audacity needs the file '%s' to import and export audio via FFmpeg.")
               .Format( mName ) );

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            S.AddTitle( XO("Location of '%s':").Format( mName ) );
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mFullPath.GetFullPath().empty())
            {
               mPathText = S.AddTextBox(
                  {},
                  XO("To find '%s', click here -->")
                     .Format(mName)
                     .Translation(),
                  0);
            }
            else
            {
               mPathText = S.AddTextBox({}, mFullPath.GetFullPath(), 0);
            }

            S.Id(ID_FFMPEG_BROWSE).AddButton(XXO("Browse..."), wxALIGN_RIGHT);
            S.AddVariableText(
               XO("To get a free copy of FFmpeg, click here -->"), true);
            S.Id(ID_FFMPEG_DLOAD).AddButton(XXO("Download"), wxALIGN_RIGHT);
         }
         S.EndMultiColumn();

         S.AddStandardButtons();
      }
      S.EndVerticalLay();

      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnBrowse(wxCommandEvent & WXUNUSED(event))
   {
      static const FileNames::FileTypes types = {
#   if defined(__WXMSW__)
         { XO("Only avformat.dll"), { wxT("avformat-*.dll") } },
#   elif defined(__WXMAC__)
         { XO("Only ffmpeg.*.dylib"), { wxT("ffmpeg.*.dylib") } },
#   else
         { XO("Only libavformat.so"), { wxT("libavformat.so.*") } },
#   endif
         FileNames::DynamicLibraries,
         FileNames::AllFiles
      };

      UpdatePath();

      /* i18n-hint: It's asking for the location of a file, for
      example, "Where is lame_enc.dll?" - you could translate
      "Where would I find the file '%s'?" instead if you want. */
      auto question = XO("Where is '%s'?").Format( mName );

      wxString path = SelectFile(
         FileNames::Operation::_None,
         question,
         mFullPath.GetPath(),
         mFullPath.GetFullName(),
         wxT(""),
         types,
         wxFD_OPEN | wxRESIZE_BORDER,
         this);

      if (!path.empty())
      {
         mFullPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & WXUNUSED(event))
   {
      HelpSystem::ShowHelp(this, L"FAQ:Installing_the_FFmpeg_Import_Export_Library");
   }

   void UpdatePath()
   {
      mFullPath = mPathText->GetValue();
   }

   wxString GetLibPath()
   {
      UpdatePath();
      return mFullPath.GetFullPath();
   }

private:
   wxString mName;
   wxFileName mFullPath;

   wxTextCtrl *mPathText;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(FindFFmpegDialog, wxDialogWrapper)
   EVT_BUTTON(ID_FFMPEG_BROWSE, FindFFmpegDialog::OnBrowse)
   EVT_BUTTON(ID_FFMPEG_DLOAD,  FindFFmpegDialog::OnDownload)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegNotFoundDialog
//----------------------------------------------------------------------------

FFmpegNotFoundDialog::FFmpegNotFoundDialog(wxWindow *parent)
   :  wxDialogWrapper(parent, wxID_ANY, XO("FFmpeg not found"))
{
   SetName();
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

void FFmpegNotFoundDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxString text;

   S.SetBorder(10);
   S.StartVerticalLay(true);
   {
      S.AddFixedText(XO(
"Audacity attempted to use FFmpeg to import an audio file,\n\
but the libraries were not found.\n\n\
To use FFmpeg import, go to Edit > Preferences > Libraries\n\
to download or locate the FFmpeg libraries."
      ));

      mDontShow = S
         .AddCheckBox(XXO("Do not show this warning again"),
            FFmpegNotFoundDontShow.Read() );

      S.AddStandardButtons(eOkButton);
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   return;
}

void FFmpegNotFoundDialog::OnOk(wxCommandEvent & WXUNUSED(event))
{
   if (mDontShow->GetValue())
   {
      FFmpegNotFoundDontShow.Write(true);
      gPrefs->Flush();
   }
   this->EndModal(0);
}

BEGIN_EVENT_TABLE(FFmpegNotFoundDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, FFmpegNotFoundDialog::OnOk)
END_EVENT_TABLE()


bool FindFFmpegLibs(wxWindow* parent)
{
   wxString path;

#if defined(__WXMSW__)
   const wxString name = wxT("avformat-*.dll");
#elif defined(__WXMAC__)
   const wxString name = wxT("ffmpeg.*.64bit.dylib");
#else
   const wxString name = wxT("libavformat.so.*");
#endif

   wxLogMessage(wxT("Looking for FFmpeg libraries..."));

   auto searchPaths = FFmpegFunctions::GetSearchPaths();

   if (!searchPaths.empty())
      path = searchPaths.front();

   FindFFmpegDialog fd(parent, path, name);

   if (fd.ShowModal() == wxID_CANCEL) {
      wxLogMessage(wxT("User canceled the dialog. Failed to find FFmpeg libraries."));
      return false;
   }

   path = fd.GetLibPath();

   wxLogMessage(wxT("User-specified path = '%s'"), path);

   if (!::wxFileExists(path)) {
      wxLogError(wxT("User-specified file does not exist. Failed to find FFmpeg libraries."));
      return false;
   }

   wxLogMessage(wxT("User-specified FFmpeg file exists. Success."));

   AVFormatPath.Write(path);
   gPrefs->Flush();

   return true;
}

BoolSetting FFmpegNotFoundDontShow{ L"/FFmpeg/NotFoundDontShow", false };

#endif //USE_FFMPEG
