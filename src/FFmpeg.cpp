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

#include "Audacity.h"   // for config*.h
#include "FFmpeg.h"
#include "AudacityApp.h"
#include "FileNames.h"
#include "Internat.h"
#include "widgets/HelpSystem.h"

#include <wx/file.h>

#include "Experimental.h"

#if !defined(USE_FFMPEG)
/// FFmpeg support may or may not be compiled in,
/// but Preferences dialog requires this function nevertheless
wxString GetFFmpegVersion(wxWindow *parent)
{
   return wxString(_("FFmpeg support not compiled in"));
}

#else

/** This pointer to the shared object has global scope and is used to track the
 * singleton object which wraps the FFmpeg codecs */
std::unique_ptr<FFmpegLibs> FFmpegLibsPtr{};
FFmpegLibs *FFmpegLibsInst()
{
   return FFmpegLibsPtr.get();
}

FFmpegLibs *PickFFmpegLibs()
{
   if (FFmpegLibsPtr)
      FFmpegLibsPtr->refcount++;
   else
      FFmpegLibsPtr = std::make_unique<FFmpegLibs>();

   return FFmpegLibsPtr.get();
}

void DropFFmpegLibs()
{
   if (FFmpegLibsPtr)
   {
      FFmpegLibsPtr->refcount--;
      if (FFmpegLibsPtr->refcount == 0)
         FFmpegLibsPtr.reset();
   }
}

bool LoadFFmpeg(bool showerror)
{
   PickFFmpegLibs();
   if (FFmpegLibsInst()->ValidLibsLoaded())
   {
      DropFFmpegLibs();
      return true;
   }
   if (!FFmpegLibsInst()->LoadLibs(NULL, showerror))
   {
      DropFFmpegLibs();
      gPrefs->Write(wxT("/FFmpeg/Enabled"), false);
      gPrefs->Flush();
      return false;
   }
   else
   {
      gPrefs->Write(wxT("/FFmpeg/Enabled"), true);
      gPrefs->Flush();
      return true;
   }
}

/** Called during Audacity start-up to try and load the ffmpeg libraries */
void FFmpegStartup()
{
   bool enabled = false;
   gPrefs->Read(wxT("/FFmpeg/Enabled"),&enabled);
   // 'false' means that no errors should be shown whatsoever
   if (!LoadFFmpeg(false))
   {
      if (enabled)
      {
         wxMessageBox(_("FFmpeg was configured in Preferences and successfully loaded before, \
                        \nbut this time Audacity failed to load it at startup. \
                        \n\nYou may want to go back to Preferences > Libraries and re-configure it."),
                      _("FFmpeg startup failed"));
      }
   }
}

wxString GetFFmpegVersion(wxWindow * WXUNUSED(parent))
{
   PickFFmpegLibs();

   wxString versionString = _("FFmpeg library not found");

   if (FFmpegLibsInst()->ValidLibsLoaded()) {
      versionString = FFmpegLibsInst()->GetLibraryVersion();
   }

   DropFFmpegLibs();

   return versionString;
}

void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl)
{
   //Most of this stuff is taken from FFmpeg tutorials and FFmpeg itself
   int av_log_level = AV_LOG_INFO;
   AVClass* avc = ptr ? *(AVClass**)ptr : NULL;
   if (level > av_log_level)
      return;
   wxString printstring(wxT(""));

   if (avc) {
      printstring.Append(wxString::Format(wxT("[%s @ %p] "), wxString::FromUTF8(avc->item_name(ptr)).c_str(), avc));
   }

   wxString frm(fmt,wxConvLibc);
#if defined(__WXMSW__)
   frm.Replace(wxT("%t"),wxT("%i"),true); //TODO: on Windows vprintf won't handle %t, and probably some others. Investigate.
#endif
#if defined(wxUSE_UNICODE)
   // String comes with %s format field and a value in value list is ascii char*. Thus in Unicode configurations
   // we have to convert %s to %S.
   frm.Replace(wxT("%s"),wxT("%S"),true);
#endif
   printstring.Append(wxString::FormatV(frm,vl));
   wxString cpt;
   switch (level)
   {
   case 0: cpt = wxT("Error"); break;
   case 1: cpt = wxT("Info"); break;
   case 2: cpt = wxT("Debug"); break;
   default: cpt = wxT("Log"); break;
   }
#ifdef EXPERIMENTAL_OD_FFMPEG
//if the decoding happens thru OD then this gets called from a non main thread, which means wxLogDebug
//will crash.
//TODO:find some workaround for the log.  perhaps use ODManager as a bridge. for now just print
   if(!wxThread::IsMain())
      printf("%s: %s\n",(char*)cpt.char_str(),(char*)printstring.char_str());
   else
#endif
      wxLogDebug(wxT("%s: %s"),cpt.c_str(),printstring.c_str());
}

//======================= Unicode aware uri protocol for FFmpeg
// Code inspired from ffmpeg-users mailing list sample

static int ufile_read(void *opaque, uint8_t *buf, int size)
{
   int ret = (int)((wxFile *) opaque)->Read(buf, size);
   return ret;
}

static int ufile_write(void *opaque, uint8_t *buf, int size)
{
   return (int) ((wxFile *) opaque)->Write(buf, size);
}

static int64_t ufile_seek(void *opaque, int64_t pos, int whence)
{
   wxSeekMode mode = wxFromStart;

#if !defined(AVSEEK_FORCE)
#define AVSEEK_FORCE 0
#endif

   switch (whence & ~AVSEEK_FORCE)
   {
   case (SEEK_SET):
      mode = wxFromStart;
      break;
   case (SEEK_CUR):
      mode = wxFromCurrent;
      break;
   case (SEEK_END):
      mode = wxFromEnd;
      break;
   case (AVSEEK_SIZE):
      return ((wxFile *) opaque)->Length();
   }

   return ((wxFile *) opaque)->Seek(pos, mode);
}

int ufile_close(AVIOContext *pb)
{
   std::unique_ptr<wxFile> f{ (wxFile *)pb->opaque };

   if (f)
      f->Close();

   return 0;
}

// Open a file with a (possibly) Unicode filename
int ufile_fopen(AVIOContext **s, const wxString & name, int flags)
{
   wxFile::OpenMode mode;

   auto f = std::make_unique<wxFile>();
   if (!f) {
      return -ENOMEM;
   }

   if (flags == (AVIO_FLAG_READ | AVIO_FLAG_WRITE)) {
      return -EINVAL;
   } else if (flags == AVIO_FLAG_WRITE) {
      mode = wxFile::write;
   } else {
      mode = wxFile::read;
   }

   if (!f->Open(name, mode)) {
      return -ENOENT;
   }

   *s = avio_alloc_context((unsigned char*)av_malloc(32768), 32768,
                           flags & AVIO_FLAG_WRITE,
                           /*opaque*/f.get(),
                           ufile_read,
                           ufile_write,
                           ufile_seek);
   if (!*s) {
      return -ENOMEM;
   }

   f.release(); // s owns the file object now

   return 0;
}


// Detect type of input file and open it if recognized. Routine
// based on the av_open_input_file() libavformat function.
int ufile_fopen_input(std::unique_ptr<FFmpegContext> &context_ptr, wxString & name)
{
   context_ptr.reset();
   auto context = std::make_unique<FFmpegContext>();

   wxFileName ff{ name };
   wxCharBuffer fname;
   const char *filename;
   int err;

   fname = ff.GetFullName().mb_str();
   filename = (const char *) fname;

   // Open the file to prepare for probing
   if ((err = ufile_fopen(&context->pb, name, AVIO_FLAG_READ)) < 0) {
      goto fail;
   }

   context->ic_ptr = avformat_alloc_context();
   context->ic_ptr->pb = context->pb;

   // And finally, attempt to associate an input stream with the file
   err = avformat_open_input(&context->ic_ptr, filename, NULL, NULL);
   if (err) {
      goto fail;
   }

   // success
   context_ptr = std::move(context);
   return 0;

fail:

   return err;
}

FFmpegContext::~FFmpegContext()
{
   if (FFmpegLibsInst()->ValidLibsLoaded())
   {
      if (ic_ptr)
         avformat_close_input(&ic_ptr);
      av_log_set_callback(av_log_default_callback);
   }

   if (pb) {
      ufile_close(pb);
      if (FFmpegLibsInst()->ValidLibsLoaded())
      {
         av_free(pb->buffer);
         av_free(pb);
      }
   }
}

streamContext *import_ffmpeg_read_next_frame(AVFormatContext* formatContext,
                                             streamContext** streams,
                                             unsigned int numStreams)
{
   streamContext *sc = NULL;
   AVPacketEx pkt;

   if (av_read_frame(formatContext, &pkt) < 0)
   {
      return NULL;
   }

   // Find a stream to which this frame belongs
   for (unsigned int i = 0; i < numStreams; i++)
   {
      if (streams[i]->m_stream->index == pkt.stream_index)
         sc = streams[i];
   }

   // Off-stream packet. Don't panic, just skip it.
   // When not all streams are selected for import this will happen very often.
   if (sc == NULL)
   {
      return (streamContext*)1;
   }

   // Copy the frame to the stream context
   sc->m_pkt.create(std::move(pkt));

   sc->m_pktDataPtr = sc->m_pkt->data;
   sc->m_pktRemainingSiz = sc->m_pkt->size;

   return sc;
}

int import_ffmpeg_decode_frame(streamContext *sc, bool flushing)
{
   int      nBytesDecoded;
   wxUint8 *pDecode = sc->m_pktDataPtr;
   int      nDecodeSiz = sc->m_pktRemainingSiz;

   sc->m_frameValid = 0;

   if (flushing)
   {
      // If we're flushing the decoders we don't actually have any NEW data to decode.
      pDecode = NULL;
      nDecodeSiz = 0;
   }
   else
   {
      if (!sc->m_pkt || (sc->m_pktRemainingSiz <= 0))
      {
         //No more data
         return -1;
      }
   }

   AVPacketEx avpkt;
   avpkt.data = pDecode;
   avpkt.size = nDecodeSiz;

   AVFrameHolder frame{ av_frame_alloc() };
   int got_output = 0;

   nBytesDecoded =
      avcodec_decode_audio4(sc->m_codecCtx,
                            frame.get(),                                   // out
                            &got_output,                             // out
                            &avpkt);                                 // in

   if (nBytesDecoded < 0)
   {
      // Decoding failed. Don't stop.
      return -1;
   }

   sc->m_samplefmt = sc->m_codecCtx->sample_fmt;
   sc->m_samplesize = av_get_bytes_per_sample(sc->m_samplefmt);

   int channels = sc->m_codecCtx->channels;
   unsigned int newsize = sc->m_samplesize * frame->nb_samples * channels;
   sc->m_decodedAudioSamplesValidSiz = newsize;
   // Reallocate the audio sample buffer if it's smaller than the frame size.
   if (newsize > sc->m_decodedAudioSamplesSiz )
   {
      // Reallocate a bigger buffer.  But av_realloc is NOT compatible with the returns of av_malloc!
      // So do this:
      sc->m_decodedAudioSamples.reset(static_cast<uint8_t *>(av_malloc(newsize)));
      sc->m_decodedAudioSamplesSiz = newsize;
      if (!sc->m_decodedAudioSamples)
      {
         //Can't allocate bytes
         return -1;
      }
   }
   if (frame->data[1]) {
      for (int i = 0; i<frame->nb_samples; i++) {
         for (int ch = 0; ch<channels; ch++) {
            memcpy(sc->m_decodedAudioSamples.get() + sc->m_samplesize * (ch + channels*i),
                  frame->extended_data[ch] + sc->m_samplesize*i,
                  sc->m_samplesize);
         }
      }
   } else {
      memcpy(sc->m_decodedAudioSamples.get(), frame->data[0], newsize);
   }

   // We may not have read all of the data from this packet. If so, the user can call again.
   // Whether or not they do depends on if m_pktRemainingSiz == 0 (they can check).
   sc->m_pktDataPtr += nBytesDecoded;
   sc->m_pktRemainingSiz -= nBytesDecoded;

   // At this point it's normally safe to assume that we've read some samples. However, the MPEG
   // audio decoder is broken. If this is the case then we just return with m_frameValid == 0
   // but m_pktRemainingSiz perhaps != 0, so the user can call again.
   if (sc->m_decodedAudioSamplesValidSiz > 0)
   {
      sc->m_frameValid = 1;
   }
   return 0;
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

   FindFFmpegDialog(wxWindow *parent, const wxString &path, const wxString &name, const wxString &type)
      :  wxDialogWrapper(parent, wxID_ANY, wxString(_("Locate FFmpeg")))
   {
      SetName(GetTitle());
      ShuttleGui S(this, eIsCreating);

      mPath = path;
      mName = name;
      mType = type;

      mLibPath.Assign(mPath, mName);

      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         text.Printf(_("Audacity needs the file '%s' to import and export audio via FFmpeg."), mName.c_str());
         S.AddTitle(text);

         S.SetBorder(3);
         S.StartHorizontalLay(wxALIGN_LEFT, true);
         {
            text.Printf(_("Location of '%s':"), mName.c_str());
            S.AddTitle(text);
         }
         S.EndHorizontalLay();

         S.StartMultiColumn(2, wxEXPAND);
         S.SetStretchyCol(0);
         {
            if (mLibPath.GetFullPath().IsEmpty()) {
               text.Printf(_("To find '%s', click here -->"), mName.c_str());
               mPathText = S.AddTextBox(wxT(""), text, 0);
            }
            else {
               mPathText = S.AddTextBox(wxT(""), mLibPath.GetFullPath(), 0);
            }
            S.Id(ID_FFMPEG_BROWSE).AddButton(_("Browse..."), wxALIGN_RIGHT);
            S.AddVariableText(_("To get a free copy of FFmpeg, click here -->"), true);
            S.Id(ID_FFMPEG_DLOAD).AddButton(_("Download"), wxALIGN_RIGHT);
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
      wxString question;
      /* i18n-hint: It's asking for the location of a file, for
      example, "Where is lame_enc.dll?" - you could translate
      "Where would I find the file '%s'?" instead if you want. */
      question.Printf(_("Where is '%s'?"), mName.c_str());

      wxString path = FileSelector(question,
         mLibPath.GetPath(),
         mLibPath.GetName(),
         wxT(""),
         mType,
         wxFD_OPEN | wxRESIZE_BORDER,
         this);
      if (!path.IsEmpty()) {
         mLibPath = path;
         mPathText->SetValue(path);
      }
   }

   void OnDownload(wxCommandEvent & WXUNUSED(event))
   {
      HelpSystem::ShowHelpDialog(this, wxT("FAQ:Installation_and_Plug-Ins#ffdown"));
   }

   wxString GetLibPath()
   {
      return mLibPath.GetFullPath();
   }

private:

   wxFileName mLibPath;

   wxString mPath;
   wxString mName;
   wxString mType;

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

BEGIN_EVENT_TABLE(FFmpegNotFoundDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, FFmpegNotFoundDialog::OnOk)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegLibs
//----------------------------------------------------------------------------

FFmpegLibs::FFmpegLibs()
{
   mLibsLoaded = false;
   refcount = 1;
   if (gPrefs) {
      mLibAVFormatPath = gPrefs->Read(wxT("/FFmpeg/FFmpegLibPath"), wxT(""));
   }

}

FFmpegLibs::~FFmpegLibs()
{
   FreeLibs();
};

bool FFmpegLibs::FindLibs(wxWindow *parent)
{
   wxString path;
   wxString name;

   wxLogMessage(wxT("Looking for FFmpeg libraries..."));
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath ('%s') is not empty."), mLibAVFormatPath.c_str());
      const wxFileName fn{ mLibAVFormatPath };
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibAVFormatPath();
      name = GetLibAVFormatName();
      wxLogMessage(wxT("mLibAVFormatPath is empty, starting with path '%s', name '%s'."),
                  path.c_str(), name.c_str());
   }

   FindFFmpegDialog fd(parent,
                        path,
                        name,
                        GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      wxLogMessage(wxT("User canceled the dialog. Failed to find FFmpeg libraries."));
      return false;
   }

   path = fd.GetLibPath();

   wxLogMessage(wxT("User-specified path = '%s'"), path.c_str());
   if (!::wxFileExists(path)) {
      wxLogError(wxT("User-specified file does not exist. Failed to find FFmpeg libraries."));
      return false;
   }
   wxLogMessage(wxT("User-specified FFmpeg file exists. Success."));
   mLibAVFormatPath = path;
   gPrefs->Write(wxT("/FFmpeg/FFmpegLibPath"), mLibAVFormatPath);
   gPrefs->Flush();

   return true;
}

bool FFmpegLibs::LoadLibs(wxWindow * WXUNUSED(parent), bool showerr)
{
#if defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
   mLibsLoaded = InitLibs(wxEmptyString, showerr);
   return mLibsLoaded;
#endif

   wxLogMessage(wxT("Trying to load FFmpeg libraries..."));
   if (ValidLibsLoaded()) {
      wxLogMessage(wxT("FFmpeg libraries are already loaded."));
      FreeLibs();
   }

   // First try loading it from a previously located path
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogMessage(wxT("mLibAVFormatPath ('%s') is not empty. Loading from it."),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If not successful, try loading it from default path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      const wxFileName fn{ GetLibAVFormatPath(), GetLibAVFormatName() };
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from default path, '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

#if defined(__WXMAC__)
   // If not successful, try loading it from legacy path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      const wxFileName fn{wxT("/usr/local/lib/audacity"), GetLibAVFormatName()};
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from legacy path, '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }
#endif

   // If not successful, try loading using system search paths
   if (!ValidLibsLoaded()) {
      wxString path = GetLibAVFormatName();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from system paths. File name is '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

   // If libraries aren't loaded - nag user about that
   /*
   if (!ValidLibsLoaded())
   {
      wxLogError(wxT("Failed to load libraries altogether."));
      int dontShowDlg;
      gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
      if ((dontShowDlg == 0) && (showerr))
         FFmpegNotFoundDialog{nullptr}.ShowModal();
   }
   */
   // Oh well, just give up
   if (!ValidLibsLoaded()) {
      wxString msg = _("Failed to find compatible FFmpeg libraries.");
      if (showerr)
         wxMessageBox(msg);
      wxLogError(msg);
      return false;
   }

   wxLogMessage(wxT("FFmpeg libraries loaded successfully."));
   return true;
}

bool FFmpegLibs::ValidLibsLoaded()
{
   return mLibsLoaded;
}

bool FFmpegLibs::InitLibs(const wxString &libpath_format, bool WXUNUSED(showerr))
{
#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
   FreeLibs();

#if defined(__WXMSW__)
   wxString syspath;
   bool pathfix = false;

   wxLogMessage(wxT("Looking up PATH environment variable..."));
   // First take PATH environment variable and store its content.
   if (wxGetEnv(wxT("PATH"),&syspath))
   {
      wxLogMessage(wxT("PATH = '%s'"), syspath.c_str());
      const wxString &fmtdir{ wxPathOnly(libpath_format) };
      wxString fmtdirsc = fmtdir + wxT(";");
      wxString scfmtdir = wxT(";") + fmtdir;
      wxLogMessage(wxT("Checking that '%s' is in PATH..."), fmtdir.c_str());
      // If the directory, where libavformat is, is not in PATH - add it
      if (!syspath.Contains(fmtdirsc) && !syspath.Contains(scfmtdir) && !syspath.Contains(fmtdir))
      {
         wxLogWarning(wxT("FFmpeg directory '%s' is not in PATH."), fmtdir.c_str());
         if (syspath.Left(1) == wxT(';'))
         {
            wxLogMessage(wxT("Temporarily preending '%s' to PATH..."), fmtdir.c_str());
            syspath.Prepend(scfmtdir);
         }
         else
         {
            wxLogMessage(wxT("Temporarily prepending '%s' to PATH..."), scfmtdir.c_str());
            syspath.Prepend(fmtdirsc);
         }

         if (wxSetEnv(wxT("PATH"),syspath.c_str()))
            // Remember to change PATH back to normal after we're done
            pathfix = true;
         else
            wxLogSysError(wxT("Setting PATH via wxSetEnv('%s') failed."),syspath.c_str());
      }
      else
      {
         wxLogMessage(wxT("FFmpeg directory is in PATH."));
      }
   }
   else
   {
      wxLogSysError(wxT("PATH does not exist."));
   }
#endif

   //Load libavformat
   // Initially we don't know where are the avcodec and avutl libs
   wxDynamicLibrary *codec = NULL;
   wxDynamicLibrary *util = NULL;
   wxFileName avcodec_filename;
   wxFileName avutil_filename;
   wxFileName name{ libpath_format };
   wxString nameFull{name.GetFullPath()};
   bool gotError = false;

   // Check for a monolithic avformat
   avformat = std::make_unique<wxDynamicLibrary>();
   wxLogMessage(wxT("Checking for monolithic avformat from '%s'."), nameFull.c_str());
   gotError = !avformat->Load(nameFull, wxDL_LAZY);

   // Verify it really is monolithic
   if (!gotError) {
      avutil_filename = FileNames::PathFromAddr(avformat->GetSymbol(wxT("avutil_version")));
      avcodec_filename = FileNames::PathFromAddr(avformat->GetSymbol(wxT("avcodec_version")));
      if (avutil_filename.GetFullPath().IsSameAs(nameFull)) {
         if (avcodec_filename.GetFullPath().IsSameAs(nameFull)) {
            util = avformat.get();
            codec = avformat.get();
         }
      }
      if (!avcodec_filename.FileExists()) {
         avcodec_filename = GetLibAVCodecName();
      }
      if (!avutil_filename.FileExists()) {
         avutil_filename = GetLibAVUtilName();
      }

      if (util == NULL || codec == NULL) {
         wxLogMessage(wxT("avformat not monolithic"));
         avformat->Unload();
         util = NULL;
         codec = NULL;
      }
      else {
         wxLogMessage(wxT("avformat is monolithic"));
      }
   }

   // The two wxFileNames don't change after this
   const wxString avcodec_filename_full{ avcodec_filename.GetFullPath() };
   const wxString avutil_filename_full{ avutil_filename.GetFullPath() };

   if (!util) {
      util = (avutil = std::make_unique<wxDynamicLibrary>()).get();
      wxLogMessage(wxT("Loading avutil from '%s'."), avutil_filename_full.c_str());
      util->Load(avutil_filename_full, wxDL_LAZY);
   }

   if (!codec) {
      codec = (avcodec = std::make_unique<wxDynamicLibrary>()).get();
      wxLogMessage(wxT("Loading avcodec from '%s'."), avcodec_filename_full.c_str());
      codec->Load(avcodec_filename_full, wxDL_LAZY);
   }

   if (!avformat->IsLoaded()) {
      name.SetFullName(libpath_format);
      nameFull = name.GetFullPath();
      wxLogMessage(wxT("Loading avformat from '%s'."), nameFull.c_str());
      gotError = !avformat->Load(nameFull, wxDL_LAZY);
   }

#if defined(__WXMSW__)
   //Return PATH to normal
   if ( pathfix )
   {
      wxString oldpath = syspath.BeforeLast(wxT(';'));
      wxLogMessage(wxT("Returning PATH to previous setting..."));
      wxSetEnv(wxT("PATH"),oldpath.c_str());
   }
#endif

   if (gotError) {
      wxLogError(wxT("Failed to load FFmpeg libraries."));
      FreeLibs();
      return false;
   }

   // Show the actual libraries loaded
   if (avutil) {
      wxLogMessage(wxT("Actual avutil path %s"),
                 FileNames::PathFromAddr(avutil->GetSymbol(wxT("avutil_version"))).c_str());
   }
   if (avcodec) {
      wxLogMessage(wxT("Actual avcodec path %s"),
                 FileNames::PathFromAddr(avcodec->GetSymbol(wxT("avcodec_version"))).c_str());
   }
   if (avformat) {
      wxLogMessage(wxT("Actual avformat path %s"),
                 FileNames::PathFromAddr(avformat->GetSymbol(wxT("avformat_version"))).c_str());
   }

   wxLogMessage(wxT("Importing symbols..."));
   FFMPEG_INITDYN(avformat, av_register_all);
   FFMPEG_INITDYN(avformat, avformat_find_stream_info);
   FFMPEG_INITDYN(avformat, av_read_frame);
   FFMPEG_INITDYN(avformat, av_seek_frame);
   FFMPEG_INITDYN(avformat, avformat_close_input);
   FFMPEG_INITDYN(avformat, avformat_write_header);
   FFMPEG_INITDYN(avformat, av_interleaved_write_frame);
   FFMPEG_INITDYN(avformat, av_oformat_next);
   FFMPEG_INITDYN(avformat, avformat_new_stream);
   FFMPEG_INITDYN(avformat, avformat_alloc_context);
   FFMPEG_INITDYN(avformat, av_write_trailer);
   FFMPEG_INITDYN(avformat, av_codec_get_tag);
   FFMPEG_INITDYN(avformat, avformat_version);
   FFMPEG_INITDYN(avformat, avformat_open_input);
   FFMPEG_INITDYN(avformat, avio_size);
   FFMPEG_INITDYN(avformat, avio_alloc_context);
   FFMPEG_INITALT(avformat, av_guess_format, avformat, guess_format);
   FFMPEG_INITDYN(avformat, avformat_free_context);

   FFMPEG_INITDYN(avcodec, av_init_packet);
   FFMPEG_INITDYN(avcodec, av_free_packet);
   FFMPEG_INITDYN(avcodec, avcodec_find_encoder);
   FFMPEG_INITDYN(avcodec, avcodec_find_encoder_by_name);
   FFMPEG_INITDYN(avcodec, avcodec_find_decoder);
   FFMPEG_INITDYN(avcodec, avcodec_open2);
   FFMPEG_INITDYN(avcodec, avcodec_decode_audio4);
   FFMPEG_INITDYN(avcodec, avcodec_encode_audio2);
   FFMPEG_INITDYN(avcodec, avcodec_close);
   FFMPEG_INITDYN(avcodec, avcodec_register_all);
   FFMPEG_INITDYN(avcodec, avcodec_version);
   FFMPEG_INITDYN(avcodec, av_codec_next);
   FFMPEG_INITDYN(avcodec, av_codec_is_encoder);
   FFMPEG_INITDYN(avcodec, avcodec_fill_audio_frame);

   FFMPEG_INITDYN(avutil, av_free);
   FFMPEG_INITDYN(avutil, av_dict_free);
   FFMPEG_INITDYN(avutil, av_dict_get);
   FFMPEG_INITDYN(avutil, av_dict_set);
   FFMPEG_INITDYN(avutil, av_get_bytes_per_sample);
   FFMPEG_INITDYN(avutil, av_log_set_callback);
   FFMPEG_INITDYN(avutil, av_log_default_callback);
   FFMPEG_INITDYN(avutil, av_fifo_alloc);
   FFMPEG_INITDYN(avutil, av_fifo_generic_read);
   FFMPEG_INITDYN(avutil, av_fifo_realloc2);
   FFMPEG_INITDYN(avutil, av_fifo_free);
   FFMPEG_INITDYN(avutil, av_fifo_size);
   FFMPEG_INITDYN(avutil, av_malloc);
   FFMPEG_INITDYN(avutil, av_fifo_generic_write);
   // FFMPEG_INITDYN(avutil, av_freep);
   FFMPEG_INITDYN(avutil, av_rescale_q);
   FFMPEG_INITDYN(avutil, avutil_version);
   FFMPEG_INITALT(avutil, av_frame_alloc, avcodec, avcodec_alloc_frame);
   FFMPEG_INITALT(avutil, av_frame_free, avcodec, avcodec_free_frame);
   FFMPEG_INITDYN(avutil, av_samples_get_buffer_size);

   wxLogMessage(wxT("All symbols loaded successfully. Initializing the library."));
#endif

   //FFmpeg initialization
   avcodec_register_all();
   av_register_all();

   wxLogMessage(wxT("Retrieving FFmpeg library version numbers:"));
   int avfver = avformat_version();
   int avcver = avcodec_version();
   int avuver = avutil_version();
   mAVCodecVersion = wxString::Format(wxT("%d.%d.%d"),avcver >> 16 & 0xFF, avcver >> 8 & 0xFF, avcver & 0xFF);
   mAVFormatVersion = wxString::Format(wxT("%d.%d.%d"),avfver >> 16 & 0xFF, avfver >> 8 & 0xFF, avfver & 0xFF);
   mAVUtilVersion = wxString::Format(wxT("%d.%d.%d"),avuver >> 16 & 0xFF, avuver >> 8 & 0xFF, avuver & 0xFF);

   wxLogMessage(wxT("   AVCodec version 0x%06x - %s (built against 0x%06x - %s)"),
                  avcver, mAVCodecVersion.c_str(), LIBAVCODEC_VERSION_INT,
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVCODEC_VERSION)).c_str());
   wxLogMessage(wxT("   AVFormat version 0x%06x - %s (built against 0x%06x - %s)"),
                  avfver, mAVFormatVersion.c_str(), LIBAVFORMAT_VERSION_INT,
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVFORMAT_VERSION)).c_str());
   wxLogMessage(wxT("   AVUtil version 0x%06x - %s (built against 0x%06x - %s)"),
                  avuver,mAVUtilVersion.c_str(), LIBAVUTIL_VERSION_INT,
                  wxString::FromUTF8(AV_STRINGIFY(LIBAVUTIL_VERSION)).c_str());

   int avcverdiff = (avcver >> 16 & 0xFF) - (int)(LIBAVCODEC_VERSION_MAJOR);
   int avfverdiff = (avfver >> 16 & 0xFF) - (int)(LIBAVFORMAT_VERSION_MAJOR);
   int avuverdiff = (avuver >> 16 & 0xFF) - (int)(LIBAVUTIL_VERSION_MAJOR);
   if (avcverdiff != 0)
      wxLogError(wxT("AVCodec version mismatch = %d"), avcverdiff);
   if (avfverdiff != 0)
      wxLogError(wxT("AVFormat version mismatch = %d"), avfverdiff);
   if (avuverdiff != 0)
      wxLogError(wxT("AVUtil version mismatch = %d"), avuverdiff);
   //make sure that header and library major versions are the same
   if (avcverdiff != 0 || avfverdiff != 0 || avuverdiff != 0)
   {
      wxLogError(wxT("Version mismatch. FFmpeg libraries are unusable."));
      return false;
   }

   return true;
}

void FFmpegLibs::FreeLibs()
{
   avformat.reset();
   avcodec.reset();
   avutil.reset();
   mLibsLoaded = false;
   return;
}

#endif //USE_FFMPEG
