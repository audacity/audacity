/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.cpp

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

\class FFmpegLibs
\brief Class used to dynamically load FFmpeg libraries

*//*******************************************************************/



#include "Audacity.h"	// for config*.h
#include "FFmpeg.h"
#include "AudacityApp.h"

#include <wx/file.h>

#ifdef _DEBUG
   #ifdef _MSC_VER
      #undef THIS_FILE
      static char*THIS_FILE= __FILE__;
      #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
   #endif
#endif

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
FFmpegLibs *FFmpegLibsInst = NULL;

FFmpegLibs *PickFFmpegLibs()
{
   if (FFmpegLibsInst != NULL)
   {
      FFmpegLibsInst->refcount++;
      return FFmpegLibsInst;
   }
   else
   {
      FFmpegLibsInst = new FFmpegLibs();
      return FFmpegLibsInst;
   }
}

void DropFFmpegLibs()
{
   if (FFmpegLibsInst != NULL)
   {
      FFmpegLibsInst->refcount--;
      if (FFmpegLibsInst->refcount == 0)
      {
         delete FFmpegLibsInst;
         FFmpegLibsInst = NULL;
      }
   }
}

bool LoadFFmpeg(bool showerror)
{
   PickFFmpegLibs();
   if (FFmpegLibsInst->ValidLibsLoaded())
   {
     DropFFmpegLibs();
     return true;
   }
   if (!FFmpegLibsInst->LoadLibs(NULL,showerror))
   {
      DropFFmpegLibs();
      gPrefs->Write(wxT("/FFmpeg/Enabled"), false);
      return false;
   }
   else
   {
      gPrefs->Write(wxT("/FFmpeg/Enabled"), true);
      return true;
   }
}

/** Called during Audacity start-up to try and load the ffmpeg libraries */
void FFmpegStartup()
{
   bool enabled = false;
   gPrefs->Read(wxT("/FFmpeg/Enabled"),&enabled);
   // 'false' means that no errors should be shown whatsoever
   if (enabled && !LoadFFmpeg(false))
   {
     wxMessageBox(_("FFmpeg was configured in Preferences and successfully loaded before, \
                      \nbut this time Audacity failed to load it at startup. \
                      \n\nYou may want to go back to Preferences > Libraries and re-configure it."),
                  _("FFmpeg startup failed"));
   }
}

wxString GetFFmpegVersion(wxWindow *parent)
{
   PickFFmpegLibs();

   wxString versionString = _("FFmpeg library not found");

   if (FFmpegLibsInst->ValidLibsLoaded()) {
      versionString = FFmpegLibsInst->GetLibraryVersion();
   }

   DropFFmpegLibs();

   return versionString;
}

void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl)
{
   //Most of this stuff is taken from FFmpeg tutorials and FFmpeg itself
   int av_log_level = AV_LOG_WARNING;
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

static int ufile_open(URLContext *h, const char *filename, int flags)
{
   wxFile *f;
   wxFile::OpenMode mode;

   f = new wxFile;
   if (!f) {
      return AVERROR(ENOMEM);
   }

   if (flags & URL_RDWR) {
      mode = wxFile::read_write;
   } else if (flags & URL_WRONLY) {
      mode = wxFile::write;
   } else {
      mode = wxFile::read;
   }

   if (!f->Open((const wxChar *) filename, mode)) {
      delete f;
      return AVERROR(ENOENT);
   }

   h->priv_data = (void *)f;

   return 0;
}

static int ufile_read(URLContext *h, unsigned char *buf, int size)
{
   //return (int) ((wxFile *) h->priv_data)->Read(buf, size);
   static int totalret = 0;
   int ret = (int) ((wxFile *) h->priv_data)->Read(buf, size);
   totalret += ret;
   return ret;
}

static int ufile_write(URLContext *h, unsigned char *buf, int size)
{
   return (int) ((wxFile *) h->priv_data)->Write(buf, size);
}

#if LIBAVFORMAT_VERSION_MAJOR >= 52
static int64_t ufile_seek(URLContext *h, int64_t pos, int whence)
#else
static offset_t ufile_seek(URLContext *h, offset_t pos, int whence)
#endif
{
   wxSeekMode mode = wxFromStart;

   switch (whence)
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
     return ((wxFile *) h->priv_data)->Length();
   }

   return ((wxFile *) h->priv_data)->Seek(pos, mode);
}

static int ufile_close(URLContext *h)
{
   wxFile *f = (wxFile *) h->priv_data;

   if (f) {
      f->Close();
      delete f;
   }

    return 0;
}

URLProtocol ufile_protocol = {
    "ufile",
    ufile_open,
    ufile_read,
    ufile_write,
    ufile_seek,
    ufile_close,
};

// Open a file with a (possibly) Unicode filename
int ufile_fopen(ByteIOContext **s, const wxString & name, int flags)
{
   URLContext *h;
   int err;

   // Open the file using our custom protocol and passing the (possibly) Unicode
   // filename.  This is playing a slight trick since our ufile_open() routine above
   // knows that the "char *" name may actually be wide characters.
   err = FFmpegLibsInst->url_open_protocol(&h, &ufile_protocol, (const char *) name.c_str(), flags);
   if (err < 0) {
      return err;
   }

   // Associate the file with a context
   err = FFmpegLibsInst->url_fdopen(s, h);
   if (err < 0) {
      FFmpegLibsInst->url_close(h);
      return err;
   }

   return 0;
}


// Size of probe buffer, for guessing file type from file contents
#define PROBE_BUF_MIN 2048
#define PROBE_BUF_MAX (1<<20)

// Detect type of input file and open it if recognized. Routine
// based on the av_open_input_file() libavformat function.
int ufile_fopen_input(AVFormatContext **ic_ptr, wxString & name)
{
   wxFileName f(name);
   wxCharBuffer fname;
   const char *filename;
   AVProbeData pd;
   ByteIOContext *pb = NULL;
   AVInputFormat *fmt = NULL;
   AVInputFormat *fmt1;
   int probe_size;
   int err;

   // Create a dummy file name using the extension from the original
   f.SetName(wxT("ufile"));
   fname = f.GetFullName().mb_str();
   filename = (const char *) fname;

   // Initialize probe data...go ahead and preallocate the maximum buffer size.
   pd.filename = filename;
   pd.buf_size = 0;
   pd.buf = (unsigned char *) FFmpegLibsInst->av_malloc(PROBE_BUF_MAX + AVPROBE_PADDING_SIZE);
   if (pd.buf == NULL) {
      err = AVERROR_NOMEM;
      goto fail;
   }

   // Open the file to prepare for probing
   if ((err = ufile_fopen(&pb, name, URL_RDONLY)) < 0) {
      goto fail;
   }

   for (probe_size = PROBE_BUF_MIN; probe_size <= PROBE_BUF_MAX && !fmt; probe_size <<= 1) {
      int score_max = probe_size < PROBE_BUF_MAX ? AVPROBE_SCORE_MAX / 4 : 0;

      // Read up to a "probe_size" worth of data
      pd.buf_size = FFmpegLibsInst->get_buffer(pb, pd.buf, probe_size);

      // AWD: with zero-length input files buf_size can come back negative;
      // this causes problems so we might as well just fail
      if (pd.buf_size < 0) {
         err = AVERROR_INVALIDDATA;
         goto fail;
      }

      // Clear up to a "AVPROBE_PADDING_SIZE" worth of unused buffer
      memset(pd.buf + pd.buf_size, 0, AVPROBE_PADDING_SIZE);

      // Reposition file for succeeding scan
      if (FFmpegLibsInst->url_fseek(pb, 0, SEEK_SET) < 0) {
         err = AVERROR(EIO);
         goto fail;
      }

      // Scan all input formats
      fmt = NULL;
      for (fmt1 = FFmpegLibsInst->av_iformat_next(NULL); fmt1 != NULL; fmt1 = FFmpegLibsInst->av_iformat_next(fmt1)) {
         int score = 0;

         // Ignore the ones that are not file based
         if (fmt1->flags & AVFMT_NOFILE) {
            continue;
         }

         // If the format can probe the file then try that first
         if (fmt1->read_probe) {
            score = fmt1->read_probe(&pd);
         }
         // Otherwize, resort to extension matching if available
         else if (fmt1->extensions) {
            if (FFmpegLibsInst->match_ext(filename, fmt1->extensions)) {
               score = 50;
            }
         }

         // Remember this format if it scored higher than a previous match
         if (score > score_max) {
            score_max = score;
            fmt = fmt1;
         }
         else if (score == score_max) {
            fmt = NULL;
         }
      }
   }

   // Didn't find a suitable format, so bail
   if (!fmt) {
      err = AVERROR_NOFMT;
      goto fail;
   }

   // And finally, attempt to associate an input stream with the file
   err = FFmpegLibsInst->av_open_input_stream(ic_ptr, pb, filename, fmt, NULL);
   if (err) {
      goto fail;
   }

   // Done with the probe buffer
   FFmpegLibsInst->av_freep(&pd.buf);

   return 0;

fail:
   if (pd.buf) {
      FFmpegLibsInst->av_freep(&pd.buf);
   }

   if (pb) {
      FFmpegLibsInst->url_fclose(pb);
   }

   *ic_ptr = NULL;

   return err;
}

/*******************************************************/

class FFmpegNotFoundDialog;

//----------------------------------------------------------------------------
// FindFFmpegDialog
//----------------------------------------------------------------------------

#define ID_FFMPEG_BROWSE 5000
#define ID_FFMPEG_DLOAD  5001

/// Allows user to locate libav* libraries
class FindFFmpegDialog : public wxDialog
{
public:

   FindFFmpegDialog(wxWindow *parent, wxString path, wxString name, wxString type)
      :  wxDialog(parent, wxID_ANY, wxString(_("Locate FFmpeg")))
   {
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

   void OnBrowse(wxCommandEvent & event)
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

   void OnDownload(wxCommandEvent & event)
   {
      wxString page = wxT("http://www.audacityteam.org/manual/index.php?title=FAQ:Installation_and_Plug-Ins%23installffmpeg");
      ::OpenInDefaultBrowser(page);
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

BEGIN_EVENT_TABLE(FindFFmpegDialog, wxDialog)
   EVT_BUTTON(ID_FFMPEG_BROWSE, FindFFmpegDialog::OnBrowse)
   EVT_BUTTON(ID_FFMPEG_DLOAD,  FindFFmpegDialog::OnDownload)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegNotFoundDialog
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(FFmpegNotFoundDialog, wxDialog)
   EVT_BUTTON(wxID_OK, FFmpegNotFoundDialog::OnOk)
END_EVENT_TABLE()


//----------------------------------------------------------------------------
// FFmpegLibs
//----------------------------------------------------------------------------

FFmpegLibs::FFmpegLibs()
{
   mLibsLoaded = false;
   refcount = 1;
   avformat = avcodec = avutil = NULL;
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
      wxLogDebug(wxT("mLibAVFormatPath ('%s') is not empty."), mLibAVFormatPath.c_str());
      wxFileName fn = mLibAVFormatPath;
      path = fn.GetPath();
      name = fn.GetFullName();
   }
   else {
      path = GetLibAVFormatPath();
      name = GetLibAVFormatName();
      wxLogDebug(wxT("mLibAVFormatPath is empty, starting with path '%s', name '%s'."), 
                  path.c_str(), name.c_str());
   }

   FindFFmpegDialog fd(parent,
                        path,
                        name,
                        GetLibraryTypeString());

   if (fd.ShowModal() == wxID_CANCEL) {
      wxLogDebug(wxT("User canceled the dialog. Failed to find FFmpeg libraries."));
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

   return true;
}

bool FFmpegLibs::LoadLibs(wxWindow *parent, bool showerr)
{

   wxLogMessage(wxT("Trying to load FFmpeg libraries..."));
   if (ValidLibsLoaded()) {
      wxLogMessage(wxT("FFmpeg libraries are already loaded."));
      FreeLibs();
   }

   // First try loading it from a previously located path
   if (!mLibAVFormatPath.IsEmpty()) {
      wxLogDebug(wxT("mLibAVFormatPath ('%s') is not empty. Loading from it."),mLibAVFormatPath.c_str());
      mLibsLoaded = InitLibs(mLibAVFormatPath,showerr);
   }

   // If not successful, try loading it from default path
   if (!mLibsLoaded && !GetLibAVFormatPath().IsEmpty()) {
      wxFileName fn(GetLibAVFormatPath(), GetLibAVFormatName());
      wxString path = fn.GetFullPath();
      wxLogMessage(wxT("Trying to load FFmpeg libraries from default path, '%s'."), path.c_str());
      mLibsLoaded = InitLibs(path,showerr);
      if (mLibsLoaded) {
         mLibAVFormatPath = path;
      }
   }

   // If not successful, try loading using system search paths
   if (!ValidLibsLoaded()) {
      wxString path = GetLibAVFormatName();
      wxLogDebug(wxT("Trying to load FFmpeg libraries from system paths. File name is '%s'."), path.c_str());
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
      FFmpegNotFoundDialog *dlg;
      gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
      if ((dontShowDlg == 0) && (showerr))
      {
          dlg = new FFmpegNotFoundDialog(NULL);
          dlg->ShowModal();
          delete dlg;
      }
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

bool FFmpegLibs::InitLibs(wxString libpath_format, bool showerr)
{
   wxString syspath;
   bool pathfix = false;

   FreeLibs();

#if defined(__WXMSW__)
   wxLogMessage(wxT("Looking up PATH environment variable..."));
   // First take PATH environment variable and store its content.
   if (wxGetEnv(wxT("PATH"),&syspath))
   {
      wxLogMessage(wxT("PATH = '%s'"), syspath.c_str());
      wxString fmtdirsc = wxPathOnly(libpath_format) + wxT(";");
      wxString scfmtdir = wxT(";") + wxPathOnly(libpath_format);
      wxString fmtdir = wxPathOnly(libpath_format);
      wxLogMessage(wxT("Checking that '%s' is in PATH..."), fmtdir.c_str());
      // If the directory, where libavformat is, is not in PATH - add it
      if (!syspath.Contains(fmtdirsc) && !syspath.Contains(scfmtdir) && !syspath.Contains(fmtdir))
      {
         wxLogWarning(wxT("FFmpeg directory is not in PATH."), fmtdir.c_str());
         if (syspath.Last() == wxT(';'))
         {
            wxLogMessage(wxT("Temporarily appending '%s' to PATH..."), fmtdir.c_str());
            syspath.Append(fmtdirsc);
         }
         else
         {
            wxLogMessage(wxT("Temporarily appending '%s' to PATH..."), scfmtdir.c_str());
            syspath.Append(scfmtdir);
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
   wxFileName name(libpath_format);
   bool gotError = false;

   avformat = new wxDynamicLibrary();
   wxLogDebug(wxT("Loading avformat from '%s'."), libpath_format.c_str());
   // Vaughan, 2010-08-17: No explanation why logging was turned off, so commented these out.
   //wxLogWindow* pLogger = wxGetApp().mLogger;
   //if (showerr)
   //   pLogger->SetActiveTarget(NULL);
   gotError = !avformat->Load(libpath_format, wxDL_LAZY);
   //if (showerr)
   //   pLogger->SetActiveTarget(pLogger);

   if (!gotError) {
      if (avformat->HasSymbol(wxT("av_free"))) {
         util = avformat;
      }
      if (avformat->HasSymbol(wxT("avcodec_init"))) {
         codec = avformat;
      }
   }

   if (!util) {
      name.SetFullName(GetLibAVUtilName());
      avutil = util =  new wxDynamicLibrary();
      wxLogDebug(wxT("Loading avutil from '%s'."), name.GetFullPath().c_str());
      //if (showerr)
      //   pLogger->SetActiveTarget(NULL);
      util->Load(name.GetFullPath(), wxDL_LAZY);
      //if (showerr)
      //   pLogger->SetActiveTarget(pLogger);
   }

   if (!codec) {
      name.SetFullName(GetLibAVCodecName());
      avcodec = codec = new wxDynamicLibrary();
      wxLogDebug(wxT("Loading avcodec from '%s'."), name.GetFullPath().c_str());
      //if (showerr)
      //   pLogger->SetActiveTarget(NULL);
      codec->Load(name.GetFullPath(), wxDL_LAZY);
      //if (showerr)
      //   pLogger->SetActiveTarget(pLogger);
   }

   if (!avformat->IsLoaded()) {
      //if (showerr)
      //   pLogger->SetActiveTarget(NULL);
      gotError = !avformat->Load(libpath_format, wxDL_LAZY);
      //if (showerr)
      //   pLogger->SetActiveTarget(pLogger);
   }

   //Return PATH to normal
   if ( pathfix )
   {
      wxString oldpath = syspath.BeforeLast(wxT(';'));
      wxLogMessage(wxT("Returning PATH to previous setting..."));
      wxSetEnv(wxT("PATH"),oldpath.c_str());
   }

   if (gotError) {
      wxLogError(wxT("Failed to load FFmpeg libraries."));
      FreeLibs();
      return false;
   }

   wxLogDebug(wxT("Importing symbols..."));
   INITDYN(avformat,av_register_all);
   INITDYN(avformat,av_open_input_file);
   INITDYN(avformat,av_find_stream_info);
   INITDYN(avformat,av_read_frame);
   INITDYN(avformat,av_seek_frame);
   INITDYN(avformat,av_close_input_file);
   INITDYN(avformat,av_index_search_timestamp);
   INITDYN(avformat,av_write_header);
   INITDYN(avformat,av_interleaved_write_frame);
   INITDYN(avformat,av_write_frame);
   INITDYN(avformat,av_iformat_next);
   INITDYN(avformat,av_oformat_next);
   INITDYN(avformat,av_set_parameters);
#if LIBAVFORMAT_VERSION_MAJOR < 53
   INITDYN(avformat,register_protocol);
   av_register_protocol = register_protocol;
#else
   INITDYN(avformat,av_register_protocol);
#endif
   INITDYN(avformat,url_open_protocol);
   INITDYN(avformat,url_fdopen);
   INITDYN(avformat,url_close);
   INITDYN(avformat,url_fopen);
   INITDYN(avformat,url_fseek);
   INITDYN(avformat,url_fclose);
   INITDYN(avformat,url_fsize);
   INITDYN(avformat,av_new_stream);
   INITDYN(avformat,av_alloc_format_context);
   INITDYN(avformat,guess_format);
   INITDYN(avformat,av_write_trailer);
   INITDYN(avformat,av_codec_get_id);
   INITDYN(avformat,av_codec_get_tag);
   INITDYN(avformat,avformat_version);
   INITDYN(avformat,av_open_input_file);
   INITDYN(avformat,av_open_input_stream);
   INITDYN(avformat,get_buffer);
   INITDYN(avformat,match_ext);

#if FFMPEG_STABLE
   INITDYN(avformat,av_init_packet);
#else
   INITDYN(codec,av_init_packet);
   INITDYN(codec,av_free_packet);
#endif

   INITDYN(codec,avcodec_init);
   INITDYN(codec,avcodec_find_encoder);
   INITDYN(codec,avcodec_find_encoder_by_name);
   INITDYN(codec,avcodec_find_decoder);
   INITDYN(codec,avcodec_find_decoder_by_name);
   INITDYN(codec,avcodec_string);
   INITDYN(codec,avcodec_get_context_defaults);
   INITDYN(codec,avcodec_alloc_context);
   INITDYN(codec,avcodec_get_frame_defaults);
   INITDYN(codec,avcodec_alloc_frame);
   INITDYN(codec,avcodec_open);
   INITDYN(codec,avcodec_decode_audio2);
   INITDYN(codec,avcodec_encode_audio);
   INITDYN(codec,avcodec_close);
   INITDYN(codec,avcodec_register_all);
   INITDYN(codec,avcodec_flush_buffers);
   INITDYN(codec,av_get_bits_per_sample);
   INITDYN(codec,av_get_bits_per_sample_format);
   INITDYN(codec,avcodec_version);
   INITDYN(codec,av_fast_realloc);
   INITDYN(codec,av_codec_next);

   INITDYN(util,av_free);
   INITDYN(util,av_log_set_callback);
   INITDYN(util,av_log_default_callback);
#if FFMPEG_STABLE
   INITDYN(util,av_fifo_init);
   INITDYN(util,av_fifo_read);
   INITDYN(util,av_fifo_realloc);
#else
   INITDYN(util,av_fifo_alloc);
   INITDYN(util,av_fifo_generic_read);
   INITDYN(util,av_fifo_realloc2);
#endif
   INITDYN(util,av_fifo_free);
   INITDYN(util,av_fifo_size);
   INITDYN(util,av_malloc);
   INITDYN(util,av_fifo_generic_write);
   INITDYN(util,av_freep);
   INITDYN(util,av_rescale_q);
   INITDYN(util,av_strstart);
   INITDYN(util,avutil_version);

   //FFmpeg initialization
   wxLogDebug(wxT("All symbols loaded successfully. Initializing the library."));
   this->avcodec_init();
   this->avcodec_register_all();
   this->av_register_all();
   
   wxLogMessage(wxT("Retrieving FFmpeg library version numbers:"));
   int avcver = this->avcodec_version();
   int avfver = this->avformat_version();
   int avuver = this->avutil_version();
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

   int avcverdiff = (avcver >> 16 & 0xFF) - int(LIBAVCODEC_VERSION_MAJOR);
   int avfverdiff = (avfver >> 16 & 0xFF) - int(LIBAVFORMAT_VERSION_MAJOR);
   int avuverdiff = (avuver >> 16 & 0xFF) - int(LIBAVUTIL_VERSION_MAJOR);
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

   av_register_protocol(&ufile_protocol);

   return true;
}

void FFmpegLibs::FreeLibs()
{
   if (avformat != NULL) {
      delete avformat;
      avformat = NULL;
   }

   if (avcodec != NULL) {
      delete avcodec;
      avcodec = NULL;
   }

   if (avutil != NULL) {
      delete avutil;
      avutil = NULL;
   }

   mLibsLoaded = false;

   return;
}

#endif //USE_FFMPEG
