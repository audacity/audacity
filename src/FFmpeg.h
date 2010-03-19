/**********************************************************************

Audacity: A Digital Audio Editor

FFmpeg.h

Audacity(R) is copyright (c) 1999-2009 Audacity Team.
License: GPL v2.  See License.txt.

******************************************************************//**

Describes shared object that is used to access FFmpeg libraries.

*//*******************************************************************/

#if !defined(__AUDACITY_FFMPEG__)
#define __AUDACITY_FFMPEG__

/* FFmpeg is written in C99. It uses many types from stdint.h. Because we are
 * compiling this using a C++ compiler we have to put it in extern "C".
 * __STDC_CONSTANT_MACROS is defined to make <stdint.h> behave like it
 * is actually being compiled with a C99 compiler.
 *
 * The symptoms are that INT64_C is not a valid type, which tends to break
 * somewhere down in the implementations using this file */

/* In order to be able to compile this file when ffmpeg is not available we
 * need access to the value of USE_FFMPEG, which means config*.h needs to come
 * in before this file. The suggest way to achieve this is by including
 * Audacity.h */

#if defined(USE_FFMPEG)
extern "C" {
   // Include errno.h before the ffmpeg includes since they depend on 
   // knowing the value of EINVAL...see bottom of avcodec.h.  Not doing
   // so will produce positive error returns when they should be < 0.
   #include <errno.h>

   #include <libavcodec/avcodec.h>
   #include <libavformat/avformat.h>
   #include <libavutil/fifo.h>
}
#endif

#include "Audacity.h"
/* rather earlier than normal, but pulls in config*.h and other program stuff
 * we need for the next bit */
#include <wx/string.h>
#include <wx/dynlib.h>
#include <wx/log.h>			// for wxLogNull
#include <wx/msgdlg.h>		// for wxMessageBox
#include <wx/utils.h>
#include "../widgets/LinkingHtmlWindow.h"
#include "FileDialog.h"
#include "ShuttleGui.h"
#include "../Prefs.h"
#include <wx/checkbox.h>
#include <wx/textctrl.h>

#include "Experimental.h"

// if you needed them, any other audacity header files would go here

/* These defines apply whether or not ffmpeg is available */
#define INITDYN(w,f) if ((*(void**)&this->f=(void*)w->GetSymbol(wxT(#f))) == NULL) { wxLogMessage(wxT("Failed to load symbol ") wxT(#f)); return false; };

/// Callback function to catch FFmpeg log messages.
/// Uses wxLogMessage.
void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl);

//----------------------------------------------------------------------------
// Get FFmpeg library version
//----------------------------------------------------------------------------
wxString GetFFmpegVersion(wxWindow *parent);

/* from here on in, this stuff only applies when ffmpeg is available */
#if defined(USE_FFMPEG)

/* This is a bit shortsighted (matches only 0.5 exactly), but i can't come up with anything smarter at the moment */
#if LIBAVFORMAT_VERSION_MAJOR == 52 && LIBAVFORMAT_VERSION_MINOR == 31 && LIBAVCODEC_VERSION_MAJOR == 52 && LIBAVCODEC_VERSION_MINOR == 20
#define FFMPEG_STABLE 1
#else
#define FFMPEG_STABLE 0
#endif

int ufile_fopen(ByteIOContext **s, const wxString & name, int flags);

#if defined(__WXMSW__)
int modify_file_url_to_utf8(char* buffer, size_t buffer_size, const char* url);
int modify_file_url_to_utf8(char* buffer, size_t buffer_size, const wchar_t* url);
#endif

//----------------------------------------------------------------------------
// Attempt to load and enable/disable FFmpeg at startup
//----------------------------------------------------------------------------
void FFmpegStartup();

bool LoadFFmpeg(bool showerror);

/// If Audacity failed to load libav*, this dialog
/// shows up and tells user about that. It will pop-up
/// again and again until it is disabled.
class FFmpegNotFoundDialog : public wxDialog
{
public:

   FFmpegNotFoundDialog(wxWindow *parent)
      :  wxDialog(parent, wxID_ANY, wxString(_("FFmpeg not found")))
   {
      ShuttleGui S(this, eIsCreating);
      PopulateOrExchange(S);
   }

   void PopulateOrExchange(ShuttleGui & S)
   {
      wxString text;

      S.SetBorder(10);
      S.StartVerticalLay(true);
      {
         S.AddFixedText(_(
"Audacity attempted to use FFmpeg to import an audio file,\n\
but the libraries were not found.\n\n\
To use FFmpeg import, go to Preferences > Libraries\n\
to download or locate the FFmpeg libraries."
         ));

         int dontShowDlg = 0;
         gPrefs->Read(wxT("/FFmpeg/NotFoundDontShow"),&dontShowDlg,0);
         mDontShow = S.AddCheckBox(_("Do not show this warning again"),dontShowDlg ? wxT("true") : wxT("false"));

         S.AddStandardButtons(eOkButton);
      }
      S.EndVerticalLay();

      Layout();
      Fit();
      SetMinSize(GetSize());
      Center();

      return;
   }

   void OnOk(wxCommandEvent & event)
   {
      if (mDontShow->GetValue())
      {
         gPrefs->Write(wxT("/FFmpeg/NotFoundDontShow"),1);
      }
      this->EndModal(0);
   }

private:

   wxCheckBox *mDontShow;

   DECLARE_EVENT_TABLE()
};

/// Manages liabv* libraries - loads/unloads libraries, imports symbols.
/// Only one instance of this class should exist at each given moment.
/// function definitions are taken from FFmpeg headers manually,
/// eventually (at next major FFmpeg version change) we'll have to review
/// them and update if necessary.
class FFmpegLibs
{
public:
   FFmpegLibs();
   ~FFmpegLibs();

   void              (*av_log_set_callback)           (void (*)(void*, int, const char*, va_list));
   void              (*av_log_default_callback)       (void* ptr, int level, const char* fmt, va_list vl);
   void              (*av_free)                       (void *ptr);
   unsigned          (*avcodec_version)               (void);
   unsigned          (*avformat_version)              (void);
   unsigned          (*avutil_version)                (void);
   void              (*avcodec_init)                  (void);
   AVCodec*          (*avcodec_find_encoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_encoder_by_name)  (const char *name);
   AVCodec*          (*avcodec_find_decoder)          (enum CodecID id);
   AVCodec*          (*avcodec_find_decoder_by_name)  (const char *name);
   enum CodecID      (*av_codec_get_id)               (const struct AVCodecTag * const *tags, unsigned int tag);
   unsigned int      (*av_codec_get_tag)              (const struct AVCodecTag * const *tags, enum CodecID id);
   void              (*avcodec_string)                (char *buf, int buf_size, AVCodecContext *enc, int encode);
   void              (*avcodec_get_context_defaults)  (AVCodecContext *s);
   AVCodecContext*   (*avcodec_alloc_context)         (void);
   void              (*avcodec_get_frame_defaults)    (AVFrame *pic);
   AVFrame*          (*avcodec_alloc_frame)           (void);
   int               (*avcodec_open)                  (AVCodecContext *avctx, AVCodec *codec);
   int               (*avcodec_decode_audio2)         (AVCodecContext *avctx, int16_t *samples, int *frame_size_ptr, const uint8_t *buf, int buf_size);
   int               (*avcodec_encode_audio)          (AVCodecContext *avctx, uint8_t *buf, int buf_size, const short *samples);
   int               (*avcodec_close)                 (AVCodecContext *avctx);
   void              (*avcodec_register_all)          (void);
   void              (*avcodec_flush_buffers)         (AVCodecContext *avctx);
   int               (*av_get_bits_per_sample)        (enum CodecID codec_id);
   int               (*av_get_bits_per_sample_format) (enum SampleFormat sample_fmt);
   void*             (*av_fast_realloc)               (void *ptr, unsigned int *size, unsigned int min_size);
   int               (*av_open_input_file)            (AVFormatContext **ic_ptr, const char *filename, AVInputFormat *fmt, int buf_size, AVFormatParameters *ap);
   int               (*av_open_input_stream)          (AVFormatContext **ic_ptr, ByteIOContext *pb, const char *filename, AVInputFormat *fmt, AVFormatParameters *ap);
   int               (*get_buffer)                    (ByteIOContext *s, unsigned char *buf, int size);
   void              (*av_register_all)               (void);
#if LIBAVFORMAT_VERSION_MAJOR < 53
   int               (*register_protocol)             (URLProtocol *protocol);
#endif
   int               (*av_register_protocol)          (URLProtocol *protocol);
   int               (*av_strstart)                   (const char *str, const char *pfx, const char **ptr);
   int               (*av_find_stream_info)           (AVFormatContext *ic);
   int               (*av_read_frame)                 (AVFormatContext *s, AVPacket *pkt);
   int               (*av_seek_frame)                 (AVFormatContext *s, int stream_index, int64_t timestamp, int flags);
   int               (*av_close_input_file)           (AVFormatContext *s);
   int               (*av_index_search_timestamp)     (AVStream *st, int64_t timestamp, int flags);
   int               (*av_write_header)               (AVFormatContext *s);
   AVInputFormat*    (*av_iformat_next)               (AVInputFormat *f);
   AVOutputFormat*   (*av_oformat_next)               (AVOutputFormat *f);
   AVCodec*          (*av_codec_next)                 (AVCodec *c);
   int               (*av_set_parameters)             (AVFormatContext *s, AVFormatParameters *ap);
   int               (*url_open_protocol)             (URLContext **puc, struct URLProtocol *up, const char *filename, int flags);
   int               (*url_fdopen)                    (ByteIOContext **s, URLContext *h);
   int               (*url_close)                     (URLContext *h);
   int               (*url_fopen)                     (ByteIOContext **s, const char *filename, int flags);
   int64_t           (*url_fseek)                     (ByteIOContext *s, int64_t offset, int whence);
   int               (*url_fclose)                    (ByteIOContext *s);
   int               (*url_fsize)                     (ByteIOContext *s);
   AVStream*         (*av_new_stream)                 (AVFormatContext *s, int id);
   AVFormatContext*  (*av_alloc_format_context)       (void);
   AVOutputFormat*   (*guess_format)                  (const char *short_name, const char *filename, const char *mime_type);
   int               (*match_ext)                     (const char *filename, const char *extensions);
   int               (*av_write_trailer)              (AVFormatContext *s);
   int               (*av_interleaved_write_frame)    (AVFormatContext *s, AVPacket *pkt);
   int               (*av_write_frame)                (AVFormatContext *s, AVPacket *pkt);
   void              (*av_init_packet)                (AVPacket *pkt);
   int               (*av_fifo_generic_write)         (AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int));   
   void              (*av_fifo_free)                  (AVFifoBuffer *f);
   int               (*av_fifo_size)                  (AVFifoBuffer *f);
   void*             (*av_malloc)                     (unsigned int size);
   void              (*av_freep)                      (void *ptr);
   int64_t           (*av_rescale_q)                  (int64_t a, AVRational bq, AVRational cq);
#if !FFMPEG_STABLE
   void              (*av_free_packet)                (AVPacket *pkt);
   AVFifoBuffer*     (*av_fifo_alloc)                 (unsigned int size);
   int               (*av_fifo_generic_read)          (AVFifoBuffer *f, uint8_t *buf, int buf_size, int (*func)(void*, void*, int));
   int               (*av_fifo_realloc2)              (AVFifoBuffer *f, unsigned int size);
#else
   int               (*av_fifo_init)                  (AVFifoBuffer *f, int size);
   int               (*av_fifo_read)                  (AVFifoBuffer *f, uint8_t *buf, int buf_size);
   void              (*av_fifo_realloc)               (AVFifoBuffer *f, unsigned int size);
#endif

   ///! Finds libav* libraries
   ///\return true if found, false if not found
   bool FindLibs(wxWindow *parent);
   ///! Loads libav* libraries
   ///\param showerr - controls whether or not to show an error dialog if libraries cannot be loaded
   ///\return true if loaded, false if not loaded
   bool LoadLibs(wxWindow *parent, bool showerr);
   ///! Checks if libraries are loaded
   ///\return true if libraries are loaded, false otherwise
   bool ValidLibsLoaded();

   ///! Initializes the libraries. Call after LoadLibs (when ValidLibsLoaded returns true)
   ///\param libpath_codec - full file path to the libavformat library
   ///\param showerr - controls whether or not to show an error dialog if libraries cannot be loaded
   ///\return true if initialization completed without errors, false otherwise
   /// do not call (it is called by FindLibs automatically)
   bool InitLibs(wxString libpath_codec, bool showerr);

   ///! Frees (unloads) loaded libraries
   void FreeLibs();

   ///! Returns library version as string
   ///\return libavformat library version or empty string?
   wxString GetLibraryVersion()
   {
      return wxString::Format(wxT("F(%s),C(%s),U(%s)"),mAVFormatVersion.c_str(),mAVCodecVersion.c_str(),mAVUtilVersion.c_str());
   }

#if defined(__WXMSW__)
   /* Library names and file filters for Windows only */

   wxString GetLibraryTypeString()
   {
      return _("Only avformat.dll|*avformat*.dll|Dynamically Linked Libraries (*.dll)|*.dll|All Files (*.*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      wxRegKey reg(wxT("HKEY_LOCAL_MACHINE\\Software\\FFmpeg for Audacity"));
      wxString path;

      if (reg.Exists()) {
         reg.QueryValue(wxT("InstallPath"), path);
      }

      return path;
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("avformat-") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) wxT(".dll"));
   }

   wxString GetLibAVCodecName()
   {
      return (wxT("avcodec-") wxT(AV_STRINGIFY(LIBAVCODEC_VERSION_MAJOR)) wxT(".dll"));
   }

   wxString GetLibAVUtilName()
   {
      return (wxT("avutil-") wxT(AV_STRINGIFY(LIBAVUTIL_VERSION_MAJOR)) wxT(".dll"));
   }
#elif defined(__WXMAC__)
   /* Library names and file filters for Mac OS only */
   wxString GetLibraryTypeString()
   {
      return _("Dynamic Libraries (*.dylib)|*.dylib|All Files (*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("/usr/local/lib/audacity");
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("libavformat.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) wxT(".dylib"));
   }

   wxString GetLibAVCodecName()
   {
      return (wxT("libavcodec.") wxT(AV_STRINGIFY(LIBAVCODEC_VERSION_MAJOR)) wxT(".dylib"));
   }

   wxString GetLibAVUtilName()
   {
      return (wxT("libavutil.") wxT(AV_STRINGIFY(LIBAVUTIL_VERSION_MAJOR)) wxT(".dylib"));
   }
#else
   /* Library names and file filters for other platforms, basically Linux and
	* other *nix platforms */
   wxString GetLibraryTypeString()
   {
      return _("Only libavformat.so|libavformat.so*|Dynamically Linked Libraries (*.so*)|*.so*|All Files (*)|*");
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("");
   }

   wxString GetLibAVFormatName()
   {
      return (wxT("libavformat.so.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)));
   }

   wxString GetLibAVCodecName()
   {
      return (wxT("libavcodec.so.") wxT(AV_STRINGIFY(LIBAVCODEC_VERSION_MAJOR)));
   }

   wxString GetLibAVUtilName()
   {
      return (wxT("libavutil.so.") wxT(AV_STRINGIFY(LIBAVUTIL_VERSION_MAJOR)));
   }
#endif // (__WXMAC__) || (__WXMSW__)

   /// Ugly reference counting. I thought of using wxStuff for that,
   /// but decided that wx reference counting is not useful, since
   /// there's no data sharing - object is shared because libraries are.
   int refcount;

private:

   ///! Stored path to libavformat library
   wxString mLibAVFormatPath;

   ///! Stored library version
   wxString mAVCodecVersion;
   wxString mAVFormatVersion;
   wxString mAVUtilVersion;
   
   ///! wx interfaces for dynamic libraries
   wxDynamicLibrary *avformat;
   wxDynamicLibrary *avcodec;
   wxDynamicLibrary *avutil;

   ///! true if libraries are loaded, false otherwise
   bool mLibsLoaded;
};

///! Helper function - creates FFmpegLibs object if it does not exists
///! or just increments reference count if it does
///! It is usually called by constructors or initializators
FFmpegLibs *PickFFmpegLibs();

///! Helper function - destroys FFmpegLibs object if there is no need for it
///! anymore, or just decrements it's reference count
void        DropFFmpegLibs();

int ufile_fopen(ByteIOContext **s, const wxString & name, int flags);
int ufile_fopen_input(AVFormatContext **ic_ptr, wxString & name);

//moving from ImportFFmpeg.cpp to FFMpeg.h so other cpp files can use this struct.
#ifdef EXPERIMENTAL_OD_FFMPEG
typedef struct _streamContext
{
   bool                 m_use;                           // TRUE = this stream will be loaded into Audacity
   AVStream            *m_stream;		      	         // an AVStream *
   AVCodecContext      *m_codecCtx;			               // pointer to m_stream->codec

   AVPacket             m_pkt;				               // the last AVPacket we read for this stream
   int                  m_pktValid;			               // is m_pkt valid?
   uint8_t             *m_pktDataPtr;		           	   // pointer into m_pkt.data
   int                  m_pktRemainingSiz;	

   int64_t			      m_pts;			              	   // the current presentation time of the input stream
   int64_t			      m_ptsOffset;		    	         // packets associated with stream are relative to this

   int                  m_frameValid;		               // is m_decodedVideoFrame/m_decodedAudioSamples valid?
   int16_t             *m_decodedAudioSamples;           // decoded audio samples stored here
   unsigned int			m_decodedAudioSamplesSiz;        // current size of m_decodedAudioSamples
   int			         m_decodedAudioSamplesValidSiz;   // # valid bytes in m_decodedAudioSamples
   int                  m_initialchannels;               // number of channels allocated when we begin the importing. Assumes that number of channels doesn't change on the fly.
} streamContext;
#endif

#endif // USE_FFMPEG
#endif // __AUDACITY_FFMPEG__

