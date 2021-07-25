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



#include "audacity/Types.h"
#include "widgets/wxPanelWrapper.h" // to inherit

#if defined(__WXMSW__)
#include <wx/msw/registry.h> // for wxRegKey
#endif

class wxCheckBox;

// TODO: Determine whether the libav* headers come from the FFmpeg or libav
// project and set IS_FFMPEG_PROJECT depending on it.
#define IS_FFMPEG_PROJECT 1

/* FFmpeg is written in C99. It uses many types from stdint.h. Because we are
 * compiling this using a C++ compiler we have to put it in extern "C".
 * __STDC_CONSTANT_MACROS is defined to make <stdint.h> behave like it
 * is actually being compiled with a C99 compiler.
 *
 * The symptoms are that INT64_C is not a valid type, which tends to break
 * somewhere down in the implementations using this file */

#if defined(USE_FFMPEG)
extern "C" {
   // Include errno.h before the ffmpeg includes since they depend on
   // knowing the value of EINVAL...see bottom of avcodec.h.  Not doing
   // so will produce positive error returns when they should be < 0.
   #include <errno.h>

   #include <libavcodec/avcodec.h>
   #include <libavformat/avformat.h>
   #include <libavutil/error.h>
   #include <libavutil/fifo.h>
   #include <libavutil/mathematics.h>

   #if defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
      #if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(55, 45, 101)
      #define av_frame_alloc avcodec_alloc_frame
      #define av_frame_free avcodec_free_frame
      #endif

      #if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(54, 59, 100)
      inline void avcodec_free_frame(AVFrame **frame) {
         av_free(*frame);
      }
      #endif

      #if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(54, 51, 100)
      #define AVCodecID CodecID
      #define AV_CODEC_ID_AAC CODEC_ID_AAC
      #define AV_CODEC_ID_AC CODEC_ID_AC
      #define AV_CODEC_ID_AC3 CODEC_ID_AC3
      #define AV_CODEC_ID_ADPCM CODEC_ID_ADPCM
      #define AV_CODEC_ID_ADPCM_CT CODEC_ID_ADPCM_CT
      #define AV_CODEC_ID_ADPCM_G726 CODEC_ID_ADPCM_G726
      #define AV_CODEC_ID_ADPCM_IMA_QT CODEC_ID_ADPCM_IMA_QT
      #define AV_CODEC_ID_ADPCM_IMA_WAV CODEC_ID_ADPCM_IMA_WAV
      #define AV_CODEC_ID_ADPCM_MS CODEC_ID_ADPCM_MS
      #define AV_CODEC_ID_ADPCM_SWF CODEC_ID_ADPCM_SWF
      #define AV_CODEC_ID_ADPCM_YAMAHA CODEC_ID_ADPCM_YAMAHA
      #define AV_CODEC_ID_ALAC CODEC_ID_ALAC
      #define AV_CODEC_ID_AMR CODEC_ID_AMR
      #define AV_CODEC_ID_AMR_NB CODEC_ID_AMR_NB
      #define AV_CODEC_ID_AMR_WB CODEC_ID_AMR_WB
      #define AV_CODEC_ID_ATRAC CODEC_ID_ATRAC
      #define AV_CODEC_ID_ATRAC3 CODEC_ID_ATRAC3
      #define AV_CODEC_ID_DTS CODEC_ID_DTS
      #define AV_CODEC_ID_DVAUDIO CODEC_ID_DVAUDIO
      #define AV_CODEC_ID_FLAC CODEC_ID_FLAC
      #define AV_CODEC_ID_GSM CODEC_ID_GSM
      #define AV_CODEC_ID_GSM_MS CODEC_ID_GSM_MS
      #define AV_CODEC_ID_IMC CODEC_ID_IMC
      #define AV_CODEC_ID_MACE CODEC_ID_MACE
      #define AV_CODEC_ID_MACE3 CODEC_ID_MACE3
      #define AV_CODEC_ID_MACE6 CODEC_ID_MACE6
      #define AV_CODEC_ID_MP CODEC_ID_MP
      #define AV_CODEC_ID_MP2 CODEC_ID_MP2
      #define AV_CODEC_ID_MP3 CODEC_ID_MP3
      #define AV_CODEC_ID_NELLYMOSER CODEC_ID_NELLYMOSER
      #define AV_CODEC_ID_NONE CODEC_ID_NONE
      #define AV_CODEC_ID_PCM CODEC_ID_PCM
      #define AV_CODEC_ID_PCM_ALAW CODEC_ID_PCM_ALAW
      #define AV_CODEC_ID_PCM_MULAW CODEC_ID_PCM_MULAW
      #define AV_CODEC_ID_PCM_S16BE CODEC_ID_PCM_S16BE
      #define AV_CODEC_ID_PCM_S16LE CODEC_ID_PCM_S16LE
      #define AV_CODEC_ID_PCM_S24BE CODEC_ID_PCM_S24BE
      #define AV_CODEC_ID_PCM_S24LE CODEC_ID_PCM_S24LE
      #define AV_CODEC_ID_PCM_S32BE CODEC_ID_PCM_S32BE
      #define AV_CODEC_ID_PCM_S32LE CODEC_ID_PCM_S32LE
      #define AV_CODEC_ID_PCM_S8 CODEC_ID_PCM_S8
      #define AV_CODEC_ID_PCM_U8 CODEC_ID_PCM_U8
      #define AV_CODEC_ID_QCELP CODEC_ID_QCELP
      #define AV_CODEC_ID_QDM CODEC_ID_QDM
      #define AV_CODEC_ID_QDM2 CODEC_ID_QDM2
      #define AV_CODEC_ID_ROQ CODEC_ID_ROQ
      #define AV_CODEC_ID_ROQ_DPCM CODEC_ID_ROQ_DPCM
      #define AV_CODEC_ID_SONIC CODEC_ID_SONIC
      #define AV_CODEC_ID_SONIC_LS CODEC_ID_SONIC_LS
      #define AV_CODEC_ID_TRUESPEECH CODEC_ID_TRUESPEECH
      #define AV_CODEC_ID_VORBIS CODEC_ID_VORBIS
      #define AV_CODEC_ID_VOXWARE CODEC_ID_VOXWARE
      #define AV_CODEC_ID_WMAPRO CODEC_ID_WMAPRO
      #define AV_CODEC_ID_WMAV CODEC_ID_WMAV
      #define AV_CODEC_ID_WMAV1 CODEC_ID_WMAV1
      #define AV_CODEC_ID_WMAV2 CODEC_ID_WMAV2
      #define AV_CODEC_ID_WMAVOICE CODEC_ID_WMAVOICE
      #endif

      #if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(54, 8, 100)
      inline bool av_codec_is_encoder(AVCodec *codec) {
         return codec != NULL && (codec->encode != NULL || codec->encode2 != NULL);
      }
      #endif
   #endif

   #if LIBAVCODEC_VERSION_MAJOR < 58
      #ifndef AV_CODEC_FLAG_QSCALE
         #define AV_CODEC_FLAG_QSCALE CODEC_FLAG_QSCALE
      #endif
      #ifndef AV_CODEC_FLAG_GLOBAL_HEADER
         #define AV_CODEC_FLAG_GLOBAL_HEADER CODEC_FLAG_GLOBAL_HEADER
      #endif
      #ifndef AV_CODEC_CAP_SMALL_LAST_FRAME
         #define AV_CODEC_CAP_SMALL_LAST_FRAME CODEC_CAP_SMALL_LAST_FRAME
      #endif
   #endif

}
#endif

/* rather earlier than normal, but pulls in config*.h and other program stuff
 * we need for the next bit */
#include "ShuttleGui.h"
#include "Prefs.h"

#include "Identifier.h"
#include "SampleFormat.h"

class wxDynamicLibrary;

// if you needed them, any other audacity header files would go here

/// Callback function to catch FFmpeg log messages.
void av_log_wx_callback(void* ptr, int level, const char* fmt, va_list vl);

//----------------------------------------------------------------------------
// Get FFmpeg library version
//----------------------------------------------------------------------------
TranslatableString GetFFmpegVersion();

/* from here on in, this stuff only applies when ffmpeg is available */
#if defined(USE_FFMPEG)

//----------------------------------------------------------------------------
// Attempt to load and enable/disable FFmpeg at startup
//----------------------------------------------------------------------------
void FFmpegStartup();

bool LoadFFmpeg(bool showerror);


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
   bool InitLibs(const wxString &libpath_codec, bool showerr);

   ///! Frees (unloads) loaded libraries
   void FreeLibs();

   ///! Returns library version as string
   ///\return libavformat library version or empty string?
   wxString GetLibraryVersion()
   {
      return wxString::Format(wxT("F(%s),C(%s),U(%s)"),mAVFormatVersion,mAVCodecVersion,mAVUtilVersion);
   }

#if defined(__WXMSW__)
   /* Library names and file filters for Windows only */

   FileNames::FileTypes GetLibraryTypes()
   {
      return {
         /* i18n-hint: do not translate avformat.  Preserve the computer gibberish.*/
         { XO("Only avformat.dll"), { GetLibAVFormatName() } },
         FileNames::DynamicLibraries,
         FileNames::AllFiles
      };
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
   FileNames::FileTypes GetLibraryTypes()
   {
      return {
         FileNames::DynamicLibraries,
         FileNames::AllFiles
      };
   }

   wxString GetLibAVFormatPath()
   {
      return wxT("/Library/Application Support/audacity/libs");
   }

   wxString GetLibAVFormatName()
   {
      if (sizeof(void*) == 8)
         return (wxT("ffmpeg.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) wxT(".64bit.dylib"));
      return (wxT("libavformat.") wxT(AV_STRINGIFY(LIBAVFORMAT_VERSION_MAJOR)) wxT(".dylib"));
   }

   wxString GetLibAVCodecName()
   {
      if (sizeof(void*) == 8)
         return (wxT("ffmpeg_codecs.") wxT(AV_STRINGIFY(LIBAVCODEC_VERSION_MAJOR)) wxT(".64bit.dylib"));
      return (wxT("libavcodec.") wxT(AV_STRINGIFY(LIBAVCODEC_VERSION_MAJOR)) wxT(".dylib"));
   }

   wxString GetLibAVUtilName()
   {
      if (sizeof(void*) == 8)
         return (wxT("ffmpeg_utils.") wxT(AV_STRINGIFY(LIBAVUTIL_VERSION_MAJOR)) wxT(".64bit.dylib"));
      return (wxT("libavutil.") wxT(AV_STRINGIFY(LIBAVUTIL_VERSION_MAJOR)) wxT(".dylib"));
   }
#else
   /* Library names and file filters for other platforms, basically Linux and
    * other *nix platforms */
   FileNames::FileTypes GetLibraryTypes()
   {
      return {
         { XO("Only libavformat.so"), { wxT("libavformat*.so*") } },
         FileNames::DynamicLibraries,
         FileNames::AllFiles
      };
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
   std::unique_ptr<wxDynamicLibrary> avformat, avcodec, avutil;

   ///! true if libraries are loaded, false otherwise
   bool mLibsLoaded;
};

///! Helper function - creates FFmpegLibs object if it does not exists
///! or just increments reference count if it does
///! It is usually called by constructors or initializators
FFmpegLibs *PickFFmpegLibs();

///! Helper function - destroys FFmpegLibs object if there is no need for it
///! anymore, or just decrements its reference count
void        DropFFmpegLibs();

// This object allows access to the AVFormatContext,
// and its destructor cleans up memory and file handles
struct FFmpegContext {
   FFmpegContext() {}
   ~FFmpegContext();

   AVIOContext *pb{};
   AVFormatContext *ic_ptr{};
};

int ufile_fopen(AVIOContext **s, const FilePath & name, int flags);
int ufile_fopen_input(std::unique_ptr<FFmpegContext> &context_ptr, FilePath & name);
int ufile_close(AVIOContext *pb);

struct streamContext;
// common utility functions
// utility calls that are shared with ImportFFmpeg
streamContext *import_ffmpeg_read_next_frame(AVFormatContext* formatContext,
                                             streamContext** streams,
                                             unsigned int numStreams);

int import_ffmpeg_decode_frame(streamContext *sc, bool flushing);

#if !defined(DISABLE_DYNAMIC_LOADING_FFMPEG)
extern "C" {
   // A little explanation of what's going on here.
   //
   // The FFmpeg function pointers used to be defined in the FFmpegLibs class and all calls would
   // be done via the global class pointer FFmpegLibsInst.  This was fine as long as the prototypes
   // defined in the class matched the actual functions in the FFmpeg libraries.  There was no
   // compile time validation to prevent invalid function calls.  So, what follows is one way of
   // getting the extra validation.
   //
   // Only one source file should define DEFINE_FFMPEG_POINTERS before including ffmpeg.h.  This
   // will cause the compiler to dump all of the function pointers to a single object file and
   // prevent duplicate symbol definitions during link.  This is currently done in ffmpeg.cpp.
   //
   // The FFMPEG_FUNCTION_WITH_RETURN and FFMPEG_FUNCTION_NO_RETURN macros do two things each:
   // 1)  Define or reference the variable that contains the actual function pointer
   // 2)  Define an inline function to pass control to the real function
   //
   // Since the macros redefine the real ffmpeg functions of the same name, the compiler will
   // make sure that the definitions are the same.  If not, it will complain.  For this to occur,
   // the functions MUST be defined in an extern "C" block otherwise the compiler just thinks the
   // functions are being overloaded.
   //
   // The compiler should optimize away the inline function since it just passes control to the real
   // function and we should wind up with about the same function call we had before, only now it is
   // safer due to the validation.
   //
   // The FFMPEG_FUNCTION_WITH_RETURN takes 4 arguments:
   // 1)  The return type           <---|
   // 2)  The function name             | Taken from the FFmpeg function prototype
   // 3)  The function arguments    <---|
   // 4)  The argument list to pass to the real function
   //
   // The FFMPEG_FUNCTION_NO_RETURN takes 3 arguments:
   // 1)  The function name         <---| Taken from the FFmpeg function prototype
   // 2)  The function arguments    <---|
   // 3)  The argument list to pass to the real function
   //
   // The FFMPEG_INITDYN macro is responsible for retrieving the address of the real function
   // and storing that address in the function pointer variable.  It will emit an error to the
   // currently define log destination and return to the calling function.
   //
#if defined(DEFINE_FFMPEG_POINTERS)
   #define FFX
#else
   #define FFX extern
#endif

#define FFMPEG_FUNCTION_WITH_RETURN(r, n, a, p)                         \
   extern "C"                                                           \
   {                                                                    \
      FFX r (*n ## _fp) a;                                              \
      inline r n a                                                      \
      {                                                                 \
         return n ## _fp p;                                             \
      }                                                                 \
   }

#define FFMPEG_FUNCTION_NO_RETURN(n, a, p)                              \
   extern "C"                                                           \
   {                                                                    \
      FFX void (*n ## _fp) a;                                           \
      inline void n a                                                   \
      {                                                                 \
         n ## _fp p;                                                    \
      }                                                                 \
   }

#define FFMPEG_INITDYN(w, f)                                            \
   {                                                                    \
      wxLogNull off;                                                    \
      *(void**)&f ## _fp = (void*)w->GetSymbol(wxT(#f));                \
   }                                                                    \
   if (f ## _fp == NULL)                                                \
   {                                                                    \
      wxLogError(wxT("Failed to load symbol ") wxT(#f));                \
      return false;                                                     \
   }

#define FFMPEG_INITALT(w, f, x, a)                                      \
   {                                                                    \
      wxLogNull off;                                                    \
      *(void**)&f ## _fp = (void*)w->GetSymbol(wxT(#f));                \
   }                                                                    \
   if (f ## _fp == NULL)                                                \
   {                                                                    \
      {                                                                 \
         wxLogNull off;                                                 \
         *(void**)&f ## _fp = (void*)x->GetSymbol(wxT(#a));             \
      }                                                                 \
      if (f ## _fp == NULL)                                             \
      {                                                                 \
         wxLogError(wxT("Failed to load symbol ") wxT(#f));             \
         return false;                                                  \
      }                                                                 \
   }

   //
   // libavutil
   //
   FFMPEG_FUNCTION_WITH_RETURN(
      unsigned,
      avutil_version,
      (void),
      ()
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_log_set_callback,
      (void (*cb)(void*, int, const char*, va_list)),
      (cb)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_log_default_callback,
      (void* ptr, int level, const char* fmt, va_list vl),
      (ptr, level, fmt, vl)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_free,
      (void *ptr),
      (ptr)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int64_t,
      av_get_default_channel_layout,
      (int nb_channels),
      (nb_channels)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_strerror,
      (int errnum, char *errbuf, size_t errbuf_size),
      (errnum, errbuf, errbuf_size)
   );

   //
   // libavcodec
   //
   FFMPEG_FUNCTION_WITH_RETURN(
      unsigned,
      avcodec_version,
      (void),
      ()
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVCodec*,
      avcodec_find_encoder,
      (enum AVCodecID id),
      (id)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVCodec*,
      avcodec_find_encoder_by_name,
      (const char *name),
      (name)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVCodec*,
      avcodec_find_decoder,
      (enum AVCodecID id),
      (id)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      const char*,
      avcodec_get_name,
      (enum AVCodecID id),
      (id)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      unsigned int,
      av_codec_get_tag,
      (const struct AVCodecTag * const *tags, enum AVCodecID id),
      (tags, id)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_open2,
      (AVCodecContext *avctx, const AVCodec *codec, AVDictionary **options),
      (avctx, codec, options);
   );
#if defined(IS_FFMPEG_PROJECT)
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_decode_audio4,
      (AVCodecContext *avctx, AVFrame *frame, int *got_output, const AVPacket *avpkt),
      (avctx, frame, got_output, avpkt)
   );
#else
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_decode_audio4,
      (AVCodecContext *avctx, AVFrame *frame, int *got_output, AVPacket *avpkt),
      (avctx, frame, got_output, avpkt)
   );
#endif
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_encode_audio2,
      (AVCodecContext *avctx, AVPacket *pkt, const AVFrame *frame, int *got_output),
      (avctx, pkt, frame, got_output)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_close,
      (AVCodecContext *avctx),
      (avctx)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      avcodec_register_all,
      (void),
      ()
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_get_bytes_per_sample,
      (enum AVSampleFormat sample_fmt),
      (sample_fmt)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avcodec_fill_audio_frame,
      (AVFrame *frame, int nb_channels, enum AVSampleFormat sample_fmt, const uint8_t *buf, int buf_size, int align),
      (frame, nb_channels, sample_fmt, buf, buf_size, align)
   );

   //
   // libavformat
   //
   FFMPEG_FUNCTION_WITH_RETURN(
      unsigned,
      avformat_version,
      (void),
      ()
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avformat_open_input,
      (AVFormatContext **ic_ptr, const char *filename, AVInputFormat *fmt, AVDictionary **options),
      (ic_ptr, filename, fmt, options)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_register_all,
      (void),
      ()
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avformat_find_stream_info,
      (AVFormatContext *ic, AVDictionary **options),
      (ic, options)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_read_frame,
      (AVFormatContext *s, AVPacket *pkt),
      (s, pkt)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_seek_frame,
      (AVFormatContext *s, int stream_index, int64_t timestamp, int flags),
      (s, stream_index, timestamp, flags)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      avformat_close_input,
      (AVFormatContext **s),
      (s)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      avformat_write_header,
      (AVFormatContext *s, AVDictionary **options),
      (s, options)
   );
#if LIBAVFORMAT_VERSION_INT >= AV_VERSION_INT(55, 33, 101)
   FFMPEG_FUNCTION_WITH_RETURN(
      AVOutputFormat*,
      av_oformat_next,
      (const AVOutputFormat *f),
      (f)
   );
#else
   FFMPEG_FUNCTION_WITH_RETURN(
      AVOutputFormat*,
      av_oformat_next,
      (AVOutputFormat *f),
      (f)
   );
#endif
   FFMPEG_FUNCTION_WITH_RETURN(
      AVCodec*,
      av_codec_next,
      (const AVCodec *c),
      (c)
   );
#if defined(IS_FFMPEG_PROJECT)
   FFMPEG_FUNCTION_WITH_RETURN(
      AVStream*,
      avformat_new_stream,
      (AVFormatContext *s, const AVCodec *c),
      (s, c)
   );
#else
   FFMPEG_FUNCTION_WITH_RETURN(
      AVStream*,
      avformat_new_stream,
      (AVFormatContext *s, AVCodec *c),
      (s, c)
   );
#endif
   FFMPEG_FUNCTION_WITH_RETURN(
      AVFormatContext*,
      avformat_alloc_context,
      (void),
      ()
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVOutputFormat*,
      av_guess_format,
      (const char *short_name, const char *filename, const char *mime_type),
      (short_name, filename, mime_type)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      avformat_free_context,
      (AVFormatContext *s),
      (s)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_write_trailer,
      (AVFormatContext *s),
      (s)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_interleaved_write_frame,
      (AVFormatContext *s, AVPacket *pkt),
      (s, pkt)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_init_packet,
      (AVPacket *pkt),
      (pkt)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_fifo_generic_write,
      (AVFifoBuffer *f, void *src, int size, int (*func)(void*, void*, int)),
      (f, src, size, func)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_fifo_free,
      (AVFifoBuffer *f),
      (f)
   );
#if LIBAVUTIL_VERSION_MAJOR >= 53
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_fifo_size,
      (const AVFifoBuffer *f),
      (f)
   );
#else
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_fifo_size,
      (AVFifoBuffer *f),
      (f)
   );
#endif
   FFMPEG_FUNCTION_WITH_RETURN(
      void*,
      av_malloc,
      (size_t size),
      (size)
   );
   /*
   FFMPEG_FUNCTION_NO_RETURN(
      av_freep,
      (void *ptr),
      (ptr)
   );
   */
   FFMPEG_FUNCTION_WITH_RETURN(
      int64_t,
      av_rescale_q,
      (int64_t a, AVRational bq, AVRational cq),
      (a, bq, cq)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_free_packet,
      (AVPacket *pkt),
      (pkt)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVFifoBuffer*,
      av_fifo_alloc,
      (unsigned int size),
      (size)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_fifo_generic_read,
      (AVFifoBuffer *f, void *buf, int buf_size, void (*func)(void*, void*, int)),
      (f, buf, buf_size, func)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_fifo_realloc2,
      (AVFifoBuffer *f, unsigned int size),
      (f, size)
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_dict_free,
      (AVDictionary **m),
      (m)
      );
#if LIBAVUTIL_VERSION_MAJOR >= 53
   FFMPEG_FUNCTION_WITH_RETURN(
      AVDictionaryEntry *,
      av_dict_get,
      (const AVDictionary *m, const char *key, const AVDictionaryEntry *prev, int flags),
      (m, key, prev, flags)
   );
#else
   FFMPEG_FUNCTION_WITH_RETURN(
      AVDictionaryEntry *,
      av_dict_get,
      (AVDictionary *m, const char *key, const AVDictionaryEntry *prev, int flags),
      (m, key, prev, flags)
   );
#endif
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_dict_set,
      (AVDictionary **pm, const char *key, const char *value, int flags),
      (pm, key, value, flags)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int64_t,
      avio_size,
      (AVIOContext *s),
      (s)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVIOContext *,
      avio_alloc_context,
      (unsigned char *buffer,
                  int buffer_size,
                  int write_flag,
                  void *opaque,
                  int (*read_packet)(void *opaque, uint8_t *buf, int buf_size),
                  int (*write_packet)(void *opaque, uint8_t *buf, int buf_size),
                  int64_t (*seek)(void *opaque, int64_t offset, int whence)),
      (buffer, buffer_size, write_flag, opaque, read_packet, write_packet, seek)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_codec_is_encoder,
      (const AVCodec *codec),
      (codec)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      AVFrame*,
      av_frame_alloc,
      (void),
      ()
   );
   FFMPEG_FUNCTION_NO_RETURN(
      av_frame_free,
      (AVFrame **frame),
      (frame)
   );
   FFMPEG_FUNCTION_WITH_RETURN(
      int,
      av_samples_get_buffer_size,
      (int *linesize, int nb_channels, int nb_samples, enum AVSampleFormat sample_fmt, int align),
      (linesize, nb_channels, nb_samples, sample_fmt, align)
   );
};


#endif

// Attach some C++ lifetime management to the struct, which owns some memory resources.
struct AVPacketEx : public AVPacket
{
   AVPacketEx()
   {
      av_init_packet(this);
      data = nullptr;
      size = 0;
   }
   AVPacketEx(const AVPacketEx &) PROHIBITED;
   AVPacketEx& operator= (const AVPacketEx&) PROHIBITED;
   AVPacketEx(AVPacketEx &&that)
   {
      steal(std::move(that));
   }
   AVPacketEx &operator= (AVPacketEx &&that)
   {
      if (this != &that) {
         reset();
         steal(std::move(that));
      }
      return *this;
   }

   ~AVPacketEx ()
   {
      reset();
   }

   void reset()
   {
      // This does not deallocate the pointer, but it frees side data.
      av_free_packet(this);
   }

private:
   void steal(AVPacketEx &&that)
   {
      memcpy(this, &that, sizeof(that));
      av_init_packet(&that);
      that.data = nullptr;
      that.size = 0;
   }
};

// utilities for RAII:

// Deleter adaptor for functions like av_free that take a pointer

/// \brief AV_Deleter is part of FFmpeg support.  It's used with the RAII
/// idiom.
template<typename T, typename R, R(*Fn)(T*)> struct AV_Deleter {
   inline R operator() (T* p) const
   {
      R result{};
      if (p)
         result = Fn(p);
      return result;
   }
};

// Specialization of previous for void return
template<typename T, void(*Fn)(T*)>
struct AV_Deleter<T, void, Fn> {
   inline void operator() (T* p) const
   {
      if (p)
         Fn(p);
   }
};

// Deleter adaptor for functions like av_freep that take a pointer to a pointer
template<typename T, typename R, R(*Fn)(T**)> struct AV_Deleterp {
   inline void operator() (T* p) const
   {
      if (p)
         Fn(&p);
   }
};

using AVFrameHolder = std::unique_ptr<
   AVFrame, AV_Deleterp<AVFrame, void, av_frame_free>
>;
using AVFifoBufferHolder = std::unique_ptr<
   AVFifoBuffer, AV_Deleter<AVFifoBuffer, void, av_fifo_free>
>;
using AVFormatContextHolder = std::unique_ptr<
   AVFormatContext, AV_Deleter<AVFormatContext, void, avformat_free_context>
>;
using AVCodecContextHolder = std::unique_ptr<
   AVCodecContext, AV_Deleter<AVCodecContext, int, avcodec_close>
>;
using AVDictionaryCleanup = std::unique_ptr<
   AVDictionary*, AV_Deleter<AVDictionary*, void, av_dict_free>
>;

/// \brief FFmpeg structure to hold a file pointer and provide a return 
/// value when closing the file.
struct UFileHolder : public std::unique_ptr<
   AVIOContext, ::AV_Deleter<AVIOContext, int, ufile_close>
>
{
   UFileHolder() = default;
   UFileHolder( UFileHolder &&that )
   : std::unique_ptr< AVIOContext, ::AV_Deleter<AVIOContext, int, ufile_close> >(
        std::move(that) )
   {
   }

   // Close explicitly, not ignoring return values.
   int close()
   {
      auto result = get_deleter() ( get() );
      release();
      return result;
   }
};

template<typename T> using AVMallocHolder = std::unique_ptr<
   T, AV_Deleter<void, void, av_free>
>;

struct streamContext
{
   bool                 m_use{};                           // TRUE = this stream will be loaded into Audacity
   AVStream            *m_stream{};                        // an AVStream *
   AVCodecContext      *m_codecCtx{};                      // pointer to m_stream->codec

   Optional<AVPacketEx>    m_pkt;                           // the last AVPacket we read for this stream
   uint8_t             *m_pktDataPtr{};                    // pointer into m_pkt.data
   int                  m_pktRemainingSiz{};

   int64_t              m_pts{};                           // the current presentation time of the input stream
   int64_t              m_ptsOffset{};                     // packets associated with stream are relative to this

   int                  m_frameValid{};                    // is m_decodedVideoFrame/m_decodedAudioSamples valid?
   AVMallocHolder<uint8_t> m_decodedAudioSamples;           // decoded audio samples stored here
   unsigned int         m_decodedAudioSamplesSiz{};        // current size of m_decodedAudioSamples
   size_t               m_decodedAudioSamplesValidSiz{};   // # valid bytes in m_decodedAudioSamples
   int                  m_initialchannels{};               // number of channels allocated when we begin the importing. Assumes that number of channels doesn't change on the fly.

   size_t               m_samplesize{};                    // input sample size in bytes
   AVSampleFormat       m_samplefmt{ AV_SAMPLE_FMT_NONE  }; // input sample format

   size_t               m_osamplesize{};                   // output sample size in bytes
   sampleFormat         m_osamplefmt{ floatSample };                    // output sample format

   streamContext() { memset(this, 0, sizeof(*this)); }
   ~streamContext()
   {
   }
};

using Scs = ArrayOf<std::unique_ptr<streamContext>>;
using ScsPtr = std::shared_ptr<Scs>;

extern FFmpegLibs *FFmpegLibsInst();

#endif // USE_FFMPEG
#endif // __AUDACITY_FFMPEG__

