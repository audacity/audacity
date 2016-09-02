/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3.cpp

  Joshua Haberman
  Leland Lucius

*//****************************************************************//**

\class MP3ImportFileHandle
\brief An ImportFileHandle for MP3 data

  Audacity has finally moved to using a single mp3 library on all
  platforms! It is the high performance, beautifully written libmad
  (mpeg audio decoder). Finally there is harmony in the mp3 universe.

  Much of this source code is based on 'minimad.c' as distributed
  with libmad.

*//****************************************************************//**

\class MP3ImportPlugin
\brief An ImportPlugin for MP3 data

*//*******************************************************************/

#include "../Audacity.h"
#include "ImportMP3.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/window.h>
#endif

#include <wx/defs.h>
#include <wx/intl.h>

#include "../Prefs.h"
#include "Import.h"
#include "ImportPlugin.h"
#include "../Internat.h"
#include "../Tags.h"

#define DESC _("MP3 files")

static const wxChar *exts[] =
{
   wxT("mp3"),
   wxT("mp2"),
   wxT("mpa")
};

#ifndef USE_LIBMAD

void GetMP3ImportPlugin(ImportPluginList &importPluginList,
                        UnusableImportPluginList &unusableImportPluginList)
{
   unusableImportPluginList.push_back(
      make_movable<UnusableImportPlugin>
         (DESC, wxArrayString(WXSIZEOF(exts), exts))
  );
}

#else /* USE_LIBMAD */

#include <wx/textctrl.h>
#include <wx/file.h>
#include <wx/thread.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/timer.h>
#include <wx/intl.h>

extern "C" {
#include "mad.h"

#ifdef USE_LIBID3TAG
#include <id3tag.h>
#endif
}

#include "../WaveTrack.h"

#define INPUT_BUFFER_SIZE 65535
#define PROGRESS_SCALING_FACTOR 100000

/* this is a private structure we can use for whatever we like, and it will get
 * passed to each of the callback routines, allowing us to keep track of
 * things. */
struct private_data {
   wxFile *file;            /* the file containing the mp3 data we're feeding the encoder */
   unsigned char *inputBuffer;
   TrackFactory *trackFactory;
   TrackHolders channels;
   ProgressDialog *progress;
   unsigned numChannels;
   int updateResult;
   bool id3checked;
};

class MP3ImportPlugin final : public ImportPlugin
{
public:
   MP3ImportPlugin():
      ImportPlugin(wxArrayString(WXSIZEOF(exts), exts))
   {
   }

   ~MP3ImportPlugin() { }

   wxString GetPluginStringID() { return wxT("libmad"); }
   wxString GetPluginFormatDescription();
   std::unique_ptr<ImportFileHandle> Open(const wxString &Filename) override;
};

class MP3ImportFileHandle final : public ImportFileHandle
{
public:
   MP3ImportFileHandle(std::unique_ptr<wxFile> &&file, wxString filename):
      ImportFileHandle(filename),
      mFile(std::move(file))
   {
   }

   ~MP3ImportFileHandle();

   wxString GetFileDescription();
   ByteCount GetFileUncompressedBytes() override;
   int Import(TrackFactory *trackFactory, TrackHolders &outTracks,
              Tags *tags) override;

   wxInt32 GetStreamCount(){ return 1; }

   const wxArrayString &GetStreamInfo() override
   {
      static wxArrayString empty;
      return empty;
   }

   void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)){}

private:
   void ImportID3(Tags *tags);

   std::unique_ptr<wxFile> mFile;
   void *mUserData;
   struct private_data mPrivateData;
   mad_decoder mDecoder;
};

void GetMP3ImportPlugin(ImportPluginList &importPluginList,
                        UnusableImportPluginList & WXUNUSED(unusableImportPluginList))
{
   importPluginList.push_back( make_movable<MP3ImportPlugin>() );
}

/* The MAD callbacks */
enum mad_flow input_cb(void *_data, struct mad_stream *stream);
enum mad_flow output_cb(void *_data,
                        struct mad_header const *header,
                        struct mad_pcm *pcm);
enum mad_flow error_cb(void *_data, struct mad_stream *stream,
                       struct mad_frame *frame);

/* convert libmad's fixed point representation to 16 bit signed integers. This
 * code is taken verbatim from minimad.c. */

inline float scale(mad_fixed_t sample)
{
   return (float) (sample / (float) (1L << MAD_F_FRACBITS));
}


wxString MP3ImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> MP3ImportPlugin::Open(const wxString &Filename)
{
   auto file = std::make_unique<wxFile>(Filename);

   if (!file->IsOpened())
      return nullptr;

   /* There's no way to tell if this is a valid mp3 file before actually
    * decoding, so we return a valid FileHandle. */

   return std::make_unique<MP3ImportFileHandle>(std::move(file), Filename);
}

wxString MP3ImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto MP3ImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO
   return 0;
}

int MP3ImportFileHandle::Import(TrackFactory *trackFactory, TrackHolders &outTracks,
                                Tags *tags)
{
   outTracks.clear();

   CreateProgress();

   /* Prepare decoder data, initialize decoder */

   mPrivateData.file        = mFile.get();
   mPrivateData.inputBuffer = new unsigned char [INPUT_BUFFER_SIZE];
   mPrivateData.progress    = mProgress.get();
   mPrivateData.updateResult= eProgressSuccess;
   mPrivateData.id3checked  = false;
   mPrivateData.numChannels = 0;
   mPrivateData.trackFactory= trackFactory;

   mad_decoder_init(&mDecoder, &mPrivateData, input_cb, 0, 0, output_cb, error_cb, 0);

   /* and send the decoder on its way! */

   bool res = (mad_decoder_run(&mDecoder, MAD_DECODER_MODE_SYNC) == 0) &&
              (mPrivateData.numChannels > 0) &&
              !(mPrivateData.updateResult == eProgressCancelled) &&
              !(mPrivateData.updateResult == eProgressFailed);

   mad_decoder_finish(&mDecoder);

   delete[] mPrivateData.inputBuffer;

   if (!res) {
      /* failure */
      /* printf("failure\n"); */
      return (mPrivateData.updateResult);
   }

   /* success */
   /* printf("success\n"); */

      /* copy the WaveTrack pointers into the Track pointer list that
       * we are expected to fill */
   for(const auto &channel : mPrivateData.channels) {
      channel->Flush();
   }
   outTracks.swap(mPrivateData.channels);

   /* Read in any metadata */
   ImportID3(tags);

   return mPrivateData.updateResult;
}

MP3ImportFileHandle::~MP3ImportFileHandle()
{
}

void MP3ImportFileHandle::ImportID3(Tags *tags)
{
#ifdef USE_LIBID3TAG
   wxFile f;   // will be closed when it goes out of scope
   struct id3_file *fp = NULL;

   if (f.Open(mFilename)) {
      // Use id3_file_fdopen() instead of id3_file_open since wxWidgets can open a
      // file with a Unicode name and id3_file_open() can't (under Windows).
      fp = id3_file_fdopen(f.fd(), ID3_FILE_MODE_READONLY);
   }

   if (!fp) {
      return;
   }

   // The file descriptor is now owned by "fp", so we must tell "f" to forget
   // about it.
   f.Detach();

   struct id3_tag *tp = id3_file_tag(fp);
   if (!tp) {
      id3_file_close(fp);
      return;
   }

   tags->Clear();

   // Loop through all frames
   bool have_year = false;
   for (int i = 0; i < (int) tp->nframes; i++) {
      struct id3_frame *frame = tp->frames[i];

      // printf("ID: %08x '%4s'\n", (int) *(int *)frame->id, frame->id);
      // printf("Desc: %s\n", frame->description);
      // printf("Num fields: %d\n", frame->nfields);

      // for (int j = 0; j < (int) frame->nfields; j++) {
      //    printf("field %d type %d\n", j, frame->fields[j].type );
      //    if (frame->fields[j].type == ID3_FIELD_TYPE_STRINGLIST) {
      //       printf("num strings %d\n", frame->fields[j].stringlist.nstrings);
      //    }
      // }

      wxString n, v;

      // Determine the tag name
      if (strcmp(frame->id, ID3_FRAME_TITLE) == 0) {
         n = TAG_TITLE;
      }
      else if (strcmp(frame->id, ID3_FRAME_ARTIST) == 0) {
         n = TAG_ARTIST;
      }
      else if (strcmp(frame->id, ID3_FRAME_ALBUM) == 0) {
         n = TAG_ALBUM;
      }
      else if (strcmp(frame->id, ID3_FRAME_TRACK) == 0) {
         n = TAG_TRACK;
      }
      else if (strcmp(frame->id, ID3_FRAME_YEAR) == 0) {
         // LLL:  When libid3tag encounters the "TYER" tag, it converts it to a
         //       "ZOBS" (obsolete) tag and adds a "TDRC" tag at the end of the
         //       list of tags using the first 4 characters of the "TYER" tag.
         //       Since we write both the "TDRC" and "TYER" tags, the "TDRC" tag
         //       will always be encountered first in the list.  We want use it
         //       since the converted "TYER" tag may have been truncated.
         if (have_year) {
            continue;
         }
         n = TAG_YEAR;
         have_year = true;
      }
      else if (strcmp(frame->id, ID3_FRAME_COMMENT) == 0) {
         n = TAG_COMMENTS;
      }
      else if (strcmp(frame->id, ID3_FRAME_GENRE) == 0) {
         n = TAG_GENRE;
      }
   else {
         // Use frame description as default tag name.  The descriptions
         // may include several "meanings" separated by "/" characters, so
         // we just use the first meaning
         n = UTF8CTOWX(frame->description).BeforeFirst(wxT('/'));
      }

      const id3_ucs4_t *ustr = NULL;

      if (n == TAG_COMMENTS) {
         ustr = id3_field_getfullstring(&frame->fields[3]);
      }
      else if (frame->nfields == 3) {
         ustr = id3_field_getstring(&frame->fields[1]);
         if (ustr) {
            char *str = (char *)id3_ucs4_utf8duplicate(ustr);
            n = UTF8CTOWX(str);
            free(str);
         }

         ustr = id3_field_getstring(&frame->fields[2]);
      }
      else if (frame->nfields >= 2) {
         ustr = id3_field_getstrings(&frame->fields[1], 0);
      }

      if (ustr) {
         char *str = (char *)id3_ucs4_utf8duplicate(ustr);
         v = UTF8CTOWX(str);
         free(str);
      }

      if (!n.IsEmpty() && !v.IsEmpty()) {
         tags->SetTag(n, v);
   }
}

   // Convert v1 genre to name
   if (tags->HasTag(TAG_GENRE)) {
      long g = -1;
      if (tags->GetTag(TAG_GENRE).ToLong(&g)) {
         tags->SetTag(TAG_GENRE, tags->GetGenre(g));
   }
}

   id3_file_close(fp);
#endif // ifdef USE_LIBID3TAG
}

//
//   MAD Callbacks
//

/* The input callback is called when the decoder wants more data. */

enum mad_flow input_cb(void *_data, struct mad_stream *stream)
{
   struct private_data *data = (struct private_data *)_data;

   data->updateResult = data->progress->Update((wxULongLong_t)data->file->Tell(),
                                             (wxULongLong_t)data->file->Length() != 0 ?
                                             (wxULongLong_t)data->file->Length() : 1);
   if(data->updateResult != eProgressSuccess)
      return MAD_FLOW_STOP;

   if(data->file->Eof()) {
      return MAD_FLOW_STOP;
   }

#ifdef USE_LIBID3TAG
   if (!data->id3checked) {
      data->file->Read(data->inputBuffer, ID3_TAG_QUERYSIZE);
      int len = id3_tag_query(data->inputBuffer, ID3_TAG_QUERYSIZE);
      if (len > 0) {
         data->file->Seek(len, wxFromStart);
      }
      else {
         data->file->Seek(0);
      }

      data->id3checked = true;
   }
#endif

   /* "Each time you refill your buffer, you need to preserve the data in
    *  your existing buffer from stream.next_frame to the end.
    *
    *  This usually amounts to calling memmove() on this unconsumed portion
    *  of the buffer and appending NEW data after it, before calling
    *  mad_stream_buffer()"
    *           -- Rob Leslie, on the mad-dev mailing list */

   unsigned int unconsumedBytes;
   if(stream->next_frame) {
      unconsumedBytes = data->inputBuffer + INPUT_BUFFER_SIZE - stream->next_frame;
      memmove(data->inputBuffer, stream->next_frame, unconsumedBytes);
   }
   else
      unconsumedBytes = 0;


   off_t read = data->file->Read(data->inputBuffer + unconsumedBytes,
                                 INPUT_BUFFER_SIZE - unconsumedBytes);

   mad_stream_buffer(stream, data->inputBuffer, read + unconsumedBytes);

   return MAD_FLOW_CONTINUE;
}

/* The output callback is called every time the decoder has finished decoding
 * a frame, allowing us to use the decoded data */

enum mad_flow output_cb(void *_data,
                        struct mad_header const * WXUNUSED(header),
                        struct mad_pcm *pcm)
{
   int samplerate;
   struct private_data *data = (struct private_data *)_data;
   int smpl;

   samplerate= pcm->samplerate;
   auto channels  = pcm->channels;
   const auto samples   = pcm->length;

   /* If this is the first run, we need to create the WaveTracks that
    * will hold the data.  We do this now because now is the first
    * moment when we know how many channels there are. */

   if(data->channels.empty()) {
      data->channels.resize(channels);

      sampleFormat format = (sampleFormat) gPrefs->
         Read(wxT("/SamplingRate/DefaultProjectSampleFormat"), floatSample);

      for(auto &channel: data->channels) {
         channel = data->trackFactory->NewWaveTrack(format, samplerate);
         channel->SetChannel(Track::MonoChannel);
      }

      /* special case: 2 channels is understood to be stereo */
      if(channels == 2) {
         data->channels.begin()->get()->SetChannel(Track::LeftChannel);
         data->channels.rbegin()->get()->SetChannel(Track::RightChannel);
         data->channels.begin()->get()->SetLinked(true);
      }
      data->numChannels = channels;
   }
   else {
      // This is not the first run, protect us from libmad glitching
      // on the number of channels
      channels = data->numChannels;
   }

   /* TODO: get rid of this by adding fixed-point support to SampleFormat.
    * For now, we allocate temporary float buffers to convert the fixed
    * point samples into something we can feed to the WaveTrack.  Allocating
    * big blocks of data like this isn't a great idea, but it's temporary.
    */
   float **channelBuffers = new float* [channels];
   for(int chn = 0; chn < channels; chn++)
      channelBuffers[chn] = new float [samples];

   for(smpl = 0; smpl < samples; smpl++)
      for(int chn = 0; chn < channels; chn++)
         channelBuffers[chn][smpl] = scale(pcm->samples[chn][smpl]);

   auto iter = data->channels.begin();
   for (int chn = 0; chn < channels; ++iter, ++chn)
      iter->get()->Append((samplePtr)channelBuffers[chn],
                                  floatSample,
                                  samples);

   for(int chn = 0; chn < channels; chn++)
      delete[] channelBuffers[chn];
   delete[] channelBuffers;

   return MAD_FLOW_CONTINUE;
}

enum mad_flow error_cb(void * WXUNUSED(_data), struct mad_stream * WXUNUSED(stream),
                       struct mad_frame * WXUNUSED(frame))
{
/* enum mad_flow {
     MAD_FLOW_CONTINUE = 0x0000,
     MAD_FLOW_STOP     = 0x0010,
     MAD_FLOW_BREAK    = 0x0011,
     MAD_FLOW_IGNORE   = 0x0020
   }; */
   /*
   printf("decoding error 0x%04x (%s)\n",
      stream->error, mad_stream_errorstr(stream));
   */

   return MAD_FLOW_CONTINUE;

   /* return MAD_FLOW_BREAK; */
}


#endif                          /* defined(USE_LIBMAD) */
