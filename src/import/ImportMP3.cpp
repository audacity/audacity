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

#include "../Audacity.h" // for USE_* macros

#include <wx/defs.h>

#include "Import.h"
#include "ImportPlugin.h"
#include "../Project.h"

#define DESC XO("MP3 files")

static const auto exts =
{
   wxT("mp3"),
   wxT("mp2"),
   wxT("mpa")
};

#ifndef USE_LIBMAD

static Importer::RegisteredUnusableImportPlugin registered
{
   std::make_unique<UnusableImportPlugin>(DESC, FileExtensions(exts.begin(), exts.end()))
};

#else

#if defined(__WXMSW__)
#include <malloc.h>
#else
#include <stdlib.h>
#endif

#include <wx/file.h>
#include <wx/string.h>

#include "../Prefs.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../prefs/QualityPrefs.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"

// PRL:  include these last,
// and correct some preprocessor namespace pollution from wxWidgets that
// caused a warning about duplicate definition
#undef SIZEOF_LONG
extern "C"
{
#include "mad.h"

#ifdef USE_LIBID3TAG
#include <id3tag.h>
#endif
}

// Specifies the number of bytes in the input buffer.  This also controls
// how many bytes will be scanned when searching for the first MP3 frame.
#define INPUT_BUFFER_SIZE 65535

// This is the number of decoded samples libmad adds at the beginning
// (This is an "observed" value.)
#define MAD_DELAY 529

class MP3ImportPlugin final : public ImportPlugin
{
public:
   MP3ImportPlugin();
   ~MP3ImportPlugin();

   wxString GetPluginStringID() override;
   TranslatableString GetPluginFormatDescription() override;
   std::unique_ptr<ImportFileHandle> Open(const FilePath &Filename, AudacityProject*) override;
};

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

class MP3ImportFileHandle final : public ImportFileHandle
{
public:
   MP3ImportFileHandle(const FilePath &filename);
   ~MP3ImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(TrackFactory *trackFactory, TrackHolders &outTracks, Tags *tags) override;
   wxInt32 GetStreamCount() override;
   const TranslatableStrings &GetStreamInfo() override;
   void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
   bool Open();
   bool CheckID3();
   bool CheckMP3();
   bool FillBuffer();
   void LoadID3(Tags *tags);

   // The MAD callbacks

   static mad_flow input_cb(void *that,
                            struct mad_stream *stream);
   mad_flow InputCB(struct mad_stream *stream);

   static mad_flow filter_cb(void *that,
                             struct mad_stream const *stream,
                             struct mad_frame *frame);
   mad_flow FilterCB(struct mad_stream const *stream, struct mad_frame *frame);

   static mad_flow output_cb(void *that,
                             struct mad_header const *header,
                             struct mad_pcm *pcm);
   mad_flow OutputCB(struct mad_header const *header, struct mad_pcm *pcm);

   static mad_flow error_cb(void *that,
                            struct mad_stream *stream,
                            struct mad_frame *frame);
   mad_flow ErrorCB(struct mad_stream *stream, struct mad_frame *frame);

private:
   mad_decoder mDecoder;

   wxFile mFile;
   wxFileOffset mFilePos;
   wxFileOffset mFileLen;

   unsigned char mInputBuffer[INPUT_BUFFER_SIZE + MAD_BUFFER_GUARD];
   int mInputBufferLen;

   TrackFactory *mTrackFactory;
   NewChannelGroup mChannels;
   unsigned mNumChannels;

   ProgressResult mUpdateResult;

   int mDelay;
   int mPadding;

   bool mHaveID3;

   friend MP3ImportPlugin;
};

// ============================================================================
// MP3ImportPlugin
// ============================================================================

MP3ImportPlugin::MP3ImportPlugin()
:  ImportPlugin(FileExtensions(exts.begin(), exts.end()))
{
}

MP3ImportPlugin::~MP3ImportPlugin()
{
}

wxString MP3ImportPlugin::GetPluginStringID()
{
   return wxT("libmad");
}

TranslatableString MP3ImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> MP3ImportPlugin::Open(
   const FilePath &Filename, AudacityProject *)
{
   auto handle = std::make_unique<MP3ImportFileHandle>(Filename);

   if (!handle->Open())
   {
      return nullptr;
   }

   return handle;
}

static Importer::RegisteredImportPlugin registered
{
   "MP3",
   std::make_unique<MP3ImportPlugin>()
};

// ============================================================================
// MP3ImportFileHandle
// ============================================================================

MP3ImportFileHandle::MP3ImportFileHandle(const FilePath &filename)
:  ImportFileHandle(filename)
{
}

MP3ImportFileHandle::~MP3ImportFileHandle()
{
}

TranslatableString MP3ImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto MP3ImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO
   return 0;
}

wxInt32 MP3ImportFileHandle::GetStreamCount()
{
   return 1;
}

const TranslatableStrings &MP3ImportFileHandle::GetStreamInfo()
{
   static TranslatableStrings empty;
   return empty;
}

void MP3ImportFileHandle::SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use))
{
}

ProgressResult MP3ImportFileHandle::Import(TrackFactory *trackFactory,
                                           TrackHolders &outTracks,
                                           Tags *tags)
{
   outTracks.clear();

   CreateProgress();

   mTrackFactory = trackFactory;
   mUpdateResult = ProgressResult::Success;
   mNumChannels = 0;

   // Set delay and padding to best possible in case the LAME tag is not present
   mDelay = MAD_DELAY;
   mPadding = 0;

   // Initialize decoder
   mad_decoder_init(&mDecoder, this, input_cb, 0, filter_cb, output_cb, error_cb, 0);

   // Send the decoder on its way!
   auto res = mad_decoder_run(&mDecoder, MAD_DECODER_MODE_SYNC);

   // Terminate decoder
   mad_decoder_finish(&mDecoder);

   // Decoding failed, so pass it on
   if (res != 0)
   {
      return ProgressResult::Failed;
   }

   // The user canceled the decoding, so bail without saving tracks or tags
   if (mUpdateResult == ProgressResult::Cancelled)
   {
      return mUpdateResult;
   }

   // Flush and trim the channels
   for (const auto &channel : mChannels)
   {
      channel->Flush();

      // Trim any padding
      if (mPadding)
      {
         double et = channel->GetEndTime();
         double t1 = et - channel->LongSamplesToTime(mPadding);
         channel->Clear(t1, et);
      }

      // And delay
      if (mDelay)
      {
         double st = channel->GetStartTime();
         double t0 = st + channel->LongSamplesToTime(mDelay);
         channel->Clear(st, t0);
      }
   }

   // Copy the WaveTrack pointers into the Track pointer list that
   // we are expected to fill
   outTracks.push_back(std::move(mChannels));

   // Load ID3 tags from the file
   LoadID3(tags);

   return mUpdateResult;
}

bool MP3ImportFileHandle::Open()
{
   mInputBufferLen = 0;
   mFilePos = 0;
   mFileLen = 0;
   mHaveID3 = false;

   // Open the file
   if (!mFile.Open(mFilename))
   {
      return false;
   }

   // Check for ID3 tags
   if (!CheckID3())
   {
      return false;
   }

   // Scan for the first MP3 frame
   if (!CheckMP3())
   {
      return false;
   }

   return true;
}

bool MP3ImportFileHandle::CheckID3()
{
   wxFileOffset mp3Start = 0;

   // An ID3v2 tag header is 10 bytes
   if (mFile.Read(mInputBuffer, 10) != 10 || mFile.Error())
   {
      // An error if we can't read 10 bytes or a true error occurred
      wxLogMessage(wxT("Couldn't read 10 bytes while searching for ID3v2 header"));
      return false;
   }

   // Does it look like an ID3v2 tag?
   if (memcmp(mInputBuffer, "ID3", 3) == 0)
   {
      // Get and decode the length
      wxFileOffset len = (mInputBuffer[6] & 0x7f);
      len = (len << 7) | (mInputBuffer[7] & 0x7f);
      len = (len << 7) | (mInputBuffer[8] & 0x7f);
      len = (len << 7) | (mInputBuffer[9] & 0x7f);

      // Start of MP3 should 10 for the ID3 header + length of tags
      mp3Start = 10 + len;

      // Remember that we have tags
      mHaveID3 = true;
   }

   // ID3v1 tags always start 128 bytes from the end of a file
   mFileLen = mFile.Seek(-128, wxFromEnd);
   if (mFileLen == wxInvalidOffset || mFile.Error())
   {
      // An error if we can't seek to 128 before end (MP3 files will always be
      // longer than this) or a true error occurred
      wxLogMessage(wxT("Couldn't seek to ID3v1 header"));
      return false;
   }

   // Read in possible header
   if (mFile.Read(mInputBuffer, 3) != 3 || mFile.Error())
   {
      // An error if we can't read the ID3v1 header
      wxLogMessage(wxT("Couldn't read 3 bytes while searching for the ID3v1 tag"));
      return false;
   }

   // Does it look like an ID3v1 tag?
   if (memcmp(mInputBuffer, "TAG", 3) == 0)
   {
      // Remember that we have tags
      mHaveID3 = true;
   }
   else
   {
      // No, so adjust file length
      mFileLen += 128;
   }

   // Look for an ID3v2 trailer tag (should probably try to load them but I can't find
   // any sample files or apps to test with)
   if (mFile.Seek(mFileLen - 10, wxFromStart) == wxInvalidOffset || mFile.Error())
   {
      // An error if we can't seek to 10 before end or before the v1 tag (MP3 files will
      // always be longer than this) or a true error occurred
      wxLogMessage(wxT("Couldn't seek while searching for ID3v2 footer"));
      return false;
   }

   // An ID3v2 tag footer is 10 bytes
   if (mFile.Read(mInputBuffer, 10) != 10 || mFile.Error())
   {
      // An error if we can't read the footer or a true error occurred
      wxLogMessage(wxT("Couldn't read 10 bytes while searching for ID3v2 footer"));
      return false;
   }

   // Does it look like a footer tag?
   if (memcmp(mInputBuffer, "3DI", 3) == 0)
   {
      // Get and decode the length
      wxFileOffset len = (mInputBuffer[6] & 0x7f);
      len = (len << 7) | (mInputBuffer[7] & 0x7f);
      len = (len << 7) | (mInputBuffer[8] & 0x7f);
      len = (len << 7) | (mInputBuffer[9] & 0x7f);

      // Yes, so "remove" tags from the file's length
      // (10 for the 3DI header, len for the tag data, and 10 more for the ID3 header)
      mFileLen -= (10 + len + 10);

      // Remember that we have tags
      mHaveID3 = true;
   }

   // Return to the end of the ID3v2 tags
   if (mFile.Seek(mp3Start, wxFromStart) == wxInvalidOffset || mFile.Error())
   {
      // An error if we can't seek to the start of the file or a true error occurred
      wxLogMessage(wxT("Couldn't seek to beginning of the MP3 frames"));
      return false;
   }

   // Finally, remove the length of any ID3v2 tags from the file's length
   mFileLen -= mp3Start;

   // Reset input controls
   mFilePos = 0;
   mInputBufferLen = 0;

   return true;
}

bool MP3ImportFileHandle::CheckMP3()
{
   // Load as much as will fit into the buffer
   if (!FillBuffer())
   {
      return false;
   }

   // Scan the input buffer for an MP3 frame.  We limit it to the input buffer len
   // minus 4 since we need to examine the full header and a header is 4 bytes.
   auto header = mInputBuffer;
   for (int i = 0, limit = mInputBufferLen - 4; i < limit; ++i, ++header)
   {
      if ((header[0] & 0xff) != 0xff)
      {
         continue;            // first 8 bits must be '1'
      }

      if ((header[1] & 0xe0) != 0xe0)
      {
         continue;            // first 3 bits must be '1'
      }

      if ((header[1] & 0x18) == 0x08)
      {
         continue;            // not MPEG-1, -2 or -2.5
      }

      switch (header[1] & 0x06)
      {
      case 0x02:              // Layer 3
      case 0x04:              // Layer 2
      case 0x06:              // Layer 1
         break;

      default:
         continue;            // no/invalid Layer I, II and III
      }

      if ((header[2] & 0xf0) == 0xf0)
      {
         continue;            // bad bitrate
      }

      if ((header[2] & 0x0c) == 0x0c)
      {
         continue;            // no sample frequency with (32,44.1,48)/(1,2,4)
      }

      static const char abl2[16] = { 0, 7, 7, 7, 0, 7, 0, 0, 0, 0, 0, 8, 8, 8, 8, 8 };
      if ((header[1] & 0x18) == 0x18 && (header[1] & 0x06) == 0x04 && abl2[header[2] >> 4] & (1 << (header[3] >> 6)))
      {
         continue;            // bad Layer II sample frequency
      }

      if ((header[3] & 0xc0) != 0x40 && (header[3] & 0x30) != 0)
      {
         continue;            // mode extension when not joint stereo
      }

      if ((header[3] & 0x03) == 0x02)
      {
         continue;            // reserved enphasis mode
      }

      // Resync the real file position to the located frame
      if (mFile.Seek(i - mFilePos, wxFromCurrent) == wxInvalidOffset || mFile.Error())
      {
         // An error if we can't seek to the start of the file or a true error occurred
         wxLogMessage(wxT("Couldn't resync to the first MP3 frame"));
         return false;
      }

      // Reset file controls
      mFilePos = 0;
      mFileLen -= i;
      mInputBufferLen = 0;

      // Log the number of bytes skipped
      if (i)
      {
         wxLogMessage(wxT("Skipped %d bytes while searching for first MP3 frame."), i);
         return false;
      }

      return true;
   }

   // Didn't find the first MP3 frame...give up.
   return false;
}

bool MP3ImportFileHandle::FillBuffer()
{
   // We either want enough to fill the input buffer or what's left in the file
   auto want = wxMin(INPUT_BUFFER_SIZE - mInputBufferLen, mFileLen - mFilePos);
   if (want > 0)
   {
      // We should always get what we ask for
      auto got = mFile.Read(&mInputBuffer[mInputBufferLen], want);
      if (got != want || mFile.Error())
      {
         return false;
      }

      // Adjust input control
      mInputBufferLen += got;
      mFilePos += got;
   }

   // MAD requires that we add MAD_BUFFER_GUARD extra bytes when we've processed
   // all of the MP3 frames.  Otherwise, we will drop the last frame.
   if (mFilePos == mFileLen)
   {
      memset(&mInputBuffer[mInputBufferLen], 0, MAD_BUFFER_GUARD);
      mInputBufferLen += MAD_BUFFER_GUARD;
   }

   return true;
}

void MP3ImportFileHandle::LoadID3(Tags *tags)
{
#ifdef USE_LIBID3TAG
   tags->Clear();

   struct id3_file *id3file = NULL;
   auto cleanup = finally([&]
   {
      if (id3file)
      {
         id3_file_close(id3file);
      }
   });

   // Use id3_file_fdopen() instead of id3_file_open since wxWidgets can open a
   // file with a Unicode name and id3_file_open() can't (under Windows).
   id3file = id3_file_fdopen(mFile.fd(), ID3_FILE_MODE_READONLY);
   if (!id3file)
   {
      return;
   }

   // The file descriptor is now owned by "id3file", so we must tell "mFile" to forget
   // about it.
   mFile.Detach();

   // Load the tags
   struct id3_tag *id3tags = id3_file_tag(id3file);
   if (!id3tags)
   {
      return;
   }

   // Convert from libid3tag's ucs4 type to wxString.
   //
   // The ucs4 type is unsigned long which can be 8 bytes instead
   // of the expected 4 bytes for a UTF-32 character, so we have
   // to convert to unsigned int and then to wxString.
   wxMBConvUTF32 converter;
   auto toString = [=](const id3_ucs4_t *in)
   {
      // Count the number of characters
      size_t len = 0;
      for (const id3_ucs4_t *p = in; *p; p++)
      {
         len++;
      }

      // Would like to use std::dynarray or runtime-sized array,
      // but VS doesn't support either.
      wxUint32 *buf = (wxUint32 *) alloca((len + 1) * sizeof(wxUint32));

      // Copy and convert to unsigned int
      wxUint32 *out;
      for (out = buf; *in; in++, out++)
      {
         *out = (wxUint32) (*in);
      }
      *out = 0;

      // Finally convert to and return wxString
      return wxString((char *) buf, converter);
   };

   // Extract tags from ID3 frames and add to our tags
   bool have_year = false;
   for (unsigned int i = 0; i < id3tags->nframes; ++i)
   {
      struct id3_frame *frame = id3tags->frames[i];

#if 0
      wxLogDebug("ID: %08x '%4s'", (int) *(int *)frame->id, frame->id);
      wxLogDebug("Desc: %s", frame->description);
      wxLogDebug("Num fields: %d", frame->nfields);

      for (unsigned int j = 0; j < frame->nfields; ++j)
      {
         wxLogDebug("field %d type %d", j, frame->fields[j].type);
         if (frame->fields[j].type == ID3_FIELD_TYPE_STRINGLIST)
         {
            wxLogDebug("num strings %d", frame->fields[j].stringlist.nstrings);
         }
      }
#endif

      wxString n;
      wxString v;

      // Determine the tag name
      if (strcmp(frame->id, ID3_FRAME_TITLE) == 0)
      {
         n = TAG_TITLE;
      }
      else if (strcmp(frame->id, ID3_FRAME_ARTIST) == 0)
      {
         n = TAG_ARTIST;
      }
      else if (strcmp(frame->id, ID3_FRAME_ALBUM) == 0)
      {
         n = TAG_ALBUM;
      }
      else if (strcmp(frame->id, ID3_FRAME_TRACK) == 0)
      {
         n = TAG_TRACK;
      }
      else if (strcmp(frame->id, ID3_FRAME_YEAR) == 0)
      {
         // LLL:  When libid3tag encounters the "TYER" tag, it converts it to a
         //       "ZOBS" (obsolete) tag and adds a "TDRC" tag at the end of the
         //       list of tags using the first 4 characters of the "TYER" tag.
         //       Since we write both the "TDRC" and "TYER" tags, the "TDRC" tag
         //       will always be encountered first in the list.  We want to use
         //       it since the converted "TYER" tag may have been truncated.
         if (have_year)
         {
            continue;
         }
         n = TAG_YEAR;
         have_year = true;
      }
      else if (strcmp(frame->id, ID3_FRAME_COMMENT) == 0)
      {
         n = TAG_COMMENTS;
      }
      else if (strcmp(frame->id, ID3_FRAME_GENRE) == 0)
      {
         n = TAG_GENRE;
      }
      else
      {
         // Use frame description as default tag name.  The descriptions
         // may include several "meanings" separated by "/" characters, so
         // we just use the first meaning
         n = UTF8CTOWX(frame->description).BeforeFirst(wxT('/'));
      }

      // Now get the tag value
      const id3_ucs4_t *ustr = NULL;

      if (n == TAG_COMMENTS)
      {
         ustr = id3_field_getfullstring(&frame->fields[3]);
      }
      else if (frame->nfields == 3)
      {
         ustr = id3_field_getstring(&frame->fields[1]);
         if (ustr)
         {
            n = toString(ustr);
         }

         ustr = id3_field_getstring(&frame->fields[2]);
      }
      else if (frame->nfields >= 2)
      {
         ustr = id3_field_getstrings(&frame->fields[1], 0);
      }

      // Convert the value 
      if (ustr)
      {
         v = toString(ustr);
      }

      // And add it to the list of tags
      if (!n.empty() && !v.empty())
      {
         tags->SetTag(n, v);
      }
   }

   // Convert v1 genre to name
   if (tags->HasTag(TAG_GENRE))
   {
      long g = -1;
      if (tags->GetTag(TAG_GENRE).ToLong(&g))
      {
         tags->SetTag(TAG_GENRE, tags->GetGenre(g));
      }
   }
#else
   (void) tags;
#endif
}

//
// MAD Callbacks
//

// The input callback is called when the decoder wants more data
mad_flow MP3ImportFileHandle::input_cb(void *that,
                                       struct mad_stream *stream)
{
   auto cb = [&]()
   {
      return ((MP3ImportFileHandle *) that)->InputCB(stream);
   };

   return GuardedCall<mad_flow>(cb, MakeSimpleGuard(MAD_FLOW_BREAK));
}

mad_flow MP3ImportFileHandle::InputCB(struct mad_stream *stream)
{
   // Update the progress
   mUpdateResult = mProgress->Update((wxLongLong_t) mFilePos, (wxLongLong_t) mFileLen);
   if (mUpdateResult != ProgressResult::Success)
   {
      return MAD_FLOW_STOP;
   }

   // Stop if we've consumed all of the MP3 data
   if (mFilePos == mFileLen)
   {
      return MAD_FLOW_STOP;
   }

   // "Each time you refill your buffer, you need to preserve the data in
   // your existing buffer from stream.next_frame to the end.
   //
   // This usually amounts to calling memmove() on this unconsumed portion
   // of the buffer and appending NEW data after it, before calling
   // mad_stream_buffer()
   //    -- Rob Leslie, on the mad-dev mailing list
   if (stream->next_frame)
   {
      mInputBufferLen -= (stream->next_frame - mInputBuffer);
      memmove(mInputBuffer, stream->next_frame, mInputBufferLen);
   }

   // Refill the buffer
   if (!FillBuffer())
   {
      return MAD_FLOW_BREAK;
   }

   // And give it back to MAD
   mad_stream_buffer(stream, mInputBuffer, mInputBufferLen);

   return MAD_FLOW_CONTINUE;
}

// The filter callback let's us examine each frame and decide if it should be
// kept or tossed.  We use this to detect the Xing or LAME tags.
mad_flow MP3ImportFileHandle::filter_cb(void *that,
                                        struct mad_stream const *stream,
                                        struct mad_frame *frame)
{
   auto cb = [&]()
   {
      return ((MP3ImportFileHandle *) that)->FilterCB(stream, frame);
   };

   return GuardedCall<mad_flow>(cb, MakeSimpleGuard(MAD_FLOW_BREAK));
}

enum mad_flow MP3ImportFileHandle::FilterCB(struct mad_stream const *stream,
                                            struct mad_frame *frame)
{
   // We only want to inspect the first frame, so disable future calls
   mDecoder.filter_func = nullptr;

   // Get the ancillary data ptr and length
   auto ptr = stream->anc_ptr.byte;
   int len = stream->anc_bitlen / 8;

   // Ensure it's something we can understand
   if (len < 4 || (memcmp(ptr, "Xing", 4) != 0 && memcmp(ptr, "Info", 4) != 0))
   {
      return MAD_FLOW_CONTINUE;
   }

   // Skip the tag
   ptr += 4;
   len -= 4;

   enum flagBits
   {
      hasFrames   = 0x0001,
      hasBytes    = 0x0002,
      hasToc      = 0x0004,
      hasScale    = 0x0008
   };

   // Extract the flags
   unsigned int flags = (((((ptr[0] << 8) + ptr[1]) << 8) + ptr[2]) << 8) + ptr[3];
   ptr += 4;
   len -= 4;

   // Skip the number of frames
   if (len >= 4 && flags & hasFrames)
   {
      ptr += 4;
      len -= 4;
   }

   // Skip the number of bytes
   if (len >= 4 && flags & hasBytes)
   {
      ptr += 4;
      len -= 4;
   }

   // Skip the TOC
   if (len >= 100 && flags & hasToc)
   {
      ptr += 100;
      len -= 100;
   }

   // Skip the VBR Scale
   if (len >= 4 && flags && hasScale)
   {
      ptr += 4;
      len -= 4;
   }

   // Bail if LAME wasn't the encoder or we don't have enough ancillary data left
   if (len < 24 || memcmp(ptr, "LAME", 4) != 0)
   {
      return MAD_FLOW_IGNORE;
   }

   // Skip down to the delay and padding
   ptr += 21;
   len -= 21;

   // Extract the delay and padding and adjust for decoder delay
   mDelay = (ptr[0] << 4) + (ptr[1] >> 4) + MAD_DELAY;
   mPadding = ((ptr[1] & 0x0f) << 8) + ptr[2] - MAD_DELAY;
   if (mPadding < 0)
   {
      mPadding = 0;
   }

   return MAD_FLOW_IGNORE;
}

// The output callback is called every time the decoder has finished decoding
 // a frame, allowing us to use the decoded data
mad_flow MP3ImportFileHandle::output_cb(void *that,
                                        struct mad_header const *header,
                                        struct mad_pcm *pcm)
{
   auto cb = [&]()
   {
      return ((MP3ImportFileHandle *) that)->OutputCB(header, pcm);
   };

   return GuardedCall<mad_flow>(cb, MakeSimpleGuard(MAD_FLOW_BREAK));
}

enum mad_flow MP3ImportFileHandle::OutputCB(struct mad_header const * WXUNUSED(header),
                                            struct mad_pcm *pcm)
{
   // If this is the first run, we need to create the WaveTracks that
   // will hold the data.  We do this now because now is the first
   // moment when we know how many channels there are.
   if (mChannels.empty())
   {
      mNumChannels = pcm->channels;

      mChannels.resize(mNumChannels);

      auto format = QualityPrefs::SampleFormatChoice();

      for (auto &channel: mChannels)
      {
         channel = mTrackFactory->NewWaveTrack(format, pcm->samplerate);
      }
   }

   // Get the number of samples in each channel
   auto samples = pcm->length;

   // Convert libmad samples to float and append to WaveTracks
   for (int chn = 0; chn < mNumChannels; ++chn)
   {
      // Number of samples will never be more than 1152
      float sampleBuf[1152];
      wxASSERT(samples <= 1152);

      // Copy over the samples
      for (int sample = 0; sample < samples; ++sample)
      {
         // Convert libmad's fixed point representation to float
         sampleBuf[sample] = ((float) pcm->samples[chn][sample] / (1L << MAD_F_FRACBITS));
      }

      // And append to the channel
      mChannels[chn]->Append((samplePtr) sampleBuf, floatSample, samples);
   }

   return MAD_FLOW_CONTINUE;
}

// The error callback is used when MAD encounters an error and needs to know
// how it should proceed
mad_flow MP3ImportFileHandle::error_cb(void *that,
                                       struct mad_stream *stream,
                                       struct mad_frame *frame)
{
   auto cb = [&]()
   {
      return ((MP3ImportFileHandle *) that)->ErrorCB(stream, frame);
   };

   return GuardedCall<mad_flow>(cb, MakeSimpleGuard(MAD_FLOW_BREAK));
}

enum mad_flow MP3ImportFileHandle::ErrorCB(struct mad_stream *stream,
                                           struct mad_frame *frame)
{
   // You always get a LOSTSYNC error at EOF, so just ignore it
   if (stream->error == MAD_ERROR_LOSTSYNC && mFilePos == mFileLen)
   {
      return MAD_FLOW_CONTINUE;
   }

   // This can happen when parsing the first frame. We can use the number of channels
   // for it hasn't yet been determined.
   if (stream->error == MAD_ERROR_BADDATAPTR && mNumChannels == 0)
   {
      return MAD_FLOW_CONTINUE;
   }

   // Let the user know about the error
   AudacityMessageBox(XO("MP3 Decoding Failed:\n\n%s").Format(mad_stream_errorstr(stream)));

   return MAD_FLOW_BREAK;
}

#endif
