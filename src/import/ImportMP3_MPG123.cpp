/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ImportMP3_MPG123.cpp

  Dmitry Vedenko

*/

#include <wx/defs.h>
#include <cstddef>

#include "Import.h"
#include "BasicUI.h"
#include "ImportPlugin.h"
#include "Project.h"

#define DESC XO("MP3 files")

#include <wx/file.h>
#include <wx/string.h>
#include <wx/log.h>

#include <mpg123.h>

#include "Prefs.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"

#include "CodeConversions.h"

namespace
{

static const auto exts = { wxT("mp3"), wxT("mp2"), wxT("mpa") };

class MP3ImportPlugin final : public ImportPlugin
{
public:
   MP3ImportPlugin()
       : ImportPlugin(
            FileExtensions(exts.begin(), exts.end()))
   {
#if MPG123_API_VERSION < 46
      // Newer versions of the library don't need that anymore, but it is safe
      // to have the no-op call present for compatibility with old versions.
      mpg123_init();
#endif
   }

   wxString GetPluginStringID() override
   {
      return wxT("libmpg123");
   }

   TranslatableString GetPluginFormatDescription() override
   {
      return DESC;
   }

   std::unique_ptr<ImportFileHandle> Open(const FilePath &Filename, AudacityProject*) override;
}; // class MP3ImportPlugin

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

class MP3ImportFileHandle final : public ImportFileHandle
{
public:
   MP3ImportFileHandle(const FilePath &filename);
   ~MP3ImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks, Tags *tags) override;

   void ReadTags(Tags* tags);

   wxInt32 GetStreamCount() override;
   const TranslatableStrings &GetStreamInfo() override;
   void SetStreamUsage(wxInt32 StreamID, bool Use) override;

private:
   bool Open();

private:
   static ptrdiff_t ReadCallback(void* handle, void* buffer, size_t size);
   static off_t SeekCallback(void* handle, off_t offset, int whence);

   wxFile mFile;
   wxFileOffset mFileLen { 0 };

   WaveTrackFactory* mTrackFactory { nullptr };
   NewChannelGroup mChannels;
   unsigned mNumChannels { 0 };

   ProgressResult mUpdateResult { ProgressResult::Success };

   mpg123_handle* mHandle { nullptr };

   friend MP3ImportPlugin;
}; // class MP3ImportFileHandle

std::unique_ptr<ImportFileHandle> MP3ImportPlugin::Open(
   const FilePath &Filename, AudacityProject *)
{
   auto handle = std::make_unique<MP3ImportFileHandle>(Filename);

   if (!handle->Open())
      return nullptr;

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

MP3ImportFileHandle::MP3ImportFileHandle(const FilePath& filename)
    : ImportFileHandle(filename)
{
   int errorCode = MPG123_OK;
   mHandle = mpg123_new(nullptr, &errorCode);

   if (errorCode != MPG123_OK)
   {
      wxLogError(
         "Failed to create MPG123 handle: %s",
         mpg123_plain_strerror(errorCode));

      mHandle = nullptr;

      return;
   }

   errorCode = mpg123_replace_reader_handle(
      mHandle, ReadCallback, SeekCallback, nullptr);

   if (errorCode != MPG123_OK)
   {
      wxLogError(
         "Failed to set reader on the MPG123 handle: %s",
         mpg123_plain_strerror(errorCode));

      mpg123_delete(mHandle);
      mHandle = nullptr;
   }

   // We force mpg123 to decode into floats
   mpg123_param(mHandle, MPG123_FLAGS, MPG123_FORCE_FLOAT, 0.0);

   if (errorCode != MPG123_OK)
   {
      wxLogError(
         "Failed to set options on the MPG123 handle",
         mpg123_plain_strerror(errorCode));

      mpg123_delete(mHandle);
      mHandle = nullptr;
   }
}

MP3ImportFileHandle::~MP3ImportFileHandle()
{
   // nullptr is a valid input for the mpg123_delete
   mpg123_delete(mHandle);
}

TranslatableString MP3ImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto MP3ImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // We have to parse the file first using mpg123_scan,
   // we do not want to do that before the import starts.
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

ProgressResult MP3ImportFileHandle::Import(WaveTrackFactory *trackFactory,
                                           TrackHolders &outTracks,
                                           Tags *tags)
{
   auto finalAction = finally([handle = mHandle]() { mpg123_close(handle); });

   outTracks.clear();
   mTrackFactory = trackFactory;

   CreateProgress();

   int ret = mpg123_scan(mHandle);

   if (ret != MPG123_OK)
   {
      wxLogError(
         "Failed to scan MP3 file: %s", mpg123_plain_strerror(ret));

      return ProgressResult::Failed;
   }

   long long framesCount = mpg123_framelength(mHandle);

   mUpdateResult = mProgress->Update(0ll, framesCount);

   if (mUpdateResult == ProgressResult::Cancelled)
      return ProgressResult::Cancelled;

   off_t frameIndex { 0 };
   unsigned char* data { nullptr };
   size_t dataSize { 0 };

   int encoding = MPG123_ENC_FLOAT_32;

   std::vector<float> conversionBuffer;

   while ((ret = mpg123_decode_frame(mHandle, &frameIndex, &data, &dataSize)) ==
                 MPG123_OK ||
          ret == MPG123_NEW_FORMAT)
   {
      mUpdateResult = mProgress->Update(
         static_cast<long long>(frameIndex), framesCount);

      if (mUpdateResult == ProgressResult::Cancelled)
         return ProgressResult::Cancelled;

      if (ret == MPG123_NEW_FORMAT)
      {
         long rate;
         int channels;
         mpg123_getformat(mHandle, &rate, &channels, &encoding);

         mNumChannels = channels == MPG123_MONO ? 1 : 2;
         mChannels.resize(mNumChannels);

         if (encoding != MPG123_ENC_FLOAT_32 && encoding != MPG123_ENC_FLOAT_64)
         {
            wxLogError("MPG123 returned unexpected encoding");

            return ProgressResult::Failed;
         }

         for (unsigned i = 0; i < mNumChannels; ++i)
            mChannels[i] = NewWaveTrack(*mTrackFactory, floatSample, rate);

         continue;
      }

      constSamplePtr samples = reinterpret_cast<constSamplePtr>(data);
      const size_t samplesCount = dataSize / sizeof(float) / mNumChannels;

      // libmpg123 picks up the format based on some "internal" precision.
      // This case is not expected to happen
      if (encoding == MPG123_ENC_FLOAT_64)
      {
         conversionBuffer.resize(samplesCount * mNumChannels);

         for (size_t sampleIndex = 0; sampleIndex < conversionBuffer.size();
              ++sampleIndex)
         {
            conversionBuffer[sampleIndex] = static_cast<float>(
               reinterpret_cast<const double*>(data)[sampleIndex]);
         }

         samples = reinterpret_cast<constSamplePtr>(conversionBuffer.data());
      }
      // Just copy the interleaved data to the channels
      for (unsigned channel = 0; channel < mNumChannels; ++channel)
      {
         mChannels[channel]->Append(
            samples + sizeof(float) * channel, floatSample, samplesCount,
            mNumChannels);
      }
   }

   if (ret != MPG123_DONE)
   {
      wxLogError(
         "Failed to decode MP3 file: %s", mpg123_plain_strerror(ret));

      return ProgressResult::Failed;
   }

   // Flush and trim the channels
   for (const auto &channel : mChannels)
      channel->Flush();

   // Copy the WaveTrack pointers into the Track pointer list that
   // we are expected to fill
   outTracks.push_back(std::move(mChannels));

   ReadTags(tags);

   return mUpdateResult;
}

void MP3ImportFileHandle::ReadTags(Tags* tags)
{
   mpg123_id3v1* v1;
   mpg123_id3v2* v2;
   int meta;

   meta = mpg123_meta_check(mHandle);

   if (meta & MPG123_ID3 && mpg123_id3(mHandle, &v1, &v2) == MPG123_OK)
   {
      if (v2 != nullptr && v2->title != nullptr && v2->title->fill > 0)
         tags->SetTag(TAG_TITLE, audacity::ToWXString(v2->title->p));
      else if (v1 != nullptr && v1->title[0] != '\0')
         tags->SetTag(TAG_TITLE, audacity::ToWXString(v1->title));

      if (v2 != nullptr && v2->artist != nullptr && v2->artist->fill > 0)
         tags->SetTag(TAG_ARTIST, audacity::ToWXString(v2->artist->p));
      else if (v1 != nullptr && v1->artist[0] != '\0')
         tags->SetTag(TAG_ARTIST, audacity::ToWXString(v1->artist));

      if (v2 != nullptr && v2->album != nullptr && v2->album->fill > 0)
         tags->SetTag(TAG_ALBUM, audacity::ToWXString(v2->album->p));
      else if (v1 != nullptr && v1->album[0] != '\0')
         tags->SetTag(TAG_ALBUM, audacity::ToWXString(v1->album));

      if (v2 != nullptr && v2->year != nullptr && v2->year->fill > 0)
         tags->SetTag(TAG_YEAR, audacity::ToWXString(v2->year->p));
      else if (v1 != nullptr && v1->year[0] != '\0')
         tags->SetTag(TAG_YEAR, audacity::ToWXString(std::string(v1->year, 4)));

      // ID2V2 genre can be quite complex:
      // (from https://id3.org/id3v2.3.0)
      // References to the ID3v1 genres can be made by, as first byte, enter
      // "(" followed by a number from the genres list (appendix A) and ended
      // with a ")" character. This is optionally followed by a refinement,
      // e.g. "(21)" or "(4)Eurodisco". Several references can be made in the
      // same frame, e.g. "(51)(39)". However, Audacity only supports one
      // genre, so we just skip ( a parse the number afterwards.
      if (v2 != nullptr && v2->genre != nullptr && v2->genre->fill > 0)
         tags->SetTag(TAG_GENRE, tags->GetGenre(std::stoi(v2->genre->p + 1)));
      else if (v1 != nullptr)
         tags->SetTag(TAG_GENRE, tags->GetGenre(v1->genre));

      if (v2 != nullptr && v2->comment != nullptr && v2->comment->fill > 0)
         tags->SetTag(TAG_COMMENTS, audacity::ToWXString(v2->comment->p));
      else if (v1 != nullptr && v1->comment[0] != '\0')
         tags->SetTag(TAG_COMMENTS, audacity::ToWXString(v1->comment));

      if (v2 != nullptr)
      {
         for (size_t i = 0; i < v2->comments; ++i)
         {
            if (v2->comment_list[i].text.fill == 0)
               continue;

            tags->SetTag(
               audacity::ToWXString(std::string(v2->comment_list[i].id, 4)),
               audacity::ToWXString(v2->comment_list[i].text.p));
         }

         for (size_t i = 0; i < v2->extras; ++i)
         {
            if (v2->extra[i].text.fill == 0)
               continue;

            tags->SetTag(
               audacity::ToWXString(std::string(v2->extra[i].id, 4)),
               audacity::ToWXString(v2->extra[i].text.p));
         }

         // libmpg123 does not parse TRCK tag, we have to do it ourselves
         for (size_t i = 0; i < v2->texts; ++i)
         {
            if (memcmp(v2->text[i].id, "TRCK", 4) == 0)
            {
               tags->SetTag(
                  TAG_TRACK, audacity::ToWXString(v2->text[i].text.p));
            }
         }
      }
   }
}

bool MP3ImportFileHandle::Open()
{
   if (mHandle == nullptr)
      return false;

   // Open the file
   if (!mFile.Open(mFilename))
   {
      return false;
   }

   // Get the length of the file
   mFileLen = mFile.Seek(0, wxFromEnd);

   if (mFileLen == wxInvalidOffset || mFile.Error())
   {
      mFile.Close();
      return false;
   }

   if (mFile.Seek(0, wxFromStart) == wxInvalidOffset || mFile.Error())
   {
      mFile.Close();
      return false;
   }

   // Check if file is an MP3
   auto errorCode = mpg123_open_handle(mHandle, this);

   if (errorCode != MPG123_OK)
   {
      wxLogError(
         "Failed to open MP3 file: %s", mpg123_plain_strerror(errorCode));

      return false;
   }

   return true;
}

ptrdiff_t MP3ImportFileHandle::ReadCallback(
   void* handle, void* buffer, size_t size)
{
   return static_cast<MP3ImportFileHandle*>(handle)->mFile.Read(buffer, size);
}

wxSeekMode GetWXSeekMode(int whence)
{
   switch (whence)
   {
   case SEEK_SET:
      return wxFromStart;
   case SEEK_CUR:
      return wxFromCurrent;
   case SEEK_END:
      return wxFromEnd;
   default:
      // We have covered all the lseek whence modes defined by POSIX
      // so this branch should not be reachable
      assert(false);
      return wxFromStart;
   }
}

off_t MP3ImportFileHandle::SeekCallback(
   void* handle, off_t offset, int whence)
{
   return static_cast<MP3ImportFileHandle*>(handle)->mFile.Seek(
      offset, GetWXSeekMode(whence));
}

} // namespace
