/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportOGG.cpp

  Joshua Haberman
  Leland Lucius

*//****************************************************************//**

\class ImportFileHandle
\brief An ImportFileHandle for data

  The Ogg format supports multiple logical bitstreams that can be chained
  within the physical bitstream. The sampling rate and number of channels
  can vary between these logical bitstreams. For the moment, we'll ignore
  all but the first logical bitstream.

  Ogg also allows for an arbitrary number of channels. Luckily, so does
  Audacity. We'll call the first channel LeftChannel, the second
  RightChannel, and all others after it MonoChannel.

*//****************************************************************//**

\class OGGImportPlugin
\brief An ImportPlugin for OGG data

*//*******************************************************************/



// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
#include <wx/window.h>
#endif

#include <wx/intl.h>
#include "Import.h"
#include "Prefs.h"
#include "../Tags.h"
#include "../widgets/ProgressDialog.h"


#define DESC XO("Ogg Vorbis files")

static const auto exts = {
   wxT("ogg")
};

#ifndef USE_LIBVORBIS
/* BPF There is no real reason to compile without LIBVORBIS, but if you do, you will needs this header */
#include "ImportPlugin.h"

static Importer::RegisteredUnusableImportPlugin registered{
      std::make_unique<UnusableImportPlugin>
         (DESC, FileExtensions( exts.begin(), exts.end() ) )
};

#else /* USE_LIBVORBIS */

#include <wx/log.h>
#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
/* ffile.h must be included AFTER at least one other wx header that includes
 * wx/setup.h, otherwise #ifdefs erroneously collapse it to nothing. This is
 * a bug in wxWidgets (ffile.h should itself include wx/setup.h), and it
 * was a bitch to track down. */
#include <wx/ffile.h>

#include <vorbis/vorbisfile.h>

#include "../WaveTrack.h"
#include "ImportPlugin.h"

using NewChannelGroup = std::vector< std::shared_ptr<WaveTrack> >;

class OggImportPlugin final : public ImportPlugin
{
public:
   OggImportPlugin()
   :  ImportPlugin( FileExtensions( exts.begin(), exts.end() ) )
   {
   }

   ~OggImportPlugin() { }

   wxString GetPluginStringID() override { return wxT("liboggvorbis"); }
   TranslatableString GetPluginFormatDescription() override;
   std::unique_ptr<ImportFileHandle> Open(
      const FilePath &Filename, AudacityProject*) override;
};


class OggImportFileHandle final : public ImportFileHandle
{
public:
   OggImportFileHandle(const FilePath & filename,
                       std::unique_ptr<wxFFile> &&file,
                       std::unique_ptr<OggVorbis_File> &&vorbisFile)
   :  ImportFileHandle(filename),
      mFile(std::move(file)),
      mVorbisFile(std::move(vorbisFile))
      , mStreamUsage{ static_cast<size_t>(mVorbisFile->links) }
   {
      for (int i = 0; i < mVorbisFile->links; i++)
      {
         auto strinfo = XO("Index[%02x] Version[%d], Channels[%d], Rate[%ld]")
            .Format(
               (unsigned int) i,
               mVorbisFile->vi[i].version,
               mVorbisFile->vi[i].channels,
               mVorbisFile->vi[i].rate);
         mStreamInfo.push_back(strinfo);
         mStreamUsage[i] = 0;
      }

   }
   ~OggImportFileHandle();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(WaveTrackFactory *trackFactory, TrackHolders &outTracks,
              Tags *tags) override;

   wxInt32 GetStreamCount() override
   {
      if (mVorbisFile)
         return mVorbisFile->links;
      else
         return 0;
   }

   const TranslatableStrings &GetStreamInfo() override
   {
      return mStreamInfo;
   }

   void SetStreamUsage(wxInt32 StreamID, bool Use) override
   {
      if (mVorbisFile)
      {
         if (StreamID < mVorbisFile->links)
            mStreamUsage[StreamID] = (Use ? 1 : 0);
      }
   }

private:
   std::unique_ptr<wxFFile> mFile;
   std::unique_ptr<OggVorbis_File> mVorbisFile;

   ArrayOf<int> mStreamUsage;
   TranslatableStrings mStreamInfo;
   std::list<NewChannelGroup> mChannels;
};


TranslatableString OggImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> OggImportPlugin::Open(
   const FilePath &filename, AudacityProject*)
{
   // Suppress some compiler warnings about unused global variables in the library header
   wxUnusedVar(OV_CALLBACKS_DEFAULT);
   wxUnusedVar(OV_CALLBACKS_NOCLOSE);
   wxUnusedVar(OV_CALLBACKS_STREAMONLY);
   wxUnusedVar(OV_CALLBACKS_STREAMONLY_NOCLOSE);

   auto vorbisFile = std::make_unique<OggVorbis_File>();
   auto file = std::make_unique<wxFFile>(filename, wxT("rb"));

   if (!file->IsOpened()) {
      // No need for a message box, it's done automatically (but how?)
      return nullptr;
   }

   int err = ov_open(file->fp(), vorbisFile.get(), NULL, 0);

   if (err < 0) {
      TranslatableString message;

      switch (err) {
         case OV_EREAD:
            message = XO("Media read error");
            break;
         case OV_ENOTVORBIS:
            message = XO("Not an Ogg Vorbis file");
            break;
         case OV_EVERSION:
            message = XO("Vorbis version mismatch");
            break;
         case OV_EBADHEADER:
            message = XO("Invalid Vorbis bitstream header");
            break;
         case OV_EFAULT:
            message = XO("Internal logic fault");
            break;
      }

      // what to do with message?
      return nullptr;
   }

   return std::make_unique<OggImportFileHandle>(filename, std::move(file), std::move(vorbisFile));
}

static Importer::RegisteredImportPlugin registered{ "OGG",
   std::make_unique< OggImportPlugin >()
};

TranslatableString OggImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto OggImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO:
   return 0;
}

ProgressResult OggImportFileHandle::Import(
   WaveTrackFactory *trackFactory, TrackHolders &outTracks,
   Tags *tags)
{
   outTracks.clear();

   wxASSERT(mFile->IsOpened());

   CreateProgress();

   //Number of streams used may be less than mVorbisFile->links,
   //but this way bitstream matches array index.
   mChannels.resize(mVorbisFile->links);

   int i = -1;
   for (auto &link : mChannels)
   {
      ++i;

      //Stream is not used
      if (mStreamUsage[i] == 0)
      {
         //This is just a padding to keep bitstream number and
         //array indices matched.
         continue;
      }

      vorbis_info *vi = ov_info(mVorbisFile.get(), i);

      link.resize(vi->channels);

      for (auto &channel : link)
         // The format agrees with what is always passed to Append() below
         channel = NewWaveTrack(*trackFactory, int16Sample, vi->rate);
   }

   /* The number of bytes to get from the codec in each run */
#define CODEC_TRANSFER_SIZE 4096u

   /* The number of samples to read between calls to the callback.
    * Balance between responsiveness of the GUI and throughput of import. */
#define SAMPLES_PER_CALLBACK 100000

   auto updateResult = ProgressResult::Success;
   long bytesRead = 0;
   {
      ArrayOf<short> mainBuffer{ CODEC_TRANSFER_SIZE };

      /* determine endianness (clever trick courtesy of Nicholas Devillard,
       * (http://www.eso.org/~ndevilla/endian/) */
      int testvar = 1, endian;
      if (*(char *)&testvar)
         endian = 0;  // little endian
      else
         endian = 1;  // big endian

      /* number of samples currently in each channel's buffer */
      long samplesRead = 0;
      int bitstream = 0;
      int samplesSinceLastCallback = 0;

      // You would think that the stream would already be seeked to 0, and
      // indeed it is if the file is legit.  But I had several ogg files on
      // my hard drive that have malformed headers, and this added call
      // causes them to be read correctly.  Otherwise they have lots of
      // zeros inserted at the beginning
      ov_pcm_seek(mVorbisFile.get(), 0);

      do {
         /* get data from the decoder */
         bytesRead = ov_read(mVorbisFile.get(), (char *)mainBuffer.get(),
            CODEC_TRANSFER_SIZE,
            endian,
            2,    // word length (2 for 16 bit samples)
            1,    // signed
            &bitstream);

         if (bytesRead == OV_HOLE) {
            wxFileName ff(mFilename);
            wxLogError(wxT("Ogg Vorbis importer: file %s is malformed, ov_read() reported a hole"),
               ff.GetFullName());
            /* http://lists.xiph.org/pipermail/vorbis-dev/2001-February/003223.html
             * is the justification for doing this - best effort for malformed file,
             * hence the message.
             */
            continue;
         }
         else if (bytesRead < 0) {
            /* Malformed Ogg Vorbis file. */
            /* TODO: Return some sort of meaningful error. */
            wxLogError(wxT("Ogg Vorbis importer: ov_read() returned error %i"),
               bytesRead);
            break;
         }

         samplesRead = bytesRead / mVorbisFile->vi[bitstream].channels / sizeof(short);

         /* give the data to the wavetracks */
         auto iter = mChannels.begin();
         std::advance(iter, bitstream);
         if (mStreamUsage[bitstream] != 0)
         {
            auto iter2 = iter->begin();
            for (int c = 0; c < mVorbisFile->vi[bitstream].channels; ++iter2, ++c)
               iter2->get()->Append((char *)(mainBuffer.get() + c),
               int16Sample,
               samplesRead,
               mVorbisFile->vi[bitstream].channels);
         }

         samplesSinceLastCallback += samplesRead;
         if (samplesSinceLastCallback > SAMPLES_PER_CALLBACK) {
            updateResult = mProgress->Update(ov_time_tell(mVorbisFile.get()),
               ov_time_total(mVorbisFile.get(), bitstream));
            samplesSinceLastCallback -= SAMPLES_PER_CALLBACK;
         }
      } while (updateResult == ProgressResult::Success && bytesRead != 0);
   }

   auto res = updateResult;
   if (bytesRead < 0)
     res = ProgressResult::Failed;

   if (res == ProgressResult::Failed || res == ProgressResult::Cancelled) {
      return res;
   }

   for (auto &link : mChannels)
   {
      for (auto &channel : link)
         channel->Flush();
      outTracks.push_back(std::move(link));
   }

   //\todo { Extract comments from each stream? }
   if (mVorbisFile->vc[0].comments > 0) {
      tags->Clear();
      for (int c = 0; c < mVorbisFile->vc[0].comments; c++) {
         wxString comment = UTF8CTOWX(mVorbisFile->vc[0].user_comments[c]);
         wxString name = comment.BeforeFirst(wxT('='));
         wxString value = comment.AfterFirst(wxT('='));
         if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
            long val;
            if (value.length() == 4 && value.ToLong(&val)) {
               name = TAG_YEAR;
            }
         }
         tags->SetTag(name, value);
      }
   }

   return res;
}

OggImportFileHandle::~OggImportFileHandle()
{
   ov_clear(mVorbisFile.get());
   mFile->Detach();    // so that it doesn't try to close the file (ov_clear()
                       // did that already)
}

#endif                          /* USE_LIBVORBIS */
