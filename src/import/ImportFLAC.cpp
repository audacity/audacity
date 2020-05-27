/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportFLAC.cpp

  Copyright 2004  Sami Liedes
  Leland Lucius

  Based on ImportPCM.cpp by Dominic Mazzoni
  Licensed under the GNU General Public License v2 or later

*//****************************************************************//**

\class FLACImportFileHandle
\brief An ImportFileHandle for FLAC data

*//****************************************************************//**

\class FLACImportPlugin
\brief An ImportPlugin for FLAC data

*//*******************************************************************/

#include "../Audacity.h" // for USE_* macros

#include "../Experimental.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#ifndef WX_PRECOMP
// Include your minimal set of headers here, or wx.h
#include <wx/window.h>
#endif

#include <wx/defs.h>
#include <wx/intl.h>    // needed for _("translated stings") even if we
                        // don't have libflac available

#include "Import.h"
#include "ImportPlugin.h"

#include "../Tags.h"
#include "../WaveClip.h"
#include "../blockfile/ODDecodeBlockFile.h"
#include "../prefs/QualityPrefs.h"
#include "../widgets/ProgressDialog.h"

#define FLAC_HEADER "fLaC"

#define DESC XO("FLAC files")

static const auto exts = {
   wxT("flac"),
   wxT("flc")
};

#ifndef USE_LIBFLAC

static Importer::RegisteredUnusableImportPlugin registered{
      std::make_unique<UnusableImportPlugin>
         (DESC, FileExtensions( exts.begin(), exts.end() ) )
};

#else /* USE_LIBFLAC */

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/file.h>
#include <wx/ffile.h>

#include "FLAC++/decoder.h"

#include "../FileFormats.h"
#include "../Prefs.h"
#include "../WaveTrack.h"
#include "ImportPlugin.h"
#include "../ondemand/ODDecodeFlacTask.h"
#include "../ondemand/ODManager.h"

#ifdef USE_LIBID3TAG
extern "C" {
#include <id3tag.h>
}
#endif

/* FLACPP_API_VERSION_CURRENT is 6 for libFLAC++ from flac-1.1.3 (see <FLAC++/export.h>) */
#if !defined FLACPP_API_VERSION_CURRENT || FLACPP_API_VERSION_CURRENT < 6
#define LEGACY_FLAC
#else
#undef LEGACY_FLAC
#endif


class FLACImportFileHandle;

class MyFLACFile final : public FLAC::Decoder::File
{
 public:
   MyFLACFile(FLACImportFileHandle *handle) : mFile(handle)
   {
      mWasError = false;
      set_metadata_ignore_all();
      set_metadata_respond(FLAC__METADATA_TYPE_VORBIS_COMMENT);
      set_metadata_respond(FLAC__METADATA_TYPE_STREAMINFO);
   }

   bool get_was_error() const
   {
      return mWasError;
   }
 private:
   friend class FLACImportFileHandle;
   FLACImportFileHandle *mFile;
   bool                  mWasError;
   wxArrayString         mComments;
 protected:
   FLAC__StreamDecoderWriteStatus write_callback(const FLAC__Frame *frame,
                                                         const FLAC__int32 * const buffer[]) override;
   void metadata_callback(const FLAC__StreamMetadata *metadata) override;
   void error_callback(FLAC__StreamDecoderErrorStatus status) override;
};


class FLACImportPlugin final : public ImportPlugin
{
 public:
   FLACImportPlugin():
   ImportPlugin( FileExtensions( exts.begin(), exts.end() ) )
   {
   }

   ~FLACImportPlugin() { }

   wxString GetPluginStringID() override { return wxT("libflac"); }
   TranslatableString GetPluginFormatDescription() override;
   std::unique_ptr<ImportFileHandle> Open(
      const FilePath &Filename, AudacityProject*)  override;
};


class FLACImportFileHandle final : public ImportFileHandle
{
   friend class MyFLACFile;
public:
   FLACImportFileHandle(const FilePath & name);
   ~FLACImportFileHandle();

   bool Init();

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;
   ProgressResult Import(TrackFactory *trackFactory, TrackHolders &outTracks,
              Tags *tags) override;

   wxInt32 GetStreamCount() override { return 1; }

   const TranslatableStrings &GetStreamInfo() override
   {
      static TranslatableStrings empty;
      return empty;
   }

   void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)) override
   {}

private:
   sampleFormat          mFormat;
   std::unique_ptr<MyFLACFile> mFile;
   wxFFile               mHandle;
   unsigned long         mSampleRate;
   unsigned long         mNumChannels;
   unsigned long         mBitsPerSample;
   FLAC__uint64          mNumSamples;
   FLAC__uint64          mSamplesDone;
   bool                  mStreamInfoDone;
   ProgressResult        mUpdateResult;
   NewChannelGroup       mChannels;
   std::unique_ptr<ODDecodeFlacTask> mDecoderTask;
};


void MyFLACFile::metadata_callback(const FLAC__StreamMetadata *metadata)
{
   switch (metadata->type)
   {
      case FLAC__METADATA_TYPE_VORBIS_COMMENT:
         for (FLAC__uint32 i = 0; i < metadata->data.vorbis_comment.num_comments; i++) {
            mComments.push_back(UTF8CTOWX((char *)metadata->data.vorbis_comment.comments[i].entry));
         }
      break;

      case FLAC__METADATA_TYPE_STREAMINFO:
         mFile->mSampleRate=metadata->data.stream_info.sample_rate;
         mFile->mNumChannels=metadata->data.stream_info.channels;
         mFile->mBitsPerSample=metadata->data.stream_info.bits_per_sample;
         mFile->mNumSamples=metadata->data.stream_info.total_samples;

         if (mFile->mBitsPerSample<=16) {
            if (mFile->mFormat<int16Sample) {
               mFile->mFormat=int16Sample;
            }
         } else if (mFile->mBitsPerSample<=24) {
            if (mFile->mFormat<int24Sample) {
               mFile->mFormat=int24Sample;
            }
         } else {
            mFile->mFormat=floatSample;
         }
         mFile->mStreamInfoDone=true;
      break;
      // handle the other types we do nothing with to avoid a warning
      case FLAC__METADATA_TYPE_PADDING:	// do nothing with padding
      case FLAC__METADATA_TYPE_APPLICATION:	// no idea what to do with this
      case FLAC__METADATA_TYPE_SEEKTABLE:	// don't need a seektable here
      case FLAC__METADATA_TYPE_CUESHEET:	// convert this to labels?
      case FLAC__METADATA_TYPE_PICTURE:		// ignore pictures
      case FLAC__METADATA_TYPE_UNDEFINED:	// do nothing with this either

      // FIXME: not declared when compiling on Ubuntu.
      //case FLAC__MAX_METADATA_TYPE: // quiet compiler warning with this line
      default:
      break;
   }
}

void MyFLACFile::error_callback(FLAC__StreamDecoderErrorStatus WXUNUSED(status))
{
   mWasError = true;

   /*
   switch (status)
   {
   case FLAC__STREAM_DECODER_ERROR_STATUS_LOST_SYNC:
      wxPrintf(wxT("Flac Error: Lost sync\n"));
      break;
   case FLAC__STREAM_DECODER_ERROR_STATUS_FRAME_CRC_MISMATCH:
      wxPrintf(wxT("Flac Error: Crc mismatch\n"));
      break;
   case FLAC__STREAM_DECODER_ERROR_STATUS_BAD_HEADER:
      wxPrintf(wxT("Flac Error: Bad Header\n"));
      break;
   default:
      wxPrintf(wxT("Flac Error: Unknown error code\n"));
      break;
   }*/
}

FLAC__StreamDecoderWriteStatus MyFLACFile::write_callback(const FLAC__Frame *frame,
                                                          const FLAC__int32 * const buffer[])
{
   // Don't let C++ exceptions propagate through libflac
   return GuardedCall< FLAC__StreamDecoderWriteStatus > ( [&] {
      auto tmp = ArrayOf< short >{ frame->header.blocksize };

      auto iter = mFile->mChannels.begin();
      for (unsigned int chn=0; chn<mFile->mNumChannels; ++iter, ++chn) {
         if (frame->header.bits_per_sample == 16) {
            for (unsigned int s=0; s<frame->header.blocksize; s++) {
               tmp[s]=buffer[chn][s];
            }

            iter->get()->Append((samplePtr)tmp.get(),
                     int16Sample,
                     frame->header.blocksize);
         }
         else {
            iter->get()->Append((samplePtr)buffer[chn],
                     int24Sample,
                     frame->header.blocksize);
         }
      }

      mFile->mSamplesDone += frame->header.blocksize;

      mFile->mUpdateResult = mFile->mProgress->Update((wxULongLong_t) mFile->mSamplesDone, mFile->mNumSamples != 0 ? (wxULongLong_t)mFile->mNumSamples : 1);
      if (mFile->mUpdateResult != ProgressResult::Success)
      {
         return FLAC__STREAM_DECODER_WRITE_STATUS_ABORT;
      }

      return FLAC__STREAM_DECODER_WRITE_STATUS_CONTINUE;
   }, MakeSimpleGuard(FLAC__STREAM_DECODER_WRITE_STATUS_ABORT) );
}

TranslatableString FLACImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}


std::unique_ptr<ImportFileHandle> FLACImportPlugin::Open(
   const FilePath &filename, AudacityProject*)
{
   // First check if it really is a FLAC file

   int cnt;
   wxFile binaryFile;
   if (!binaryFile.Open(filename)) {
      return nullptr; // File not found
   }

   // FIXME: TRAP_ERR wxFILE ops in FLAC Import could fail.
   // Seek() return value is not examined, for example.
#ifdef USE_LIBID3TAG
   // Skip any ID3 tags that might be present
   id3_byte_t query[ID3_TAG_QUERYSIZE];
   cnt = binaryFile.Read(query, sizeof(query));
   cnt = id3_tag_query(query, cnt);
   binaryFile.Seek(cnt);
#endif

   char buf[5];
   cnt = binaryFile.Read(buf, 4);
   binaryFile.Close();

   if (cnt == wxInvalidOffset || strncmp(buf, FLAC_HEADER, 4) != 0) {
      // File is not a FLAC file
      return nullptr;
   }

   // Open the file for import
   auto handle = std::make_unique<FLACImportFileHandle>(filename);

   bool success = handle->Init();
   if (!success) {
      return nullptr;
   }

   // This std::move is needed to "upcast" the pointer type
   return std::move(handle);
}

static Importer::RegisteredImportPlugin registered{ "FLAC",
   std::make_unique< FLACImportPlugin >()
};

FLACImportFileHandle::FLACImportFileHandle(const FilePath & name)
:  ImportFileHandle(name),
   mSamplesDone(0),
   mStreamInfoDone(false),
   mUpdateResult(ProgressResult::Success)
{
   mFormat = QualityPrefs::SampleFormatChoice();
   mFile = std::make_unique<MyFLACFile>(this);
}

bool FLACImportFileHandle::Init()
{
#ifdef EXPERIMENTAL_OD_FLAC
   mDecoderTask = std::make_unique<ODDecodeFlacTask>();

   ODFlacDecoder* odDecoder = (ODFlacDecoder*)mDecoderTask->CreateFileDecoder(mFilename);
   if(!odDecoder || !odDecoder->ReadHeader())
   {
      return false;
   }
   //copy the meta data over to the class

   mSampleRate=odDecoder->mSampleRate;
   mNumChannels=odDecoder->mNumChannels;
   mBitsPerSample=odDecoder->mBitsPerSample;

   mNumSamples=odDecoder->mNumSamples;
   mBitsPerSample=odDecoder->mBitsPerSample;
   mFormat=odDecoder->mFormat;
   mStreamInfoDone=true;


   return true;
#endif
#ifdef LEGACY_FLAC
   bool success = mFile->set_filename(OSINPUT(mFilename));
   if (!success) {
      return false;
   }
   mFile->set_metadata_respond(FLAC__METADATA_TYPE_STREAMINFO);
   mFile->set_metadata_respond(FLAC__METADATA_TYPE_VORBIS_COMMENT);
   FLAC::Decoder::File::State state = mFile->init();
   if (state != FLAC__FILE_DECODER_OK) {
      return false;
   }
#else
   if (!mHandle.Open(mFilename, wxT("rb"))) {
      return false;
   }

   // Even though there is an init() method that takes a filename, use the one that
   // takes a file handle because wxWidgets can open a file with a Unicode name and
   // libflac can't (under Windows).
   //
   // Responsibility for closing the file is passed to libflac.
   // (it happens when mFile->finish() is called)
   bool result = mFile->init(mHandle.fp())?true:false;
   mHandle.Detach();

   if (result != FLAC__STREAM_DECODER_INIT_STATUS_OK) {
      return false;
   }
#endif
   mFile->process_until_end_of_metadata();

#ifdef LEGACY_FLAC
   state = mFile->get_state();
   if (state != FLAC__FILE_DECODER_OK) {
      return false;
   }
#else
   // not necessary to check state, error callback will catch errors, but here's how:
   if (mFile->get_state() > FLAC__STREAM_DECODER_READ_FRAME) {
      return false;
   }
#endif

   if (!mFile->is_valid() || mFile->get_was_error()) {
      // This probably is not a FLAC file at all
      return false;
   }
   return true;
}

TranslatableString FLACImportFileHandle::GetFileDescription()
{
   return DESC;
}


auto FLACImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   // TODO: Get Uncompressed byte count.
   return 0;
}


ProgressResult FLACImportFileHandle::Import(TrackFactory *trackFactory,
                                 TrackHolders &outTracks,
                                 Tags *tags)
{
   outTracks.clear();

   wxASSERT(mStreamInfoDone);

   CreateProgress();

   mChannels.resize(mNumChannels);

   {
      auto iter = mChannels.begin();
      for (size_t c = 0; c < mNumChannels; ++iter, ++c)
         *iter = trackFactory->NewWaveTrack(mFormat, mSampleRate);
   }

//Start OD
   bool useOD = false;
#ifdef EXPERIMENTAL_OD_FLAC
   useOD=true;
#endif

   // TODO: Vigilant Sentry: Variable res unused after assignment (error code DA1)
   //    Should check the result.
   #ifdef LEGACY_FLAC
      bool res = (mFile->process_until_end_of_file() != 0);
   #else
      bool res = true;
      if(!useOD)
         res = (mFile->process_until_end_of_stream() != 0);
   #endif
      wxUnusedVar(res);

   //add the task to the ODManager
   if(useOD)
   {
      auto fileTotalFrames =
         (sampleCount)mNumSamples; // convert from FLAC__uint64
      auto maxBlockSize = mChannels.begin()->get()->GetMaxBlockSize();
      for (decltype(fileTotalFrames) i = 0; i < fileTotalFrames; i += maxBlockSize) {
         const auto blockLen =
            limitSampleBufferSize( maxBlockSize, fileTotalFrames - i );

         auto iter = mChannels.begin();
         for (size_t c = 0; c < mNumChannels; ++c, ++iter)
            iter->get()->RightmostOrNewClip()->AppendBlockFile(
               [&]( wxFileNameWrapper filePath, size_t len ) {
                  return make_blockfile<ODDecodeBlockFile>(
                  std::move(filePath), wxFileNameWrapper{ mFilename },
                  i, len, c, ODTask::eODFLAC);
               },
               blockLen
            );

         mUpdateResult = mProgress->Update(
            i.as_long_long(),
            fileTotalFrames.as_long_long()
         );
         if (mUpdateResult != ProgressResult::Success)
            break;
      }

      bool moreThanStereo = mNumChannels>2;
      for (const auto &channel : mChannels)
      {
         mDecoderTask->AddWaveTrack(channel);
         if(moreThanStereo)
         {
            //if we have 3 more channels, they get imported on separate tracks, so we add individual tasks for each.
            ODManager::Instance()->AddNewTask(std::move(mDecoderTask));
            mDecoderTask = std::make_unique<ODDecodeFlacTask>(); //TODO: see if we need to use clone to keep the metadata.
         }
      }
      //if we have mono or a linked track (stereo), we add ONE task for the one linked wave track
      if(!moreThanStereo)
         ODManager::Instance()->AddNewTask(std::move(mDecoderTask));
   }
//END OD


   if (mUpdateResult == ProgressResult::Failed || mUpdateResult == ProgressResult::Cancelled) {
      return mUpdateResult;
   }

   for (const auto &channel : mChannels)
      channel->Flush();

   if (!mChannels.empty())
      outTracks.push_back(std::move(mChannels));

   tags->Clear();
   size_t cnt = mFile->mComments.size();
   for (size_t c = 0; c < cnt; c++) {
      wxString name = mFile->mComments[c].BeforeFirst(wxT('='));
      wxString value = mFile->mComments[c].AfterFirst(wxT('='));
      if (name.Upper() == wxT("DATE") && !tags->HasTag(TAG_YEAR)) {
         long val;
         if (value.length() == 4 && value.ToLong(&val)) {
            name = TAG_YEAR;
         }
      }
      tags->SetTag(name, value);
   }

   return mUpdateResult;
}


FLACImportFileHandle::~FLACImportFileHandle()
{
   //don't finish *mFile if we are using OD,
   //because it was not initialized in Init().
#ifndef EXPERIMENTAL_OD_FLAC
   mFile->finish();
#endif
}

#endif /* USE_LIBFLAC */
