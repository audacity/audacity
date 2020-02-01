/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQT.cpp

  Joshua Haberman

  On OS X, handles import of MPEG-4 audio including AAC and Apple 
  Lossless, import of audio from MPEG-4 video files, and import of
  AIF(F)/AIFC files (AIF(F) might contain content libsndfile can't 
  handle). Could be modified to support QuickTime import on Windows.

**********************************************************************/

#include "../Audacity.h" // for USE_* macros

#include "Import.h"
#include "ImportPlugin.h"
#include "../widgets/AudacityMessageBox.h"
#include "../widgets/ProgressDialog.h"

#define DESC XO("QuickTime files")

static const auto exts = {
   wxT("aif"),
   wxT("aifc"),
   wxT("aiff"),
   wxT("mov"),
   wxT("aac"),
   wxT("m4a"),
   wxT("mp4")
};

#if defined(__WXMAC__)
#undef USE_QUICKTIME
#endif

#ifndef USE_QUICKTIME

// Bug 2068: misleading error message about QuickTime
// In 64 bit versions we cannot compile in (obsolete) QuickTime
// So don't register the QuickTime extensions, so ensuring we never report
// "This version of Audacity was not compiled with QuickTime files support"  
// When attempting to import MP4 files.
/*
static Importer::RegisteredUnusableImportPlugin registered{
      std::make_unique<UnusableImportPlugin>(DESC,
         FileExtensions( exts.begin(), exts.end() ) )
};
*/

#else /* USE_QUICKTIME */

// There's a name collision between our Track and QuickTime's...workaround it
#define Track XTrack

#if defined(__WXMAC__)
// These get used when building under OSX
   #include <Carbon/Carbon.h>
   #include <QuickTime/QuickTime.h>

   #include <wx/osx/core/private.h>
#else
// These get used when building under Windows
   #include <ConditionalMacros.h>
   #include <Movies.h>
   #include <QuickTimeComponents.h>
   #include <Sound.h>
   #include <Folders.h>
   #include <ToolUtils.h>
   #include <Gestalt.h>
   #include <Navigation.h>
#endif

// There's a name collision between our Track and QuickTime's...workaround it
#undef Track

#include "../Tags.h"
#include "../WaveTrack.h"

#define kQTAudioPropertyID_MaxAudioSampleSize   'mssz'

class QTImportPlugin final : public ImportPlugin
{
 public:
   QTImportPlugin()
   :  ImportPlugin( FileExtensions( exts.begin(), exts.end() ) ),
      mInitialized(false)
   {
      OSErr err = noErr;

#if defined(__WXMSW__)
      err = ::InitializeQTML(0);
#endif

      if (err == noErr) {
         err = ::EnterMovies();
         if (err == noErr) {
            mInitialized = true;
         }
         else {
#if defined(__WXMSW__)
            TerminateQTML();
#endif
         }
      }
   }

   ~QTImportPlugin()
   {
      if (mInitialized) {
         ExitMovies();
#if defined(__WXMSW__)
         TerminateQTML();
#endif
      }

      mInitialized = false;
   }

   wxString GetPluginStringID() override { return wxT("quicktime"); }

   TranslatableString GetPluginFormatDescription() override;
   std::unique_ptr<ImportFileHandle> Open(
      const wxString & Filename, AudacityProject*) override;

 private:
   bool mInitialized;
};

class QTImportFileHandle final : public ImportFileHandle
{
 public:
   QTImportFileHandle(const wxString & name, Movie movie)
   :  ImportFileHandle(name)
   {
      mMovie = movie;
   }

   virtual ~QTImportFileHandle()
   {
      if (mMovie) {
         DisposeMovie(mMovie);
         mMovie = NULL;
      }
   }

   TranslatableString GetFileDescription() override;
   ByteCount GetFileUncompressedBytes() override;

   wxInt32 GetStreamCount() override
   {
      return 1;
   }

   const TranslatableStrings &GetStreamInfo() override
   {
      static TranslatableStrings empty;
      return empty;
   }

   void SetStreamUsage(wxInt32 StreamID, bool Use) override
   {
   }

   ProgressResult Import(TrackFactory *trackFactory,
              TrackHolders &outTracks,
              Tags *tags) override;

 private:
   void AddMetadata(Tags *tags);

   Movie mMovie;
};

TranslatableString QTImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

std::unique_ptr<ImportFileHandle> QTImportPlugin::Open(
   const wxString & Filename, AudacityProject*)
{
   OSErr err;
   FSRef inRef;
   Movie theMovie = NULL;
   Handle dataRef = NULL;
   OSType dataRefType = 0;
   short resID = 0;

#if defined(__WXMAC__)
   err = wxMacPathToFSRef(Filename, &inRef);
#else
   // LLL:  This will not work for pathnames with Unicode characters...find
   //       another method.
   err = FSPathMakeRef((UInt8 *)OSFILENAME(Filename), &inRef, NULL);
#endif

   if (err != noErr) {
      return nullptr;
   }

   err = QTNewDataReferenceFromFSRef(&inRef, 0, &dataRef, &dataRefType);
   if (err != noErr) {
      return nullptr;
   }

   // instantiate the movie
   err = NewMovieFromDataRef(&theMovie,
                             newMovieActive | newMovieDontAskUnresolvedDataRefs,
                             &resID,
                             dataRef,
                             dataRefType);
   DisposeHandle(dataRef);
   if (err != noErr) {
      return nullptr;
   }

   return std::make_unique<QTImportFileHandle>(Filename, theMovie);
}

static Importer::RegisteredImportPlugin registered{ "QT",
   std::make_unique< QTImportPlugin >()
};


TranslatableString QTImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto QTImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   return 0;
}

ProgressResult QTImportFileHandle::Import(TrackFactory *trackFactory,
                               TrackHolders &outTracks,
                               Tags *tags)
{
   outTracks.clear();

   OSErr err = noErr;
   MovieAudioExtractionRef maer = NULL;
   auto updateResult = ProgressResult::Success;
   auto totSamples =
      (sampleCount) GetMovieDuration(mMovie); // convert from TimeValue
   decltype(totSamples) numSamples = 0;
   Boolean discrete = true;
   UInt32 quality = kQTAudioRenderQuality_Max;
   AudioStreamBasicDescription desc;
   UInt32 maxSampleSize;
   bool res = false;

   auto cleanup = finally( [&] {
      if (maer) {
         MovieAudioExtractionEnd(maer);
      }
   } );

   CreateProgress();

   do
   {
      err = MovieAudioExtractionBegin(mMovie, 0, &maer);
      if (err != noErr) {
         AudacityMessageBox( XO("Unable to start QuickTime extraction") );
         break;
      }
   
      err = MovieAudioExtractionSetProperty(maer,
                                            kQTPropertyClass_MovieAudioExtraction_Audio,
                                            kQTMovieAudioExtractionAudioPropertyID_RenderQuality,
                                            sizeof(quality),
                                            &quality);
      if (err != noErr) {
         AudacityMessageBox( XO("Unable to set QuickTime render quality") );
         break;
      }
   
      err = MovieAudioExtractionSetProperty(maer,
                                            kQTPropertyClass_MovieAudioExtraction_Movie,
                                            kQTMovieAudioExtractionMoviePropertyID_AllChannelsDiscrete,
                                            sizeof(discrete),
                                            &discrete);
      if (err != noErr) {
         AudacityMessageBox( XO(
"Unable to set QuickTime discrete channels property") );
         break;
      }
   
      err = MovieAudioExtractionGetProperty(maer,
                                            kQTPropertyClass_MovieAudioExtraction_Audio,
                                            kQTAudioPropertyID_MaxAudioSampleSize,
                                            sizeof(maxSampleSize),
                                            &maxSampleSize,
                                            NULL);
      if (err != noErr) {
         AudacityMessageBox( XO(
"Unable to get QuickTime sample size property") );
         break;
      }
   
      err = MovieAudioExtractionGetProperty(maer,
                                            kQTPropertyClass_MovieAudioExtraction_Audio,
                                            kQTMovieAudioExtractionAudioPropertyID_AudioStreamBasicDescription,
                                            sizeof(desc),
                                            &desc,
                                            NULL);
      if (err != noErr) {
         AudacityMessageBox( XO("Unable to retrieve stream description") );
         break;
      }
   
      auto numchan = desc.mChannelsPerFrame;
      const size_t bufsize = 5 * desc.mSampleRate;
   
      // determine sample format
      sampleFormat format;
      switch (maxSampleSize)
      {
         case 16:
            format = int16Sample;
            break;
   
         case 24:
            format = int24Sample;
            break;
   
         default:
            format = floatSample;
            break;
      }

      // Allocate an array of pointers, whose size is not known statically,
      // and prefixed with the AudioBufferList structure.
      MallocPtr< AudioBufferList > abl{
         static_cast< AudioBufferList * >(
            calloc( 1, offsetof( AudioBufferList, mBuffers ) +
               (sizeof(AudioBuffer) * numchan))) };
      abl->mNumberBuffers = numchan;
   
      NewChannelGroup channels{ numchan };

      const auto size = sizeof(float) * bufsize;
      ArraysOf<unsigned char> holders{ numchan, size };
      for (size_t c = 0; c < numchan; c++) {
         auto &buffer = abl->mBuffers[c];
         auto &holder = holders[c];
         auto &channel = channels[c];

         buffer.mNumberChannels = 1;
         buffer.mDataByteSize = size;

         buffer.mData = holder.get();
   
         channel = trackFactory->NewWaveTrack( format );
         channel->SetRate( desc.mSampleRate );
      }
   
      do {
         UInt32 flags = 0;
         UInt32 numFrames = bufsize;
   
         err = MovieAudioExtractionFillBuffer(maer,
                                              &numFrames,
                                              abl.get(),
                                              &flags);
         if (err != noErr) {
            AudacityMessageBox( XO("Unable to get fill buffer") );
            break;
         }
   
         for (size_t c = 0; c < numchan; c++) {
            channels[c]->Append((char *) abl->mBuffers[c].mData, floatSample, numFrames);
         }
   
         numSamples += numFrames;
   
         updateResult = mProgress->Update(
            numSamples.as_long_long(),
            totSamples.as_long_long() );
   
         if (numFrames == 0 || flags & kQTMovieAudioExtractionComplete) {
            break;
         }
      } while (updateResult == ProgressResult::Success);
   
      res = (updateResult == ProgressResult::Success && err == noErr);
   
      if (res) {
         for (auto &channel: channels)
            channel->Flush();
         if (!channels.empty())
            outTracks.push_back(std::move(channels));
      }

      //
      // Extract any metadata
      //
      if (res) {
         AddMetadata(tags);
      }
   } while (false);

// done:

   return (res ? ProgressResult::Success : ProgressResult::Failed);
}

static const struct
{
   const OSType key;
   const wxChar *name;
}
names[] =
{
   {  kQTMetaDataCommonKeyAuthor,         wxT("Author")           },
   {  kQTMetaDataCommonKeyComment,        TAG_COMMENTS            },
   {  kQTMetaDataCommonKeyCopyright,      TAG_COPYRIGHT           },
   {  kQTMetaDataCommonKeyDirector,       wxT("Director")         },
   {  kQTMetaDataCommonKeyDisplayName,    wxT("Full Name")        },
   {  kQTMetaDataCommonKeyInformation,    wxT("Information")      },
   {  kQTMetaDataCommonKeyKeywords,       wxT("Keywords")         },
   {  kQTMetaDataCommonKeyProducer,       wxT("Producer")         },
   {  kQTMetaDataCommonKeyAlbum,          TAG_ALBUM               },
   {  kQTMetaDataCommonKeyArtist,         TAG_ARTIST              },
   {  kQTMetaDataCommonKeyChapterName,    wxT("Chapter")          },
   {  kQTMetaDataCommonKeyComposer,       wxT("Composer")         },
   {  kQTMetaDataCommonKeyDescription,    wxT("Description")      },
   {  kQTMetaDataCommonKeyGenre,          TAG_GENRE               },
   {  kQTMetaDataCommonKeyOriginalFormat, wxT("Original Format")  },
   {  kQTMetaDataCommonKeyOriginalSource, wxT("Original Source")  },
   {  kQTMetaDataCommonKeyPerformers,     wxT("Performers")       },
   {  kQTMetaDataCommonKeySoftware,       TAG_SOFTWARE            },
   {  kQTMetaDataCommonKeyWriter,         wxT("Writer")           },
};

void QTImportFileHandle::AddMetadata(Tags *tags)
{
   QTMetaDataRef metaDataRef = NULL;
   auto cleanup = finally( [&] {
      // we are done so release our metadata object
      if ( metaDataRef )
         QTMetaDataRelease(metaDataRef);
   } );

   OSErr err;

   err = QTCopyMovieMetaData(mMovie, &metaDataRef);
   if (err != noErr) {
      return;
   }

   for (int i = 0; i < WXSIZEOF(names); i++) {
      QTMetaDataItem item = kQTMetaDataItemUninitialized;
      // OSType key = names[i].key;

      err = QTMetaDataGetNextItem(metaDataRef,
                                  kQTMetaDataStorageFormatWildcard,
                                  kQTMetaDataItemUninitialized,
                                  kQTMetaDataKeyFormatCommon,
                                  (const UInt8 *) &names[i].key,
                                  sizeof(names[i].key),
                                  &item);
      if (err != noErr) {
         continue;
      }

      if (item == kQTMetaDataItemUninitialized) {
         continue;
      }

      QTPropertyValueType outPropType;
      ::ByteCount outPropValueSize;
      ::ByteCount outPropValueSizeUsed = 0;
      UInt32 outPropFlags;
      UInt32 dataType;

      // Get data type
      err =  QTMetaDataGetItemProperty(metaDataRef,
                                       item,
                                       kPropertyClass_MetaDataItem,
                                       kQTMetaDataItemPropertyID_DataType,
                                       sizeof(dataType),
                                       &dataType,
                                       &outPropValueSizeUsed);
      if (err != noErr) {
         continue;
      }

      // Get the data length
      err = QTMetaDataGetItemPropertyInfo(metaDataRef,
                                          item,
                                          kPropertyClass_MetaDataItem,
                                          kQTMetaDataItemPropertyID_Value,
                                          &outPropType,
                                          &outPropValueSize,
                                          &outPropFlags );
      if (err != noErr) {
         continue;
      }

      // Alloc memory for it
      ArrayOf<char> outVals{ outPropValueSize };

      // Retrieve the data
      err =  QTMetaDataGetItemProperty(metaDataRef,
                                       item,
                                       kPropertyClass_MetaDataItem,
                                       kQTMetaDataItemPropertyID_Value,
                                       outPropValueSize,
                                       outVals.get(),
                                       &outPropValueSizeUsed);
      if (err != noErr)
         continue;

      wxString v;

      switch (dataType)
      {
         case kQTMetaDataTypeUTF8:
            v = wxString(outVals.get(), wxConvUTF8);
         break;
         case kQTMetaDataTypeUTF16BE:
         {
            wxMBConvUTF16BE conv;
            v = wxString(outVals.get(), conv);
         }
         break;
      }

      if (!v.empty()) {
         tags->SetTag(names[i].name, v);
      }
   }

   return;
}

#endif
