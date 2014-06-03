/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportQT.cpp

  Joshua Haberman

  Handles importing MPEG-4 audio files, including AAC and Apple Lossless,
  on Mac OS X.  Could be modified to support QuickTime importing on
  Windows, too.

**********************************************************************/

#include "../Audacity.h"
#include "ImportPlugin.h"

#define DESC _("QuickTime files")

static const wxChar *exts[] =
{
   wxT("mov"),
   wxT("aac"),
   wxT("m4a")
};

#ifndef USE_QUICKTIME

void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList)
{
   UnusableImportPlugin* qtIsUnsupported =
      new UnusableImportPlugin(DESC, wxArrayString(WXSIZEOF(exts), exts));

   unusableImportPluginList->Append(qtIsUnsupported);
}

#else /* USE_QUICKTIME */

#include <wx/msgdlg.h>

// There's a name collision between our Track and QuickTime's...workaround it
#define Track XTrack

#if defined(__WXMAC__)
// These get used when building under OSX
   #include <Carbon/Carbon.h>
   #include <QuickTime/QuickTime.h>

   #include <wx/mac/private.h>
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

#include "../Internat.h"
#include "../Tags.h"
#include "../WaveTrack.h"
#include "ImportQT.h"

#define kQTAudioPropertyID_MaxAudioSampleSize   'mssz'

class QTImportPlugin : public ImportPlugin
{
 public:
   QTImportPlugin()
   :  ImportPlugin(wxArrayString(WXSIZEOF(exts), exts)),
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

   wxString GetPluginStringID() { return wxT("quicktime"); }

   wxString GetPluginFormatDescription();
   ImportFileHandle *Open(wxString Filename);

 private:
   bool mInitialized;
};

class QTImportFileHandle : public ImportFileHandle
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

   wxString GetFileDescription();
   int GetFileUncompressedBytes();

   wxInt32 GetStreamCount()
   {
      return 1;
   }

   wxArrayString *GetStreamInfo()
   {
      return NULL;
   }

   void SetStreamUsage(wxInt32 StreamID, bool Use)
   {
   }

   int Import(TrackFactory *trackFactory,
              Track ***outTracks,
              int *outNumTracks,
              Tags *tags);

 private:
   void AddMetadata(Tags *tags);

   Movie mMovie;
};

void GetQTImportPlugin(ImportPluginList *importPluginList,
                       UnusableImportPluginList *unusableImportPluginList)
{
   importPluginList->Append(new QTImportPlugin);
}

wxString QTImportPlugin::GetPluginFormatDescription()
{
   return DESC;
}

ImportFileHandle *QTImportPlugin::Open(wxString Filename)
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
      return NULL;
   }

   err = QTNewDataReferenceFromFSRef(&inRef, 0, &dataRef, &dataRefType);
   if (err != noErr) {
      return NULL;
   }

   // instantiate the movie
   err = NewMovieFromDataRef(&theMovie,
                             newMovieActive | newMovieDontAskUnresolvedDataRefs,
                             &resID,
                             dataRef,
                             dataRefType);
   DisposeHandle(dataRef);
   if (err != noErr) {
      return NULL;
   }

   return new QTImportFileHandle(Filename, theMovie);
}


wxString QTImportFileHandle::GetFileDescription()
{
   return DESC;
}

int QTImportFileHandle::GetFileUncompressedBytes()
{
   return 0;
}

int QTImportFileHandle::Import(TrackFactory *trackFactory,
                               Track ***outTracks,
                               int *outNumTracks,
                               Tags *tags)
{
   OSErr err = noErr;
   MovieAudioExtractionRef maer = NULL;
   int updateResult = eProgressSuccess;
   sampleCount totSamples = (sampleCount) GetMovieDuration(mMovie);
   sampleCount numSamples = 0;
   Boolean discrete = true;
   UInt32 quality = kQTAudioRenderQuality_Max;
   AudioStreamBasicDescription desc;
   UInt32 maxSampleSize;
   UInt32 numchan;
   UInt32 bufsize;

   CreateProgress();

   err = MovieAudioExtractionBegin(mMovie, 0, &maer);
   if (err != noErr) {
      wxMessageBox(_("Unable to start QuickTime extraction"));
      goto done;
   }

   err = MovieAudioExtractionSetProperty(maer,
                                         kQTPropertyClass_MovieAudioExtraction_Audio,
                                         kQTMovieAudioExtractionAudioPropertyID_RenderQuality,
                                         sizeof(quality),
                                         &quality);
   if (err != noErr) {
      wxMessageBox(_("Unable to set QuickTime render quality"));
      goto done;
   }

   err = MovieAudioExtractionSetProperty(maer,
                                         kQTPropertyClass_MovieAudioExtraction_Movie,
                                         kQTMovieAudioExtractionMoviePropertyID_AllChannelsDiscrete,
                                         sizeof(discrete),
                                         &discrete);
   if (err != noErr) {
      wxMessageBox(_("Unable to set QuickTime discrete channels property"));
      goto done;
   }

   err = MovieAudioExtractionGetProperty(maer,
                                         kQTPropertyClass_MovieAudioExtraction_Audio,
                                         kQTAudioPropertyID_MaxAudioSampleSize,
                                         sizeof(maxSampleSize),
                                         &maxSampleSize,
                                         NULL);
   if (err != noErr) {
      wxMessageBox(_("Unable to get QuickTime sample size property"));
      goto done;
   }

   err = MovieAudioExtractionGetProperty(maer,
                                         kQTPropertyClass_MovieAudioExtraction_Audio,
                                         kQTMovieAudioExtractionAudioPropertyID_AudioStreamBasicDescription,
                                         sizeof(desc),
                                         &desc,
                                         NULL);
   if (err != noErr) {
      wxMessageBox(_("Unable to retrieve stream description"));
      goto done;
   }

   numchan = desc.mChannelsPerFrame;
   bufsize = 5 * desc.mSampleRate;

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

   AudioBufferList *abl = (AudioBufferList *)
      calloc(1, offsetof(AudioBufferList, mBuffers) + (sizeof(AudioBuffer) * numchan));
   abl->mNumberBuffers = numchan;

   WaveTrack **channels = new WaveTrack *[numchan];

   int c;
   for (c = 0; c < numchan; c++) {
      abl->mBuffers[c].mNumberChannels = 1;
      abl->mBuffers[c].mDataByteSize = sizeof(float) * bufsize;
      abl->mBuffers[c].mData = malloc(abl->mBuffers[c].mDataByteSize);

      channels[c] = trackFactory->NewWaveTrack(format);
      channels[c]->SetRate(desc.mSampleRate);

      if (numchan == 2) {
         if (c == 0) {
            channels[c]->SetChannel(Track::LeftChannel);
            channels[c]->SetLinked(true);
         }
         else if (c == 1) {
            channels[c]->SetChannel(Track::RightChannel);
         }
      }
   }

   do {
      UInt32 flags = 0;
      UInt32 numFrames = bufsize;

      err = MovieAudioExtractionFillBuffer(maer,
                                           &numFrames,
                                           abl,
                                           &flags);
      if (err != noErr) {
         wxMessageBox(_("Unable to get fill buffer"));
         break;
      }

      for (c = 0; c < numchan; c++) {
         channels[c]->Append((char *) abl->mBuffers[c].mData, floatSample, numFrames);
      }

      numSamples += numFrames;

      updateResult = mProgress->Update((wxULongLong_t)numSamples,
                                       (wxULongLong_t)totSamples);

      if (numFrames == 0 || flags & kQTMovieAudioExtractionComplete) {
         break;
      }
   } while (updateResult == eProgressSuccess);

   bool res = (updateResult == eProgressSuccess && err == noErr);

   if (res) {
      for (c = 0; c < numchan; c++) {
         channels[c]->Flush();
      }

      *outTracks = (Track **) channels;
      *outNumTracks = numchan;
   }
   else {
      for (c = 0; c < numchan; c++) {
         delete channels[c];
      }

      delete [] channels;
   }

   for (c = 0; c < numchan; c++) {
      free(abl->mBuffers[c].mData);
   }
   free(abl);

   //
   // Extract any metadata
   //
   if (res) {
      AddMetadata(tags);
   }

done:

   if (maer) {
      MovieAudioExtractionEnd(maer);
   }

   return (res ? eProgressSuccess : eProgressFailed);
}

static const struct
{
   OSType key;
   wxChar *name;
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
   OSErr err;

   err = QTCopyMovieMetaData(mMovie, &metaDataRef);
   if (err != noErr) {
      return;
   }

   for (int i = 0; i < WXSIZEOF(names); i++) {
      QTMetaDataItem item = kQTMetaDataItemUninitialized;
      OSType key = names[i].key;

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

      QTPropertyValuePtr outValPtr = nil;
      QTPropertyValueType outPropType;
      ByteCount outPropValueSize;
      ByteCount outPropValueSizeUsed = 0;
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
      outValPtr = malloc(outPropValueSize);

      // Retrieve the data
      err =  QTMetaDataGetItemProperty(metaDataRef,
                                       item,
                                       kPropertyClass_MetaDataItem,
                                       kQTMetaDataItemPropertyID_Value,
                                       outPropValueSize,
                                       outValPtr,
                                       &outPropValueSizeUsed);
      if (err != noErr) {
         free(outValPtr);
         continue;
      }

      wxString v = wxT("");

      switch (dataType)
      {
         case kQTMetaDataTypeUTF8:
            v = wxString((char *)outValPtr, wxConvUTF8);
         break;
         case kQTMetaDataTypeUTF16BE:
         {
            wxMBConvUTF16BE conv;
            v = wxString((char *)outValPtr, conv);
         }
         break;
      }

      if (!v.IsEmpty()) {
         tags->SetTag(names[i].name, v);
      }

      free(outValPtr);
   }

   // we are done so release our metadata object
   QTMetaDataRelease(metaDataRef);

   return;
}

#endif
