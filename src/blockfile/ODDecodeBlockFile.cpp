/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeBlockFile.cpp

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeBlockFile
\brief ODDecodeBlockFile is a special type of SimpleBlockFile that does not necessarily have summary OR audio data available
The summary is eventually computed and written to a file in a background thread.  See ODPCMAliasBlockFile for a similar class.

*//*******************************************************************/

#include "../Audacity.h"
#include "ODDecodeBlockFile.h"

#include <float.h>

#include <wx/utils.h>
#include <wx/wxcrtvararg.h>
#include <wx/log.h>
#include <wx/thread.h>
#include <sndfile.h>

#include "../DirManager.h"
#include "../FileFormats.h"
#include "../ondemand/ODManager.h"
#include "NotYetAvailableException.h"

const int bheaderTagLen = 20;
char bheaderTag[bheaderTagLen + 1] = "AudacityBlockFile112";



   /// Create a disk file and write summary and sample data to it
ODDecodeBlockFile::ODDecodeBlockFile(wxFileNameWrapper &&baseFileName, wxFileNameWrapper &&audioFileName, sampleCount aliasStart,
                     size_t aliasLen, int aliasChannel,unsigned int decodeType)
try
   : SimpleBlockFile{ std::move(baseFileName),
                    NULL, aliasLen, floatSample, true, true },
   //floatSample has no effect.  last two bools - bypass writing of blockfile and cache

   mType(decodeType),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
   mDecoder = NULL;
   mAudioFileName = std::move(audioFileName);
   mFormat = int16Sample;
}
catch ( const FileException & WXUNUSED(e) )
{
   // The constructor SimpleBlockFile sometimes throws this,
   // but it never will for the arguments that were passed to it here.
   // So add a catch for completeness, but just assert that this won't happen.
   wxASSERT(false);
   throw;
}

/// Create the memory structure to refer to the given block file
ODDecodeBlockFile::ODDecodeBlockFile(wxFileNameWrapper &&existingFile, wxFileNameWrapper &&audioFileName, sampleCount aliasStart,
                     size_t aliasLen, int aliasChannel, unsigned int decodeType,
                   float min, float max, float rms, bool dataAvailable):
   SimpleBlockFile{ std::move(existingFile), aliasLen, min, max, rms },

   mType(decodeType),
   mDataAvailable( dataAvailable ),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
   mDecoder = NULL;
   mAudioFileName = std::move(audioFileName);
   mFormat = int16Sample;
}



ODDecodeBlockFile::~ODDecodeBlockFile()
{

}


//Check to see if we have the file for these calls.
auto ODDecodeBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   if(IsSummaryAvailable())
   {
      wxFFile summaryFile(mFileName.GetFullPath());
      return summaryFile.Length();
   }
   else
   {
      return 0;
   }
}


/// Gets extreme values for the specified region
auto ODDecodeBlockFile::GetMinMaxRMS(
   size_t start, size_t len, bool mayThrow) const -> MinMaxRMS
{
   if(IsSummaryAvailable())
   {
      return SimpleBlockFile::GetMinMaxRMS(start, len, mayThrow);
   }
   else
   {
      if (mayThrow)
         throw NotYetAvailableException{ mAudioFileName };

      //fake values.  These values are used usually for normalization and amplifying, so we want
      //the max to be maximal and the min to be minimal
      return {
         -1.0f, 1.0f, 0.707f //sin with amp of 1 rms
      };
   }
}

/// Gets extreme values for the entire block
auto ODDecodeBlockFile::GetMinMaxRMS(bool mayThrow) const -> MinMaxRMS
{
  if(IsSummaryAvailable())
   {
      return SimpleBlockFile::GetMinMaxRMS(mayThrow);
   }
   else
   {
      if (mayThrow)
         throw NotYetAvailableException{ mAudioFileName };

      //fake values.  These values are used usually for normalization and amplifying, so we want
      //the max to be maximal and the min to be minimal
      return {
         -1.0f, 1.0f, 0.707f //sin with amp of 1 rms
      };
   }
}

/// Returns the 256 byte summary data block
/// Fill with zeroes and return false if data are unavailable for any reason.
bool ODDecodeBlockFile::Read256(float *buffer, size_t start, size_t len)
{
   if(IsSummaryAvailable())
   {
      return SimpleBlockFile::Read256(buffer,start,len);
   }
   else
   {
      ClearSamples((samplePtr)buffer, floatSample, 0, len);
      return false;
   }
}

/// Returns the 64K summary data block
/// Fill with zeroes and return false if data are unavailable for any reason.
bool ODDecodeBlockFile::Read64K(float *buffer, size_t start, size_t len)
{
   if(IsSummaryAvailable())
   {
      return SimpleBlockFile::Read64K(buffer,start,len);
   }
   else
   {
      ClearSamples((samplePtr)buffer, floatSample, 0, len);
      return false;
   }
}

/// If the summary has been computed,
/// Construct a NEW SimpleBlockFile based on this one.
/// otherwise construct an ODDecodeBlockFile that still needs to be computed.
/// @param newFileName The filename to copy the summary data to.
BlockFilePtr ODDecodeBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   BlockFilePtr newBlockFile;

   //mAliasedFile can change so we lock readdatamutex, which is responsible for it.
   auto locker = LockForRead();
   if(IsSummaryAvailable())
   {
      //create a simpleblockfile, because once it has the summary it is a simpleblockfile for all intents an purposes
      newBlockFile  = SimpleBlockFile::Copy(std::move(newFileName)) ;
   }
   else
   {
      //Summary File might exist in this case, but it probably (99.999% of the time) won't.
      newBlockFile = make_blockfile<ODDecodeBlockFile>
         (std::move(newFileName), wxFileNameWrapper{mAudioFileName},
          mAliasStart, mLen, mAliasChannel, mType,
          mMin, mMax, mRMS, IsSummaryAvailable());
      //The client code will need to schedule this blockfile for OD decoding if it is going to a NEW track.
      //It can do this by checking for IsDataAvailable()==false.
   }

   return newBlockFile;
}


/// Writes the xml as a SimpleBlockFile if we can (if we have a summary file)
/// Otherwise writes XML as a subset of attributes with 'odpcmaliasblockfile as the start tag.
/// Most notably, the summaryfile attribute refers to a file that does not yet, so when the project file is read back in
/// and this object reconstructed, it needs to avoid trying to open it as well as schedule itself for OD loading
void ODDecodeBlockFile::SaveXML(XMLWriter &xmlFile)
// may throw
{
   auto locker = LockForRead();
   if(IsSummaryAvailable())
   {
      SimpleBlockFile::SaveXML(xmlFile);
   }
   else
   {
      xmlFile.StartTag(wxT("oddecodeblockfile"));
      {
         //unlock to prevent deadlock and resume lock after.
         auto suspension = locker.Suspend();
         ODLocker locker2{ &mFileNameMutex };
         xmlFile.WriteAttr(wxT("summaryfile"), mFileName.GetFullName());
      }
      xmlFile.WriteAttr(wxT("audiofile"), mAudioFileName.GetFullPath());
      xmlFile.WriteAttr(wxT("aliasstart"),
                        mAliasStart.as_long_long());
      xmlFile.WriteAttr(wxT("aliaslen"), mLen);
      xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);
      xmlFile.WriteAttr(wxT("decodetype"), (size_t)mType);

      xmlFile.EndTag(wxT("oddecodeblockfile"));
   }
}

/// Constructs a ODDecodeBlockFile from the xml output of WriteXML.
/// Also schedules the ODDecodeBlockFile for OD loading.
// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in ProjectFSCK().
BlockFilePtr ODDecodeBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileNameWrapper summaryFileName;
   wxFileNameWrapper audioFileName;
   sampleCount aliasStart = 0;
   size_t aliasLen = 0;
   int aliasChannel=0;
   long nValue;
   long long nnValue;
   unsigned int   decodeType=0;

   while(*attrs)
   {
      const wxChar *attr =  *attrs++;
      const wxChar *value = *attrs++;
      if (!value)
         break;

      const wxString strValue = value;
      if (!wxStricmp(attr, wxT("summaryfile")) &&
            // Can't use XMLValueChecker::IsGoodFileName here, but do part of its test.
            XMLValueChecker::IsGoodFileString(strValue) &&
            (strValue.length() + 1 + dm.GetProjectDataDir().length() <= PLATFORM_MAX_PATH))
      {
         if (!dm.AssignFile(summaryFileName, strValue, false))
            // Make sure summaryFileName is back to uninitialized state so we can detect problem later.
            summaryFileName.Clear();
      }
      else if( !wxStricmp(attr, wxT("audiofile")) )
      {
         if (XMLValueChecker::IsGoodPathName(strValue))
            audioFileName.Assign(strValue);
         else if (XMLValueChecker::IsGoodFileName(strValue, dm.GetProjectDataDir()))
            // Allow fallback of looking for the file name, located in the data directory.
            audioFileName.Assign(dm.GetProjectDataDir(), strValue);
         else if (XMLValueChecker::IsGoodPathString(strValue))
            // If the file is missing, we failed XMLValueChecker::IsGoodPathName()
            // and XMLValueChecker::IsGoodFileName, because both do existence tests,
            // but we want to keep the reference to the file because it's a good path string.
            audioFileName.Assign(strValue);
      }
      else if ( !wxStricmp(attr, wxT("aliasstart")) )
      {
         if (XMLValueChecker::IsGoodInt64(strValue) &&
             strValue.ToLongLong(&nnValue) && (nnValue >= 0))
            aliasStart = nnValue;
      }
      else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
      {  // integer parameters
         if (!wxStricmp(attr, wxT("aliaslen")) && (nValue >= 0))
            aliasLen = nValue;
         else if (!wxStricmp(attr, wxT("aliaschannel")) && XMLValueChecker::IsValidChannel(aliasChannel))
            aliasChannel = nValue;
         else if( !wxStricmp(attr, wxT("decodetype")) )
            decodeType = nValue;
      }
   }

   return make_blockfile<ODDecodeBlockFile>
      (std::move(summaryFileName), std::move(audioFileName),
       aliasStart, aliasLen, aliasChannel, decodeType, 0, 0, 0, false);
}



void ODDecodeBlockFile::Recover(void)
{
   if(IsSummaryAvailable())
   {
      WriteODDecodeBlockFile();
   }
}

bool ODDecodeBlockFile::IsSummaryAvailable() const
{
   return IsDataAvailable();
}

bool ODDecodeBlockFile::IsDataAvailable() const
{
   return mDataAvailable != 0;
}

/// Write the summary to disk, using the derived ReadData() to get the data
/// Here, the decoder ODTask associated with this file must fetch the samples with
/// the ODDecodeTask::Decode() method.
int ODDecodeBlockFile::WriteODDecodeBlockFile()
{

   // To build the summary data, call ReadData (implemented by the
   // derived classes) to get the sample data
   SampleBuffer sampleData;// = NewSamples(mLen, floatSample);
   int ret;

   {
      //use the decoder here.
      ODLocker locker{ &mDecoderMutex };

      if(!mDecoder)
         return -1;

      //sampleData and mFormat are set by the decoder.
      ret = mDecoder->Decode(sampleData, mFormat, mAliasStart, mLen, mAliasChannel);

      if(ret < 0) {
         wxPrintf("ODDecodeBlockFile Decode failure\n");
         return ret; //failure
      }
   }

   {
      //the summary is also calculated here.
      ODLocker locker{ &mFileNameMutex };
      //TODO: we may need to write a version of WriteSimpleBlockFile that uses threadsafe FILE vs wxFile
      bool bSuccess =
         WriteSimpleBlockFile(
                              sampleData.ptr(),
                              mLen,
                              mFormat,
                              NULL);
      if ( !bSuccess )
         return -1;
   }

   wxAtomicInc( mDataAvailable );

   return ret;
}

///sets the file name the summary info will be saved in.  threadsafe.
void ODDecodeBlockFile::SetFileName(wxFileNameWrapper &&name)
{
   mFileNameMutex.Lock();
   mFileName=std::move(name);
/* mchinen oct 9 2009 don't think we need the char* but leaving it in for now just as a reminder that we might
   if wxFileName isn't threadsafe.
   mFileNameChar.reinit(strlen(mFileName.GetFullPath().mb_str(wxConvUTF8))+1);
   strcpy(mFileNameChar.get(), mFileName.GetFullPath().mb_str(wxConvUTF8)); */
   mFileNameMutex.Unlock();
}

///sets the file name the summary info will be saved in.  threadsafe.
auto ODDecodeBlockFile::GetFileName() const -> GetFileNameResult
{
   return { mFileName, ODLocker{ &mFileNameMutex } };
}

/// A thread-safe version of CalcSummary.  BlockFile::CalcSummary
/// uses a static summary array across the class, which we can't use.
/// Get a buffer containing a summary block describing this sample
/// data.  This must be called by derived classes when they
/// are constructed, to allow them to construct their summary data,
/// after which they should write that data to their disk file.
///
/// This method also has the side effect of setting the mMin, mMax,
/// and mRMS members of this class.
///
/// Unlike BlockFile's implementation You SHOULD DELETE the returned buffer.
/// this is protected so it shouldn't be hard to deal with - just override
/// all BlockFile methods that use this method.
///
/// @param buffer A buffer containing the sample data to be analyzed
/// @param len    The length of the sample data
/// @param format The format of the sample data.
void *ODDecodeBlockFile::CalcSummary(samplePtr buffer, size_t len,
                             sampleFormat format, ArrayOf<char> &cleanup)
{
   cleanup.reinit(mSummaryInfo.totalSummaryBytes);
   char* localFullSummary = cleanup.get();

   memcpy(localFullSummary, bheaderTag, bheaderTagLen);

   float *summary64K = (float *)(localFullSummary + mSummaryInfo.offset64K);
   float *summary256 = (float *)(localFullSummary + mSummaryInfo.offset256);

   Floats floats;
   float *fbuffer;

   //mchinen: think we can hack this - don't allocate and copy if we don't need to.,
   if(format==floatSample)
   {
      fbuffer = (float*)buffer;
   }
   else
   {
      floats.reinit(len);
      fbuffer = floats.get();
      CopySamples(buffer, format,
               (samplePtr)fbuffer, floatSample, len);
   }

   BlockFile::CalcSummaryFromBuffer(fbuffer, len, summary256, summary64K);

   return localFullSummary;
}




/// Reads the specified data from the aliased file, using libsndfile,
/// and converts it to the given sample format.
///
/// @param data   The buffer to read the sample data into.
/// @param format The format to convert the data into
/// @param start  The offset within the block to begin reading
/// @param len    The number of samples to read
size_t ODDecodeBlockFile::ReadData(samplePtr data, sampleFormat format,
                                size_t start, size_t len, bool mayThrow) const
{
   auto locker = LockForRead();
   if(IsSummaryAvailable())
      return SimpleBlockFile::ReadData(data, format, start, len, mayThrow);
   else
   {
      if (mayThrow)
         throw NotYetAvailableException{ mAudioFileName };

      //we should do an ODRequest to start processing the data here, and wait till it finishes. and just do a SimpleBlockFile
      //ReadData.
      ClearSamples(data, format, 0, len);
      return 0;
   }
}

/// Read the summary of this alias block from disk.  Since the audio data
/// is elsewhere, this consists of reading the entire summary file.
/// Fill with zeroes and return false if data are unavailable for any reason.
///
/// @param *data The buffer where the summary data will be stored.  It must
///              be at least mSummaryInfo.totalSummaryBytes long.
bool ODDecodeBlockFile::ReadSummary(ArrayOf<char> &data)
{
   //I dont think we need to add a mutex here because only the main thread changes filenames and calls ReadSummary
   if(IsSummaryAvailable())
      return SimpleBlockFile::ReadSummary(data);

   data.reinit( mSummaryInfo.totalSummaryBytes );
   memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);
   return false;
}

///set the decoder,
void ODDecodeBlockFile::SetODFileDecoder(ODFileDecoder* decoder)
{
   //since this is the only place that writes to mdecoder, it is totally thread-safe to read check without the mutex
   if(decoder == mDecoder)
      return;
   mDecoderMutex.Lock();
   mDecoder = decoder;
   mDecoderMutex.Unlock();
}


/// Prevents a read on other threads.
void ODDecodeBlockFile::LockRead() const
{
   mReadDataMutex.Lock();
}
/// Allows reading of encoded file on other threads.
void ODDecodeBlockFile::UnlockRead() const
{
   mReadDataMutex.Unlock();
}

const wxFileNameWrapper &ODDecodeBlockFile::GetExternalFileName() const
{
   if ( !IsDataAvailable() )
      return GetEncodedAudioFilename();
   return SimpleBlockFile::GetExternalFileName();
}

void ODDecodeBlockFile::SetExternalFileName( wxFileNameWrapper &&newName )
{
   ChangeAudioFile( std::move( newName ) );
}

/// Modify this block to point at a different file.  This is generally
/// looked down on, but it is necessary in one case: see
/// DirManager::EnsureSafeFilename().
void ODDecodeBlockFile::ChangeAudioFile(wxFileNameWrapper &&newAudioFile)
{
   mAudioFileName = std::move(newAudioFile);
}



static DirManager::RegisteredBlockFileDeserializer sRegistration {
   "oddecodeblockfile",
   []( DirManager &dm, const wxChar **attrs ){
      auto result = ODDecodeBlockFile::BuildFromXML( dm, attrs );
      ODManager::MarkLoadedODFlag();
      return result;
   }
};

///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
ODFileDecoder::ODFileDecoder(const wxString & fName)
   : mFName{ fName }
{
   mInited = false;
}

ODFileDecoder::~ODFileDecoder()
{
}

bool ODFileDecoder::IsInitialized()
{
   bool ret;
   mInitedLock.Lock();
   ret = mInited;
   mInitedLock.Unlock();
   return ret;
}

///Derived classes should call this after they have parsed the header.
void ODFileDecoder::MarkInitialized()
{
   mInitedLock.Lock();
   mInited=true;
   mInitedLock.Unlock();
}

