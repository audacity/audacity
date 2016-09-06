/**********************************************************************

  Audacity: A Digital Audio Editor

  ODPCMAliasBlockFile.cpp

  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODPCMAliasBlockFile
\brief ODPCMAliasBlockFile is a special type of PCMAliasBlockFile that does not necessarily have summary data available
The summary is eventually computed and written to a file in a background thread.

*//*******************************************************************/

#include "../Audacity.h"
#include "ODPCMAliasBlockFile.h"

#include <float.h>

#include <wx/file.h>
#include <wx/utils.h>
#include <wx/wxchar.h>
#include <wx/log.h>
#include <wx/thread.h>
#include <sndfile.h>

#include "../AudacityApp.h"
#include "PCMAliasBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

#include "../ondemand/ODManager.h"
#include "../AudioIO.h"

//#include <errno.h>

extern AudioIO *gAudioIO;

const int aheaderTagLen = 20;
char aheaderTag[aheaderTagLen + 1] = "AudacityBlockFile112";


ODPCMAliasBlockFile::ODPCMAliasBlockFile(
      wxFileNameWrapper &&fileName,
      wxFileNameWrapper &&aliasedFileName,
      sampleCount aliasStart,
      size_t aliasLen, int aliasChannel)
: PCMAliasBlockFile { std::move(fileName), std::move(aliasedFileName),
                      aliasStart, aliasLen, aliasChannel, false }
{
   mSummaryAvailable = mSummaryBeingComputed = mHasBeenSaved = false;
}

///summaryAvailable should be true if the file has been written already.
ODPCMAliasBlockFile::ODPCMAliasBlockFile(
      wxFileNameWrapper &&existingSummaryFileName,
      wxFileNameWrapper &&aliasedFileName,
      sampleCount aliasStart,
      size_t aliasLen, int aliasChannel,
      float min, float max, float rms, bool summaryAvailable)
: PCMAliasBlockFile(std::move(existingSummaryFileName), std::move(aliasedFileName),
                    aliasStart, aliasLen,
                    aliasChannel, min, max, rms)
{
   mSummaryAvailable=summaryAvailable;
   mSummaryBeingComputed=mHasBeenSaved=false;
 }

ODPCMAliasBlockFile::~ODPCMAliasBlockFile()
{
}



//Check to see if we have the file for these calls.
auto ODPCMAliasBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   if(IsSummaryAvailable())
   {
      DiskByteCount ret;
      mFileNameMutex.Lock();
      wxFFile summaryFile(mFileName.GetFullPath());
      ret= summaryFile.Length();
      mFileNameMutex.Unlock();
      return ret;
   }
   else
   {
      return 0;
   }
}

/// Locks the blockfile only if it has a file that exists.  This needs to be done
/// so that the unsaved ODPCMAliasBlockfiles are deleted upon exit
void ODPCMAliasBlockFile::Lock()
{
   if(IsSummaryAvailable()&&mHasBeenSaved)
      PCMAliasBlockFile::Lock();
}

//when the file closes, it locks the blockfiles, but only conditionally.
// It calls this so we can check if it has been saved before.
void ODPCMAliasBlockFile::CloseLock()
{
   if(mHasBeenSaved)
      PCMAliasBlockFile::Lock();
}


/// unlocks the blockfile only if it has a file that exists.  This needs to be done
/// so that the unsaved ODPCMAliasBlockfiles are deleted upon exit
void ODPCMAliasBlockFile::Unlock()
{
   if(IsSummaryAvailable() && IsLocked())
      PCMAliasBlockFile::Unlock();
}


/// Gets extreme values for the specified region
void ODPCMAliasBlockFile::GetMinMax(size_t start, size_t len,
                          float *outMin, float *outMax, float *outRMS) const
{
   if(IsSummaryAvailable())
   {
      PCMAliasBlockFile::GetMinMax(start,len,outMin,outMax,outRMS);
   }
   else
   {
      //fake values.  These values are used usually for normalization and amplifying, so we want
      //the max to be maximal and the min to be minimal
      *outMin = -1.0*JUST_BELOW_MAX_AUDIO;
      *outMax = 1.0*JUST_BELOW_MAX_AUDIO;
      *outRMS = (float)0.707;//sin with amp of 1 rms
   }
}

/// Gets extreme values for the entire block
void ODPCMAliasBlockFile::GetMinMax(float *outMin, float *outMax, float *outRMS) const
{
  if(IsSummaryAvailable())
   {
      PCMAliasBlockFile::GetMinMax(outMin,outMax,outRMS);
   }
   else
   {
      //fake values.  These values are used usually for normalization and amplifying, so we want
      //the max to be maximal and the min to be minimal
      *outMin = -1.0*JUST_BELOW_MAX_AUDIO;
      *outMax = 1.0*JUST_BELOW_MAX_AUDIO;
      *outRMS = (float)0.707;//sin with amp of 1 rms
   }
}

/// Returns the 256 byte summary data block.  Clients should check to see if the summary is available before trying to read it with this call.
bool ODPCMAliasBlockFile::Read256(float *buffer, size_t start, size_t len)
{
   if(IsSummaryAvailable())
   {
      return PCMAliasBlockFile::Read256(buffer,start,len);
   }
   else
   {
      //return nothing.
      buffer = NULL;
      return true;
   }
}

/// Returns the 64K summary data block. Clients should check to see if the summary is available before trying to read it with this call.
bool ODPCMAliasBlockFile::Read64K(float *buffer, size_t start, size_t len)
{
   if(IsSummaryAvailable())
   {
      return PCMAliasBlockFile::Read64K(buffer,start,len);
   }
   else
   {
      //return nothing - it hasn't been calculated yet
      buffer = NULL;
      return true;
   }
}

/// If the summary has been computed,
/// Construct a NEW PCMAliasBlockFile based on this one.
/// otherwise construct an ODPCMAliasBlockFile that still needs to be computed.
/// @param newFileName The filename to copy the summary data to.
BlockFilePtr ODPCMAliasBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   BlockFilePtr newBlockFile;

   //mAliasedFile can change so we lock readdatamutex, which is responsible for it.
   LockRead();
   //If the file has been written AND it has been saved, we create a PCM alias blockfile because for
   //all intents and purposes, it is the same.
   //However, if it hasn't been saved yet, we shouldn't create one because the default behavior of the
   //PCMAliasBlockFile is to lock on exit, and this will cause orphaned blockfiles..
   if(IsSummaryAvailable() && mHasBeenSaved)
   {
      newBlockFile  = make_blockfile<PCMAliasBlockFile>
         (std::move(newFileName), wxFileNameWrapper{mAliasedFileName},
          mAliasStart, mLen, mAliasChannel, mMin, mMax, mRMS);

   }
   else
   {
      //Summary File might exist in this case, but it might not.
      newBlockFile  = make_blockfile<ODPCMAliasBlockFile>
         (std::move(newFileName), wxFileNameWrapper{mAliasedFileName},
          mAliasStart, mLen, mAliasChannel, mMin, mMax, mRMS,
          IsSummaryAvailable());
      //The client code will need to schedule this blockfile for OD summarizing if it is going to a NEW track.
   }

   UnlockRead();

   return newBlockFile;
}


/// Writes the xml as a PCMAliasBlockFile if we can (if we have a summary file)
/// Otherwise writes XML as a subset of attributes with 'odpcmaliasblockfile as the start tag.
/// Most notably, the summaryfile attribute refers to a file that does not yet, so when the project file is read back in
/// and this object reconstructed, it needs to avoid trying to open it as well as schedule itself for OD loading
void ODPCMAliasBlockFile::SaveXML(XMLWriter &xmlFile)
{
   //we lock this so that mAliasedFileName doesn't change.
   LockRead();
   if(IsSummaryAvailable())
   {
      PCMAliasBlockFile::SaveXML(xmlFile);
      mHasBeenSaved = true;
   }
   else
   {
      xmlFile.StartTag(wxT("odpcmaliasblockfile"));

      //unlock to prevent deadlock and resume lock after.
      UnlockRead();
      mFileNameMutex.Lock();
      xmlFile.WriteAttr(wxT("summaryfile"), mFileName.GetFullName());
      mFileNameMutex.Unlock();
      LockRead();

      xmlFile.WriteAttr(wxT("aliasfile"), mAliasedFileName.GetFullPath());
      xmlFile.WriteAttr(wxT("aliasstart"),
                        mAliasStart.as_long_long());
      xmlFile.WriteAttr(wxT("aliaslen"), mLen);
      xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);

      xmlFile.EndTag(wxT("odpcmaliasblockfile"));
   }

   UnlockRead();
}

/// Constructs a ODPCMAliasBlockFile from the xml output of WriteXML.
/// Does not schedule the ODPCMAliasBlockFile for OD loading.  Client code must do this.
// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in DirManager::ProjectFSCK().
BlockFilePtr ODPCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileNameWrapper summaryFileName;
   wxFileNameWrapper aliasFileName;
   sampleCount aliasStart = 0;
   size_t aliasLen = 0;
   int aliasChannel=0;
   long nValue;
   long long nnValue;

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
            (strValue.Length() + 1 + dm.GetProjectDataDir().Length() <= PLATFORM_MAX_PATH))
      {
         if (!dm.AssignFile(summaryFileName, strValue, false))
            // Make sure summaryFileName is back to uninitialized state so we can detect problem later.
            summaryFileName.Clear();
      }
      else if( !wxStricmp(attr, wxT("aliasfile")) )
      {
         if (XMLValueChecker::IsGoodPathName(strValue))
            aliasFileName.Assign(strValue);
         else if (XMLValueChecker::IsGoodFileName(strValue, dm.GetProjectDataDir()))
            // Allow fallback of looking for the file name, located in the data directory.
            aliasFileName.Assign(dm.GetProjectDataDir(), strValue);
         else if (XMLValueChecker::IsGoodPathString(strValue))
            // If the aliased file is missing, we failed XMLValueChecker::IsGoodPathName()
            // and XMLValueChecker::IsGoodFileName, because both do existence tests,
            // but we want to keep the reference to the missing file because it's a good path string.
            aliasFileName.Assign(strValue);
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
      }
   }

   return make_blockfile<ODPCMAliasBlockFile>
      (std::move(summaryFileName), std::move(aliasFileName),
       aliasStart, aliasLen, aliasChannel, 0, 0, 0, false);
}



void ODPCMAliasBlockFile::Recover(void)
{
   if(IsSummaryAvailable())
   {
      WriteSummary();
   }
}

bool ODPCMAliasBlockFile::IsSummaryAvailable() const
{
   bool retval;
   mSummaryAvailableMutex.Lock();
   retval= mSummaryAvailable;
   mSummaryAvailableMutex.Unlock();
   return retval;
}

///Calls write summary, and makes sure it is only done once in a thread-safe fashion.
void ODPCMAliasBlockFile::DoWriteSummary()
{
   mWriteSummaryMutex.Lock();
   if(!IsSummaryAvailable())
      WriteSummary();
   mWriteSummaryMutex.Unlock();
}

///sets the file name the summary info will be saved in.  threadsafe.
void ODPCMAliasBlockFile::SetFileName(wxFileNameWrapper &&name)
{
   mFileNameMutex.Lock();
   mFileName = std::move(name);
   mFileNameMutex.Unlock();
}

///sets the file name the summary info will be saved in.  threadsafe.
auto ODPCMAliasBlockFile::GetFileName() const -> GetFileNameResult
{
   return { mFileName, ODLocker{ &mFileNameMutex } };
}

/// Write the summary to disk, using the derived ReadData() to get the data
void ODPCMAliasBlockFile::WriteSummary()
{
   //the mFileName path may change, for example, when the project is saved.
   //(it moves from /tmp/ to wherever it is saved to.
   mFileNameMutex.Lock();

   //wxFFile is not thread-safe - if any error occurs in opening the file,
   // it posts a wxlog message which WILL crash
   // Audacity because it goes into the wx GUI.
   // For this reason I left the wxFFile method commented out. (mchinen)
   //    wxFFile summaryFile(mFileName.GetFullPath(), wxT("wb"));

   // ...and we use fopen instead.
   wxString sFullPath = mFileName.GetFullPath();
   char* fileNameChar = new char[strlen(sFullPath.mb_str(wxConvFile)) + 1];
   strcpy(fileNameChar, sFullPath.mb_str(wxConvFile));
   FILE* summaryFile = fopen(fileNameChar, "wb");

   mFileNameMutex.Unlock();

   // JKC ANSWER-ME: Whay is IsOpened() commented out?
   if( !summaryFile){//.IsOpened() ){

      // Never silence the Log w.r.t write errors; they always count
      //however, this is going to be called from a non-main thread,
      //and wxLog calls are not thread safe.
      printf("Unable to write summary data to file: %s", fileNameChar);
      delete [] fileNameChar;
      return;
   }
   delete [] fileNameChar;

   // To build the summary data, call ReadData (implemented by the
   // derived classes) to get the sample data
   SampleBuffer sampleData(mLen, floatSample);
   this->ReadData(sampleData.ptr(), floatSample, 0, mLen);

   ArrayOf<char> cleanup;
   void *summaryData = CalcSummary(sampleData.ptr(), mLen,
                                            floatSample, cleanup);

   //summaryFile.Write(summaryData, mSummaryInfo.totalSummaryBytes);
   fwrite(summaryData, 1, mSummaryInfo.totalSummaryBytes, summaryFile);
   fclose(summaryFile);


    //     printf("write successful. filename: %s\n", fileNameChar);

   mSummaryAvailableMutex.Lock();
   mSummaryAvailable=true;
   mSummaryAvailableMutex.Unlock();
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
void *ODPCMAliasBlockFile::CalcSummary(samplePtr buffer, size_t len,
                             sampleFormat format, ArrayOf<char> &cleanup)
{
   cleanup.reinit(mSummaryInfo.totalSummaryBytes);
   char* localFullSummary = cleanup.get();

   memcpy(localFullSummary, aheaderTag, aheaderTagLen);

   float *summary64K = (float *)(localFullSummary + mSummaryInfo.offset64K);
   float *summary256 = (float *)(localFullSummary + mSummaryInfo.offset256);

   float *fbuffer;

   //mchinen: think we can hack this - don't allocate and copy if we don't need to.,
   if(format==floatSample)
   {
      fbuffer = (float*)buffer;
   }
   else
   {
      fbuffer = new float[len];
      CopySamples(buffer, format,
               (samplePtr)fbuffer, floatSample, len);
   }

   BlockFile::CalcSummaryFromBuffer(fbuffer, len, summary256, summary64K);

   //if we've used the float sample..
   if(format!=floatSample)
   {
      delete[] fbuffer;
   }
   return localFullSummary;
}




/// Reads the specified data from the aliased file, using libsndfile,
/// and converts it to the given sample format.
/// Copied from PCMAliasBlockFIle but wxLog calls taken out for thread safety
///
/// @param data   The buffer to read the sample data into.
/// @param format The format to convert the data into
/// @param start  The offset within the block to begin reading
/// @param len    The number of samples to read
size_t ODPCMAliasBlockFile::ReadData(samplePtr data, sampleFormat format,
                                size_t start, size_t len) const
{

   LockRead();

   SF_INFO info;

   if(!mAliasedFileName.IsOk()){ // intentionally silenced
      memset(data,0,SAMPLE_SIZE(format)*len);
      UnlockRead();
      return len;
   }

   memset(&info, 0, sizeof(info));

   wxString aliasPath = mAliasedFileName.GetFullPath();

   wxFile f;   // will be closed when it goes out of scope
   SFFile sf;

   if (f.Exists(aliasPath) && f.Open(aliasPath)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      sf.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &info, FALSE));
   }
   // FIXME: TRAP_ERR failure of wxFile open incompletely handled in ODPCMAliasBlockFile::ReadData.


   if (!sf) {

      memset(data,0,SAMPLE_SIZE(format)*len);

      mSilentAliasLog = TRUE;
      // Set a marker to display an error message
      if (!wxGetApp().ShouldShowMissingAliasedFileWarning())
         wxGetApp().MarkAliasedFilesMissingWarning(this);

      UnlockRead();
      return len;
   }

   mSilentAliasLog=FALSE;

   // Third party library has its own type alias, check it
   static_assert(sizeof(sampleCount::type) <= sizeof(sf_count_t),
                 "Type sf_count_t is too narrow to hold a sampleCount");
   SFCall<sf_count_t>(sf_seek, sf.get(),
                      ( mAliasStart + start ).as_long_long(), SEEK_SET);

   wxASSERT(info.channels >= 0);
   SampleBuffer buffer(len * info.channels, floatSample);

   size_t framesRead = 0;

   if (format == int16Sample &&
       !sf_subtype_more_than_16_bits(info.format)) {
      // Special case: if the file is in 16-bit (or less) format,
      // and the calling method wants 16-bit data, go ahead and
      // read 16-bit data directly.  This is a pretty common
      // case, as most audio files are 16-bit.
      framesRead = SFCall<sf_count_t>(sf_readf_short, sf.get(), (short *)buffer.ptr(), len);

      for (int i = 0; i < framesRead; i++)
         ((short *)data)[i] =
            ((short *)buffer.ptr())[(info.channels * i) + mAliasChannel];
   }
   else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      framesRead = SFCall<sf_count_t>(sf_readf_float, sf.get(), (float *)buffer.ptr(), len);
      float *bufferPtr = &((float *)buffer.ptr())[mAliasChannel];
      CopySamples((samplePtr)bufferPtr, floatSample,
                  (samplePtr)data, format,
                  framesRead, true, info.channels);
   }

   UnlockRead();
   return framesRead;
}

/// Read the summary of this alias block from disk.  Since the audio data
/// is elsewhere, this consists of reading the entire summary file.
///
/// @param *data The buffer where the summary data will be stored.  It must
///              be at least mSummaryInfo.totalSummaryBytes long.
bool ODPCMAliasBlockFile::ReadSummary(void *data)
{

   mFileNameMutex.Lock();
   wxFFile summaryFile(mFileName.GetFullPath(), wxT("rb"));

   if( !summaryFile.IsOpened() ){

      // NEW model; we need to return valid data
      memset(data,0,(size_t)mSummaryInfo.totalSummaryBytes);

      // we silence the logging for this operation in this object
      // after first occurrence of error; it's already reported and
      // spewing at the user will complicate the user's ability to
      // deal
      mSilentLog=TRUE;

      mFileNameMutex.Unlock();
      return true;

   }else mSilentLog=FALSE; // worked properly, any future error is NEW

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   FixSummary(data);


   mFileNameMutex.Unlock();
   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Prevents a read on other threads.
void ODPCMAliasBlockFile::LockRead() const
{
   mReadDataMutex.Lock();
}
/// Allows reading on other threads.
void ODPCMAliasBlockFile::UnlockRead() const
{
   mReadDataMutex.Unlock();
}


