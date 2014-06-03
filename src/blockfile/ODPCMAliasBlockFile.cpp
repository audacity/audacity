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

#include <float.h>
#include "ODPCMAliasBlockFile.h"

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
      wxFileName fileName,
      wxFileName aliasedFileName,
      sampleCount aliasStart,
      sampleCount aliasLen, int aliasChannel)
: PCMAliasBlockFile(fileName, aliasedFileName,
                    aliasStart, aliasLen, aliasChannel,false)
{
   mSummaryAvailable = mSummaryBeingComputed = mHasBeenSaved = false;
}

///summaryAvailable should be true if the file has been written already.
ODPCMAliasBlockFile::ODPCMAliasBlockFile(
      wxFileName existingSummaryFileName,
      wxFileName aliasedFileName,
      sampleCount aliasStart,
      sampleCount aliasLen, int aliasChannel,
      float min, float max, float rms, bool summaryAvailable)
: PCMAliasBlockFile(existingSummaryFileName, aliasedFileName,
                    aliasStart, aliasLen,
                    aliasChannel, min, max, rms)
{
   mSummaryAvailable=summaryAvailable;
   mSummaryBeingComputed=mHasBeenSaved=false;
 }

ODPCMAliasBlockFile::~ODPCMAliasBlockFile()
{
}

/// Increases the reference count of this block by one.  Only
/// DirManager should call this method.
/// This method has been overidden to be threadsafe.  It is important especially
/// if two blockfiles deref at the same time resulting in a double deletion of the file
void ODPCMAliasBlockFile::Ref()
{
   mRefMutex.Lock();
   BlockFile::Ref();
   mRefMutex.Unlock();
}

/// Decreases the reference count of this block by one.  If this
/// causes the count to become zero, deletes the associated disk
/// file and deletes this object
bool ODPCMAliasBlockFile::Deref()
{
   bool ret;
   mDerefMutex.Lock();
   ret = BlockFile::Deref();
   if(!ret)
   {
      //Deref returns true when deleted, in which case we should not be touching instance variables, or ever calling this function again.
      mDerefMutex.Unlock();
   }
   return ret;
}



//Check to see if we have the file for these calls.
wxLongLong ODPCMAliasBlockFile::GetSpaceUsage()
{
   if(IsSummaryAvailable())
   {
      wxLongLong ret;
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

//when the file closes, it locks the blockfiles, but it calls this so we can check if it has been saved before.
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
void ODPCMAliasBlockFile::GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS)
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
void ODPCMAliasBlockFile::GetMinMax(float *outMin, float *outMax, float *outRMS)
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
bool ODPCMAliasBlockFile::Read256(float *buffer, sampleCount start, sampleCount len)
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
bool ODPCMAliasBlockFile::Read64K(float *buffer, sampleCount start, sampleCount len)
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
/// Construct a new PCMAliasBlockFile based on this one.
/// otherwise construct an ODPCMAliasBlockFile that still needs to be computed.
/// @param newFileName The filename to copy the summary data to.
BlockFile *ODPCMAliasBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile;

   //mAliasedFile can change so we lock readdatamutex, which is responsible for it.
   LockRead();
   //If the file has been written AND it has been saved, we create a PCM alias blockfile because for
   //all intents and purposes, it is the same.
   //However, if it hasn't been saved yet, we shouldn't create one because the default behavior of the
   //PCMAliasBlockFile is to lock on exit, and this will cause orphaned blockfiles..
   if(IsSummaryAvailable() && mHasBeenSaved)
   {
      newBlockFile  = new PCMAliasBlockFile(newFileName,
                                                   mAliasedFileName, mAliasStart,
                                                   mLen, mAliasChannel,
                                                   mMin, mMax, mRMS);

   }
   else
   {
      //Summary File might exist in this case, but it might not.
      newBlockFile  = new ODPCMAliasBlockFile(newFileName,
                                                   mAliasedFileName, mAliasStart,
                                                   mLen, mAliasChannel,
                                                   mMin, mMax, mRMS,IsSummaryAvailable());
      //The client code will need to schedule this blockfile for OD summarizing if it is going to a new track.
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
      xmlFile.WriteAttr(wxT("aliasstart"), mAliasStart);
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
BlockFile *ODPCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   sampleCount aliasStart=0, aliasLen=0;
   int aliasChannel=0;
   long nValue;

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
      else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
      {  // integer parameters
         if (!wxStricmp(attr, wxT("aliasstart")) && (nValue >= 0))
            aliasStart = nValue;
         else if (!wxStricmp(attr, wxT("aliaslen")) && (nValue >= 0))
            aliasLen = nValue;
         else if (!wxStricmp(attr, wxT("aliaschannel")) && XMLValueChecker::IsValidChannel(aliasChannel))
            aliasChannel = nValue;
      }
   }

   return new ODPCMAliasBlockFile(summaryFileName, aliasFileName,
                                    aliasStart, aliasLen, aliasChannel,
                                    0,0,0, false);
}



void ODPCMAliasBlockFile::Recover(void)
{
   if(IsSummaryAvailable())
   {
      WriteSummary();
   }
}

bool ODPCMAliasBlockFile::IsSummaryAvailable()
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
void ODPCMAliasBlockFile::SetFileName(wxFileName &name)
{
   mFileNameMutex.Lock();
   mFileName=name;
   mFileNameMutex.Unlock();
}

///sets the file name the summary info will be saved in.  threadsafe.
wxFileName ODPCMAliasBlockFile::GetFileName()
{
   wxFileName name;
   mFileNameMutex.Lock();
   name = mFileName;
   mFileNameMutex.Unlock();
   return name;
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
   samplePtr sampleData = NewSamples(mLen, floatSample);
   this->ReadData(sampleData, floatSample, 0, mLen);

   void *summaryData = CalcSummary(sampleData, mLen,
                                            floatSample);

   //summaryFile.Write(summaryData, mSummaryInfo.totalSummaryBytes);
   fwrite(summaryData, 1, mSummaryInfo.totalSummaryBytes, summaryFile);
   fclose(summaryFile);
   DeleteSamples(sampleData);
   delete [] (char *) summaryData;


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
/// Unlike BlockFile's implementation You SHOULD delete the returned buffer.
/// this is protected so it shouldn't be hard to deal with - just override
/// all BlockFile methods that use this method.
///
/// @param buffer A buffer containing the sample data to be analyzed
/// @param len    The length of the sample data
/// @param format The format of the sample data.
void *ODPCMAliasBlockFile::CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format)
{
   char* localFullSummary = new char[mSummaryInfo.totalSummaryBytes];

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
   sampleCount sumLen;
   sampleCount i, j, jcount;

   float min, max;
   float sumsq;

   // Recalc 256 summaries
   sumLen = (len + 255) / 256;


   for (i = 0; i < sumLen; i++) {
      min = fbuffer[i * 256];
      max = fbuffer[i * 256];
      sumsq = ((float)min) * ((float)min);
      jcount = 256;
      if (i * 256 + jcount > len)
         jcount = len - i * 256;
      for (j = 1; j < jcount; j++) {
         float f1 = fbuffer[i * 256 + j];
         sumsq += ((float)f1) * ((float)f1);
         if (f1 < min)
            min = f1;
         else if (f1 > max)
            max = f1;
      }

      float rms = (float)sqrt(sumsq / jcount);

      summary256[i * 3] = min;
      summary256[i * 3 + 1] = max;
      summary256[i * 3 + 2] = rms;
   }

   for (i = sumLen; i < mSummaryInfo.frames256; i++) {
      // filling in the remaining bits with non-harming/contributing values
      summary256[i * 3] = FLT_MAX;  // min
      summary256[i * 3 + 1] = -FLT_MAX;   // max
      summary256[i * 3 + 2] = 0.0f; // rms
   }

   // Recalc 64K summaries
   sumLen = (len + 65535) / 65536;

   for (i = 0; i < sumLen; i++) {
      min = summary256[3 * i * 256];
      max = summary256[3 * i * 256 + 1];
      sumsq = (float)summary256[3 * i * 256 + 2];
      sumsq *= sumsq;

      for (j = 1; j < 256; j++) {
         if (summary256[3 * (i * 256 + j)] < min)
            min = summary256[3 * (i * 256 + j)];
         if (summary256[3 * (i * 256 + j) + 1] > max)
            max = summary256[3 * (i * 256 + j) + 1];
         float r1 = summary256[3 * (i * 256 + j) + 2];
         sumsq += r1*r1;
      }

      float rms = (float)sqrt(sumsq / 256);

      summary64K[i * 3] = min;
      summary64K[i * 3 + 1] = max;
      summary64K[i * 3 + 2] = rms;
   }
   for (i = sumLen; i < mSummaryInfo.frames64K; i++) {
      summary64K[i * 3] = 0.0f;
      summary64K[i * 3 + 1] = 0.0f;
      summary64K[i * 3 + 2] = 0.0f;
   }

   // Recalc block-level summary
   min = summary64K[0];
   max = summary64K[1];
   sumsq = (float)summary64K[2];
   sumsq *= sumsq;

   for (i = 1; i < sumLen; i++) {
      if (summary64K[3*i] < min)
         min = summary64K[3*i];
      if (summary64K[3*i+1] > max)
         max = summary64K[3*i+1];
      float r1 = (float)summary64K[3*i+2];
      sumsq += (r1*r1);
   }

   mMin = min;
   mMax = max;
   mRMS = sqrt(sumsq / sumLen);


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
int ODPCMAliasBlockFile::ReadData(samplePtr data, sampleFormat format,
                                sampleCount start, sampleCount len)
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
   //there are thread-unsafe crashes here - not sure why.  sf_open may be called on the same file
   //from different threads, but this seems okay, unless it is implemented strangely..
   static ODLock sfMutex;

   wxFile f;   // will be closed when it goes out of scope
   SNDFILE *sf = NULL;

   if (f.Exists(aliasPath) && f.Open(aliasPath)) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      ODManager::LockLibSndFileMutex();
      sf = sf_open_fd(f.fd(), SFM_READ, &info, FALSE);
      ODManager::UnlockLibSndFileMutex();
   }

   if (!sf){

      memset(data,0,SAMPLE_SIZE(format)*len);

      mSilentAliasLog=TRUE;
      // Set a marker to display an error message
      if (!wxGetApp().ShouldShowMissingAliasedFileWarning())
         wxGetApp().MarkAliasedFilesMissingWarning(this);

      UnlockRead();
      return len;
   }

   mSilentAliasLog=FALSE;

   ODManager::LockLibSndFileMutex();
   sf_seek(sf, mAliasStart + start, SEEK_SET);
   ODManager::UnlockLibSndFileMutex();

   samplePtr buffer = NewSamples(len * info.channels, floatSample);

   int framesRead = 0;

   if (format == int16Sample &&
       !sf_subtype_more_than_16_bits(info.format)) {
      // Special case: if the file is in 16-bit (or less) format,
      // and the calling method wants 16-bit data, go ahead and
      // read 16-bit data directly.  This is a pretty common
      // case, as most audio files are 16-bit.
      ODManager::LockLibSndFileMutex();
      framesRead = sf_readf_short(sf, (short *)buffer, len);
      ODManager::UnlockLibSndFileMutex();

      for (int i = 0; i < framesRead; i++)
         ((short *)data)[i] =
            ((short *)buffer)[(info.channels * i) + mAliasChannel];
   }
   else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      ODManager::LockLibSndFileMutex();
      framesRead = sf_readf_float(sf, (float *)buffer, len);
      ODManager::UnlockLibSndFileMutex();
      float *bufferPtr = &((float *)buffer)[mAliasChannel];
      CopySamples((samplePtr)bufferPtr, floatSample,
                  (samplePtr)data, format,
                  framesRead, true, info.channels);
   }

   DeleteSamples(buffer);

   ODManager::LockLibSndFileMutex();
   sf_close(sf);
   ODManager::UnlockLibSndFileMutex();

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

      // new model; we need to return valid data
      memset(data,0,(size_t)mSummaryInfo.totalSummaryBytes);

      // we silence the logging for this operation in this object
      // after first occurrence of error; it's already reported and
      // spewing at the user will complicate the user's ability to
      // deal
      mSilentLog=TRUE;

      mFileNameMutex.Unlock();
      return true;

   }else mSilentLog=FALSE; // worked properly, any future error is new

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   FixSummary(data);


   mFileNameMutex.Unlock();
   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Prevents a read on other threads.
void ODPCMAliasBlockFile::LockRead()
{
   mReadDataMutex.Lock();
}
/// Allows reading on other threads.
void ODPCMAliasBlockFile::UnlockRead()
{
   mReadDataMutex.Unlock();
}


