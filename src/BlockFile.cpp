/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile.cpp

  Joshua Haberman
  Dominic Mazzoni

*******************************************************************//*!

\class BlockFile
\brief A BlockFile is a chunk of immutable audio data.

A BlockFile represents a chunk of audio data.  These chunks are
assembled into sequences by the class Sequence.  These classes
are at the heart of how Audacity stores audio data.

BlockFile is an abstract base class that can be implemented in
many different ways.  However it does have a fairly large amount
of shared code that deals with the physical file and manipulating
the summary data.

BlockFile should be thought of as an immutable class.  After it
is constructed, it is essentially never changed (though there are
a few exceptions).  Most notably, the audio data and summary data
are never altered once it is constructed.  This is important to
some of the derived classes that are actually aliases to audio
data stored in existing files.

BlockFiles are reference-counted, and deleted when their reference
count hits zero.  DirManager is the class responsible for
constructing and managing BlockFiles and managing their reference
counts.

*//****************************************************************//**

\class SummaryInfo
\brief Works with BlockFile to hold info about max and min and RMS
over multiple samples, which in turn allows rapid drawing when zoomed
out.

*//*******************************************************************/

#include "Audacity.h"
#include "BlockFile.h"

#include <float.h>
#include <cmath>

#include <wx/utils.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/log.h>

#include "Internat.h"
#include "MemoryX.h"

// msmeyer: Define this to add debug output via printf()
//#define DEBUG_BLOCKFILE

#ifdef DEBUG_BLOCKFILE
#define BLOCKFILE_DEBUG_OUTPUT(op, i) \
   wxPrintf(wxT("[BlockFile %x %s] %s: %i\n"), (unsigned)this, \
            mFileName.GetFullName().c_str(), wxT(op), i);
#else
#define BLOCKFILE_DEBUG_OUTPUT(op, i)
#endif

static const int headerTagLen = 20;
static char headerTag[headerTagLen + 1] = "AudacityBlockFile112";

SummaryInfo::SummaryInfo(size_t samples)
{
   format = floatSample;

   fields = 3; /* min, max, rms */

   bytesPerFrame = sizeof(float) * fields;

   frames64K = (samples + 65535) / 65536;
   frames256 = frames64K * 256;

   offset64K = headerTagLen;
   offset256 = offset64K + (frames64K * bytesPerFrame);
   totalSummaryBytes = offset256 + (frames256 * bytesPerFrame);
}

ArrayOf<char> BlockFile::fullSummary;

/// Initializes the base BlockFile data.  The block is initially
/// unlocked and its reference count is 1.
///
/// @param fileName The name of the disk file associated with this
///                 BlockFile.  Not all BlockFiles will store their
///                 sample data here (for example, AliasBlockFiles
///                 read their data from elsewhere), but all BlockFiles
///                 will store at least the summary data here.
///
/// @param samples  The number of samples this BlockFile contains.
BlockFile::BlockFile(wxFileNameWrapper &&fileName, size_t samples):
   mLockCount(0),
   mFileName(std::move(fileName)),
   mLen(samples),
   mSummaryInfo(samples)
{
   mSilentLog=FALSE;
}

// static
unsigned long BlockFile::gBlockFileDestructionCount { 0 };

BlockFile::~BlockFile()
{
   if (!IsLocked() && mFileName.HasName())
      wxRemoveFile(mFileName.GetFullPath());

   ++gBlockFileDestructionCount;
}

/// Returns the file name of the disk file associated with this
/// BlockFile.  Not all BlockFiles store their sample data here,
/// but most BlockFiles have at least their summary data here.
/// (some, i.e. SilentBlockFiles, do not correspond to a file on
///  disk and have empty file names)
auto BlockFile::GetFileName() const -> GetFileNameResult
{
   return { mFileName };
}

///sets the file name the summary info will be saved in.  threadsafe.
void BlockFile::SetFileName(wxFileNameWrapper &&name)
{
   mFileName=std::move(name);
}


/// Marks this BlockFile as "locked."  A locked BlockFile may not
/// be moved or deleted, only copied.  Locking a BlockFile prevents
/// it from disappearing if the project is saved in a different location.
/// When doing a "Save As," Audacity locks all blocks belonging
/// to the already-existing project, to ensure that the existing
/// project remains valid with all the blocks it needs.  Audacity
/// also locks the blocks of the last saved version of a project when
/// the project is deleted so that the files aren't deleted when their
/// refcount hits zero.
void BlockFile::Lock()
{
   mLockCount++;
   BLOCKFILE_DEBUG_OUTPUT("Lock", mLockCount);
}

/// Marks this BlockFile as "unlocked."
void BlockFile::Unlock()
{
   mLockCount--;
   BLOCKFILE_DEBUG_OUTPUT("Unlock", mLockCount);
}

/// Returns true if the block is locked.
bool BlockFile::IsLocked()
{
   return mLockCount > 0;
}

/// Get a buffer containing a summary block describing this sample
/// data.  This must be called by derived classes when they
/// are constructed, to allow them to construct their summary data,
/// after which they should write that data to their disk file.
///
/// This method also has the side effect of setting the mMin, mMax,
/// and mRMS members of this class.
///
/// You must not DELETE the returned buffer; it is static to this
/// method.
///
/// @param buffer A buffer containing the sample data to be analyzed
/// @param len    The length of the sample data
/// @param format The format of the sample data.
void *BlockFile::CalcSummary(samplePtr buffer, size_t len,
                             sampleFormat format, ArrayOf<char> &cleanup)
{
   // Caller has nothing to deallocate
   cleanup.reset();

   fullSummary.reinit(mSummaryInfo.totalSummaryBytes);

   memcpy(fullSummary.get(), headerTag, headerTagLen);

   float *summary64K = (float *)(fullSummary.get() + mSummaryInfo.offset64K);
   float *summary256 = (float *)(fullSummary.get() + mSummaryInfo.offset256);

   float *fbuffer = new float[len];
   CopySamples(buffer, format,
               (samplePtr)fbuffer, floatSample, len);

   CalcSummaryFromBuffer(fbuffer, len, summary256, summary64K);

   delete[] fbuffer;

   return fullSummary.get();
}

void BlockFile::CalcSummaryFromBuffer(const float *fbuffer, size_t len,
                                      float *summary256, float *summary64K)
{
   decltype(len) sumLen;

   float min, max;
   float sumsq;
   double totalSquares = 0.0;
   double fraction { 0.0 };

   // Recalc 256 summaries
   sumLen = (len + 255) / 256;
   int summaries = 256;

   for (decltype(sumLen) i = 0; i < sumLen; i++) {
      min = fbuffer[i * 256];
      max = fbuffer[i * 256];
      sumsq = ((float)min) * ((float)min);
      decltype(len) jcount = 256;
      if (jcount > len - i * 256) {
         jcount = len - i * 256;
         fraction = 1.0 - (jcount / 256.0);
      }
      for (decltype(jcount) j = 1; j < jcount; j++) {
         float f1 = fbuffer[i * 256 + j];
         sumsq += ((float)f1) * ((float)f1);
         if (f1 < min)
            min = f1;
         else if (f1 > max)
            max = f1;
      }

      totalSquares += sumsq;
      float rms = (float)sqrt(sumsq / jcount);

      summary256[i * 3] = min;
      summary256[i * 3 + 1] = max;
      summary256[i * 3 + 2] = rms;  // The rms is correct, but this may be for less than 256 samples in last loop.
   }
   for (auto i = sumLen; i < mSummaryInfo.frames256; i++) {
      // filling in the remaining bits with non-harming/contributing values
      // rms values are not "non-harming", so keep  count of them:
      summaries--;
      summary256[i * 3] = FLT_MAX;  // min
      summary256[i * 3 + 1] = -FLT_MAX;   // max
      summary256[i * 3 + 2] = 0.0f; // rms
   }

   // Calculate now while we can do it accurately
   mRMS = sqrt(totalSquares/len);

   // Recalc 64K summaries
   sumLen = (len + 65535) / 65536;

   for (decltype(sumLen) i = 0; i < sumLen; i++) {
      min = summary256[3 * i * 256];
      max = summary256[3 * i * 256 + 1];
      sumsq = (float)summary256[3 * i * 256 + 2];
      sumsq *= sumsq;
      for (decltype(len) j = 1; j < 256; j++) {   // we can overflow the useful summary256 values here, but have put non-harmful values in them
         if (summary256[3 * (i * 256 + j)] < min)
            min = summary256[3 * (i * 256 + j)];
         if (summary256[3 * (i * 256 + j) + 1] > max)
            max = summary256[3 * (i * 256 + j) + 1];
         float r1 = summary256[3 * (i * 256 + j) + 2];
         sumsq += r1*r1;
      }

      double denom = (i < sumLen - 1) ? 256.0 : summaries - fraction;
      float rms = (float)sqrt(sumsq / denom);

      summary64K[i * 3] = min;
      summary64K[i * 3 + 1] = max;
      summary64K[i * 3 + 2] = rms;
   }
   for (auto i = sumLen; i < mSummaryInfo.frames64K; i++) {
      wxASSERT_MSG(false, wxT("Out of data for mSummaryInfo"));   // Do we ever get here?
      summary64K[i * 3] = 0.0f;  // probably should be FLT_MAX, need a test case
      summary64K[i * 3 + 1] = 0.0f; // probably should be -FLT_MAX, need a test case
      summary64K[i * 3 + 2] = 0.0f; // just padding
   }

   // Recalc block-level summary (mRMS already calculated)
   min = summary64K[0];
   max = summary64K[1];

   for (decltype(sumLen) i = 1; i < sumLen; i++) {
      if (summary64K[3*i] < min)
         min = summary64K[3*i];
      if (summary64K[3*i+1] > max)
         max = summary64K[3*i+1];
   }

   mMin = min;
   mMax = max;
}

static void ComputeMinMax256(float *summary256,
                             float *outMin, float *outMax, int *outBads)
{
   float min, max;
   int i;
   int bad = 0;

   min = 1.0;
   max = -1.0;
   for(i=0; i<256; i++) {
      if (summary256[3*i] < min)
         min = summary256[3*i];
      else if (!(summary256[3*i] >= min))
         bad++;
      if (summary256[3*i+1] > max)
         max = summary256[3*i+1];
      else if (!(summary256[3*i+1] <= max))
         bad++;
      if (std::isnan(summary256[3*i+2]))
         bad++;
      if (summary256[3*i+2] < -1 || summary256[3*i+2] > 1)
         bad++;
   }

   *outMin = min;
   *outMax = max;
   *outBads = bad;
}

/// Byte-swap the summary data, in case it was saved by a system
/// on a different platform
void BlockFile::FixSummary(void *data)
{
   if (mSummaryInfo.format != floatSample ||
       mSummaryInfo.fields != 3)
      return;

   float *summary64K = (float *)((char *)data + mSummaryInfo.offset64K);
   float *summary256 = (float *)((char *)data + mSummaryInfo.offset256);

   float min, max;
   int bad;
   int i;

   ComputeMinMax256(summary256, &min, &max, &bad);

   if (min != summary64K[0] || max != summary64K[1] || bad > 0) {
      unsigned int *buffer = (unsigned int *)data;
      int len = mSummaryInfo.totalSummaryBytes / 4;

      for(i=0; i<len; i++)
         buffer[i] = wxUINT32_SWAP_ALWAYS(buffer[i]);

      ComputeMinMax256(summary256, &min, &max, &bad);
      if (min == summary64K[0] && max == summary64K[1] && bad == 0) {
         // The byte-swapping worked!
         return;
      }

      // Hmmm, no better, we should swap back

      for(i=0; i<len; i++)
         buffer[i] = wxUINT32_SWAP_ALWAYS(buffer[i]);
   }
}

/// Retrieves the minimum, maximum, and maximum RMS of the
/// specified sample data in this block.
///
/// @param start The offset in this block where the region should begin
/// @param len   The number of samples to include in the region
/// @param *outMin A pointer to where the minimum value for this region
///                should be stored
/// @param *outMax A pointer to where the maximum value for this region
///                should be stored
/// @param *outRMS A pointer to where the maximum RMS value for this
///                region should be stored.
void BlockFile::GetMinMax(size_t start, size_t len,
                  float *outMin, float *outMax, float *outRMS) const
{
   // TODO: actually use summaries
   SampleBuffer blockData(len, floatSample);
   this->ReadData(blockData.ptr(), floatSample, start, len);

   float min = FLT_MAX;
   float max = -FLT_MAX;
   float sumsq = 0;

   for( decltype(len) i = 0; i < len; i++ )
   {
      float sample = ((float*)blockData.ptr())[i];

      if( sample > max )
         max = sample;
      if( sample < min )
         min = sample;
      sumsq += (sample*sample);
   }

   *outMin = min;
   *outMax = max;
   *outRMS = sqrt(sumsq/len);
}

/// Retrieves the minimum, maximum, and maximum RMS of this entire
/// block.  This is faster than the other GetMinMax function since
/// these values are already computed.
///
/// @param *outMin A pointer to where the minimum value for this block
///                should be stored
/// @param *outMax A pointer to where the maximum value for this block
///                should be stored
/// @param *outRMS A pointer to where the maximum RMS value for this
///                block should be stored.
void BlockFile::GetMinMax(float *outMin, float *outMax, float *outRMS) const
{
   *outMin = mMin;
   *outMax = mMax;
   *outRMS = mRMS;
}

/// Retrieves a portion of the 256-byte summary buffer from this BlockFile.  This
/// data provides information about the minimum value, the maximum
/// value, and the maximum RMS value for every group of 256 samples in the
/// file.
///
/// @param *buffer The area where the summary information will be
///                written.  It must be at least len*3 long.
/// @param start   The offset in 256-sample increments
/// @param len     The number of 256-sample summary frames to read
bool BlockFile::Read256(float *buffer,
                        size_t start, size_t len)
{
   wxASSERT(start >= 0);

   char *summary = new char[mSummaryInfo.totalSummaryBytes];
   // FIXME: TRAP_ERR ReadSummary() could return fail.
   this->ReadSummary(summary);

   start = std::min( start, mSummaryInfo.frames256 );
   len = std::min( len, mSummaryInfo.frames256 - start );

   CopySamples(summary + mSummaryInfo.offset256 + (start * mSummaryInfo.bytesPerFrame),
               mSummaryInfo.format,
               (samplePtr)buffer, floatSample, len * mSummaryInfo.fields);

   if (mSummaryInfo.fields == 2) {
      // No RMS info
      for(auto i = len; i--;) {
         buffer[3*i+2] = (fabs(buffer[2*i]) + fabs(buffer[2*i+1]))/4.0;
         buffer[3*i+1] = buffer[2*i+1];
         buffer[3*i] = buffer[2*i];
      }
   }

   delete[] summary;

   return true;
}

/// Retrieves a portion of the 64K summary buffer from this BlockFile.  This
/// data provides information about the minimum value, the maximum
/// value, and the maximum RMS value for every group of 64K samples in the
/// file.
///
/// @param *buffer The area where the summary information will be
///                written.  It must be at least len*3 long.
/// @param start   The offset in 64K-sample increments
/// @param len     The number of 64K-sample summary frames to read
bool BlockFile::Read64K(float *buffer,
                        size_t start, size_t len)
{
   wxASSERT(start >= 0);

   char *summary = new char[mSummaryInfo.totalSummaryBytes];
   // FIXME: TRAP_ERR ReadSummary() could return fail.
   this->ReadSummary(summary);

   start = std::min( start, mSummaryInfo.frames64K );
   len = std::min( len, mSummaryInfo.frames64K - start );

   CopySamples(summary + mSummaryInfo.offset64K +
               (start * mSummaryInfo.bytesPerFrame),
               mSummaryInfo.format,
               (samplePtr)buffer, floatSample, len*mSummaryInfo.fields);

   if (mSummaryInfo.fields == 2) {
      // No RMS info; make guess
      for(auto i = len; i--;) {
         buffer[3*i+2] = (fabs(buffer[2*i]) + fabs(buffer[2*i+1]))/4.0;
         buffer[3*i+1] = buffer[2*i+1];
         buffer[3*i] = buffer[2*i];
      }
   }

   delete[] summary;

   return true;
}

/// Constructs an AliasBlockFile based on the given information about
/// the aliased file.
///
/// Note that derived classes /must/ call AliasBlockFile::WriteSummary()
/// in their constructors for the summary file to be correctly written
/// to disk.
///
/// @param baseFileName The name of the summary file to be written, but
///                     without an extension.  This constructor will add
///                     the appropriate extension before passing it to
///                     BlockFile::BlockFile
/// @param aliasedFileName The name of the file where the audio data for
///                     this block actually exists.
/// @param aliasStart   The offset in the aliased file where this block's
///                     data begins
/// @param aliasLen     The length of this block's data in the aliased
///                     file.
/// @param aliasChannel The channel where this block's data is located in
///                     the aliased file
AliasBlockFile::AliasBlockFile(wxFileNameWrapper &&baseFileName,
                               wxFileNameWrapper &&aliasedFileName,
                               sampleCount aliasStart,
                               size_t aliasLen, int aliasChannel):
   BlockFile {
      (baseFileName.SetExt(wxT("auf")), std::move(baseFileName)),
      aliasLen
   },
   mAliasedFileName(std::move(aliasedFileName)),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
   mSilentAliasLog=FALSE;
}

AliasBlockFile::AliasBlockFile(wxFileNameWrapper &&existingSummaryFileName,
                               wxFileNameWrapper &&aliasedFileName,
                               sampleCount aliasStart,
                               size_t aliasLen,
                               int aliasChannel,
                               float min, float max, float rms):
   BlockFile{ std::move(existingSummaryFileName), aliasLen },
   mAliasedFileName(std::move(aliasedFileName)),
   mAliasStart(aliasStart),
   mAliasChannel(aliasChannel)
{
   mMin = min;
   mMax = max;
   mRMS = rms;
   mSilentAliasLog=FALSE;
}
/// Write the summary to disk.  Derived classes must call this method
/// from their constructors for the summary to be correctly written.
/// It uses the derived class's ReadData() to retrieve the data to
/// summarize.
void AliasBlockFile::WriteSummary()
{
   // Now checked carefully in the DirManager
   //wxASSERT( !wxFileExists(FILENAME(mFileName.GetFullPath())));

   // I would much rather have this code as part of the constructor, but
   // I can't call virtual functions from the constructor.  So we just
   // need to ensure that every derived class calls this in *its* constructor
   wxFFile summaryFile(mFileName.GetFullPath(), wxT("wb"));

   if( !summaryFile.IsOpened() ){
      // Never silence the Log w.r.t write errors; they always count
      // as NEW errors
      wxLogError(wxT("Unable to write summary data to file %s"),
                   mFileName.GetFullPath().c_str());
      // If we can't write, there's nothing to do.
      return;
   }

   // To build the summary data, call ReadData (implemented by the
   // derived classes) to get the sample data
   SampleBuffer sampleData(mLen, floatSample);
   this->ReadData(sampleData.ptr(), floatSample, 0, mLen);

   ArrayOf<char> cleanup;
   void *summaryData = BlockFile::CalcSummary(sampleData.ptr(), mLen,
                                            floatSample, cleanup);
   summaryFile.Write(summaryData, mSummaryInfo.totalSummaryBytes);
}

AliasBlockFile::~AliasBlockFile()
{
}

/// Read the summary of this alias block from disk.  Since the audio data
/// is elsewhere, this consists of reading the entire summary file.
///
/// @param *data The buffer where the summary data will be stored.  It must
///              be at least mSummaryInfo.totalSummaryBytes long.
bool AliasBlockFile::ReadSummary(void *data)
{
   wxFFile summaryFile(mFileName.GetFullPath(), wxT("rb"));

   {
      Maybe<wxLogNull> silence{};
      if (mSilentLog)
         silence.create();

      if (!summaryFile.IsOpened()){

         // NEW model; we need to return valid data
         memset(data, 0, (size_t)mSummaryInfo.totalSummaryBytes);

         // we silence the logging for this operation in this object
         // after first occurrence of error; it's already reported and
         // spewing at the user will complicate the user's ability to
         // deal
         mSilentLog = TRUE;
         return true;

      }
      else mSilentLog = FALSE; // worked properly, any future error is NEW
   }

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   FixSummary(data);

   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Modify this block to point at a different file.  This is generally
/// looked down on, but it is necessary in one case: see
/// DirManager::EnsureSafeFilename().
void AliasBlockFile::ChangeAliasedFileName(wxFileNameWrapper &&newAliasedFile)
{
   mAliasedFileName = std::move(newAliasedFile);
}

auto AliasBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   wxFFile summaryFile(mFileName.GetFullPath());
   return summaryFile.Length();
}

