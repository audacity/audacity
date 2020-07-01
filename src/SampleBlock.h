/**********************************************************************

Audacity: A Digital Audio Editor

SampleBlock.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_BLOCK__
#define __AUDACITY_SAMPLE_BLOCK__

#include "ClientData.h" // to inherit

#include <sqlite3.h>

class AudacityProject;
class ProjectFileIO;
class XMLWriter;

class SampleBlock;
using SampleBlockPtr = std::shared_ptr<SampleBlock>;
using SampleBlockID = sqlite3_int64;

class MinMaxRMS
{
public:
   float min;
   float max;
   float RMS;
};

class SampleBlock
{
public:
   SampleBlock(AudacityProject *project);
   virtual ~SampleBlock();

   static SampleBlockPtr Get(AudacityProject *project,
                             SampleBlockID sbid);

   static SampleBlockPtr Create(AudacityProject *project,
                                samplePtr src,
                                size_t numsamples,
                                sampleFormat srcformat);

   static SampleBlockPtr CreateSilent(AudacityProject *project,
                                      size_t numsamples,
                                      sampleFormat srcformat);

   static SampleBlockPtr CreateFromXML(AudacityProject *project,
                                       sampleFormat srcformat,
                                       const wxChar **attrs);

   void Lock();
   void Unlock();
   void CloseLock();

   bool SetSamples(samplePtr src, size_t numsamples, sampleFormat srcformat);

   bool SetSilent(size_t numsamples, sampleFormat srcformat);

   bool Commit();

   void Delete();

   SampleBlockID GetBlockID();

   size_t GetSamples(samplePtr dest,
                     sampleFormat destformat,
                     size_t sampleoffset,
                     size_t numsamples);
   sampleFormat GetSampleFormat() const;
   size_t GetSampleCount() const;

   bool GetSummary256(float *dest, size_t frameoffset, size_t numframes);
   bool GetSummary64k(float *dest, size_t frameoffset, size_t numframes);
   double GetSumMin() const;
   double GetSumMax() const;
   double GetSumRms() const;

   /// Gets extreme values for the specified region
   MinMaxRMS GetMinMaxRMS(size_t start, size_t len);

   /// Gets extreme values for the entire block
   MinMaxRMS GetMinMaxRMS() const;

   size_t GetSpaceUsage() const;
   void SaveXML(XMLWriter &xmlFile);

private:
   bool Load(SampleBlockID sbid);
   bool GetSummary(float *dest,
                   size_t frameoffset,
                   size_t numframes,
                   const char *srccolumn,
                   size_t srcbytes);
   size_t GetBlob(void *dest,
                  sampleFormat destformat,
                  const char *srccolumn,
                  sampleFormat srcformat,
                  size_t srcoffset,
                  size_t srcbytes);
   void CalcSummary();

private:
   AudacityProject *mProject;
   ProjectFileIO & mIO;
   bool mValid;
   bool mDirty;
   bool mSilent;
   int mRefCnt;

   SampleBlockID mBlockID;

   ArrayOf<char> mSamples;
   size_t mSampleBytes;
   size_t mSampleCount;
   sampleFormat mSampleFormat;

   ArrayOf<char> mSummary256;
   size_t mSummary256Bytes;
   ArrayOf<char> mSummary64k;
   size_t mSummary64kBytes;
   double mSumMin;
   double mSumMax;
   double mSumRms;

   const char *columns =
      "sampleformat, summin, summax, sumrms, summary256, summary64k, samples";

   friend class ProjectFileIO;
#if defined(WORDS_BIGENDIAN)
#error All sample block data is little endian...big endian not yet supported
#endif
};

#endif
