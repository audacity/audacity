/**********************************************************************

Audacity: A Digital Audio Editor

SqliteSampleBlock.cpp

Paul Licameli -- split from SampleBlock.cpp and SampleBlock.h

**********************************************************************/

#include <float.h>
#include <sqlite3.h>

#include "DBConnection.h"
#include "ProjectFileIO.h"
#include "SampleFormat.h"
#include "xml/XMLTagHandler.h"

#include "SampleBlock.h" // to inherit

///\brief Implementation of @ref SampleBlock using Sqlite database
class SqliteSampleBlock final : public SampleBlock
{
public:

   explicit SqliteSampleBlock(
      const std::shared_ptr<ConnectionPtr> &ppConnection);
   ~SqliteSampleBlock() override;

   void CloseLock() override;

   void SetSamples(samplePtr src, size_t numsamples, sampleFormat srcformat);

   void SetSilent(size_t numsamples, sampleFormat srcformat);

   void Commit();

   void Delete();

   SampleBlockID GetBlockID() const override;

   size_t DoGetSamples(samplePtr dest,
                       sampleFormat destformat,
                       size_t sampleoffset,
                       size_t numsamples) override;
   sampleFormat GetSampleFormat() const;
   size_t GetSampleCount() const override;

   bool GetSummary256(float *dest, size_t frameoffset, size_t numframes) override;
   bool GetSummary64k(float *dest, size_t frameoffset, size_t numframes) override;
   double GetSumMin() const;
   double GetSumMax() const;
   double GetSumRms() const;

   /// Gets extreme values for the specified region
   MinMaxRMS DoGetMinMaxRMS(size_t start, size_t len) override;

   /// Gets extreme values for the entire block
   MinMaxRMS DoGetMinMaxRMS() const override;

   size_t GetSpaceUsage() const override;
   void SaveXML(XMLWriter &xmlFile) override;

private:
   void Load(SampleBlockID sbid);
   bool GetSummary(float *dest,
                   size_t frameoffset,
                   size_t numframes,
                   sqlite3_stmt *stmt,
                   size_t srcbytes);
   size_t GetBlob(void *dest,
                  sampleFormat destformat,
                  sqlite3_stmt *stmt,
                  sampleFormat srcformat,
                  size_t srcoffset,
                  size_t srcbytes);
   void CalcSummary();

private:
   DBConnection *Conn() const
   {
      auto &pConnection = mppConnection->mpConnection;
      if (!pConnection) {
         throw SimpleMessageBoxException
         {
            XO("Failed to open the project's database")
         };
      }
      return pConnection.get();
   }
   sqlite3 *DB() const
   {
      return Conn()->DB();
   }

   friend SqliteSampleBlockFactory;

   const std::shared_ptr<ConnectionPtr> mppConnection;
   bool mValid;
   bool mDirty;
   bool mSilent;
   bool mLocked = false;

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

#if defined(WORDS_BIGENDIAN)
#error All sample block data is little endian...big endian not yet supported
#endif
};

///\brief Implementation of @ref SampleBlockFactory using Sqlite database
class SqliteSampleBlockFactory final : public SampleBlockFactory
{
public:
   explicit SqliteSampleBlockFactory( AudacityProject &project );

   ~SqliteSampleBlockFactory() override;

   SampleBlockPtr DoGet(SampleBlockID sbid) override;

   SampleBlockPtr DoCreate(samplePtr src,
      size_t numsamples,
      sampleFormat srcformat) override;

   SampleBlockPtr DoCreateSilent(
      size_t numsamples,
      sampleFormat srcformat) override;

   SampleBlockPtr DoCreateFromXML(
      sampleFormat srcformat,
      const wxChar **attrs) override;

private:
   const std::shared_ptr<ConnectionPtr> mppConnection;
};

SqliteSampleBlockFactory::SqliteSampleBlockFactory( AudacityProject &project )
   : mppConnection{ ConnectionPtr::Get(project).shared_from_this() }
{
   
}

SqliteSampleBlockFactory::~SqliteSampleBlockFactory() = default;

SampleBlockPtr SqliteSampleBlockFactory::DoCreate(
   samplePtr src, size_t numsamples, sampleFormat srcformat )
{
   auto sb = std::make_shared<SqliteSampleBlock>(mppConnection);
   sb->SetSamples(src, numsamples, srcformat);
   return sb;
}

SampleBlockPtr SqliteSampleBlockFactory::DoCreateSilent(
   size_t numsamples, sampleFormat srcformat )
{
   auto sb = std::make_shared<SqliteSampleBlock>(mppConnection);
   sb->SetSilent(numsamples, srcformat);
   return sb;
}


SampleBlockPtr SqliteSampleBlockFactory::DoCreateFromXML(
   sampleFormat srcformat, const wxChar **attrs )
{
   auto sb = std::make_shared<SqliteSampleBlock>(mppConnection);
   sb->mSampleFormat = srcformat;

   int found = 0;

   // loop through attrs, which is a null-terminated list of attribute-value pairs
   while(*attrs)
   {
      const wxChar *attr = *attrs++;
      const wxChar *value = *attrs++;

      if (!value)
      {
         break;
      }

      const wxString strValue = value;   // promote string, we need this for all
      double dblValue;
      long long nValue;

      if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLongLong(&nValue) && (nValue >= 0))
      {
         if (wxStrcmp(attr, wxT("blockid")) == 0)
         {
            // This may throw
            sb->Load((SampleBlockID) nValue);
            found++;
         }
         else if (wxStrcmp(attr, wxT("samplecount")) == 0)
         {
            sb->mSampleCount = nValue;
            sb->mSampleBytes = sb->mSampleCount * SAMPLE_SIZE(sb->mSampleFormat);
            found++;
         }
      }
      else if (XMLValueChecker::IsGoodString(strValue) && Internat::CompatibleToDouble(strValue, &dblValue))
      {
         if (wxStricmp(attr, wxT("min")) == 0)
         {
            sb->mSumMin = dblValue;
            found++;
         }
         else if (wxStricmp(attr, wxT("max")) == 0)
         {
            sb->mSumMax = dblValue;
            found++;
         }
         else if ((wxStricmp(attr, wxT("rms")) == 0) && (dblValue >= 0.0))
         {
            sb->mSumRms = dblValue;
            found++;
         }
      }
   }

   // Were all attributes found?
   if (found != 5)
   {
      return nullptr;
   }
   
   return sb;
}

SampleBlockPtr SqliteSampleBlockFactory::DoGet( SampleBlockID sbid )
{
   auto sb = std::make_shared<SqliteSampleBlock>(mppConnection);
   sb->Load(sbid);
   return sb;
}

SqliteSampleBlock::SqliteSampleBlock(
   const std::shared_ptr<ConnectionPtr> &ppConnection)
:  mppConnection(ppConnection)
{
   mValid = false;
   mSilent = false;

   mBlockID = 0;

   mSampleFormat = floatSample;
   mSampleBytes = 0;
   mSampleCount = 0;

   mSummary256Bytes = 0;
   mSummary64kBytes = 0;
   mSumMin = 0.0;
   mSumMax = 0.0;
   mSumRms = 0.0;
}

SqliteSampleBlock::~SqliteSampleBlock()
{
   // See ProjectFileIO::Bypass() for a description of mIO.mBypass
   if (!mLocked && !Conn()->ShouldBypass())
   {
      // In case Delete throws, don't let an exception escape a destructor,
      // but we can still enqueue the delayed handler so that an error message
      // is presented to the user.
      // The failure in this case may be a less harmful waste of space in the
      // database, which should not cause aborting of the attempted edit.
      GuardedCall( [this]{ Delete(); } );
   }
}

void SqliteSampleBlock::CloseLock()
{
   mLocked = true;
}

SampleBlockID SqliteSampleBlock::GetBlockID() const
{
   return mBlockID;
}

sampleFormat SqliteSampleBlock::GetSampleFormat() const
{
   return mSampleFormat;
}

size_t SqliteSampleBlock::GetSampleCount() const
{
   return mSampleCount;
}

size_t SqliteSampleBlock::DoGetSamples(samplePtr dest,
                                     sampleFormat destformat,
                                     size_t sampleoffset,
                                     size_t numsamples)
{
   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::GetSamples,
      "SELECT samples FROM sampleblocks WHERE blockid = ?1;");

   return GetBlob(dest,
                  destformat,
                  stmt,
                  mSampleFormat,
                  sampleoffset * SAMPLE_SIZE(mSampleFormat),
                  numsamples * SAMPLE_SIZE(mSampleFormat)) / SAMPLE_SIZE(mSampleFormat);
}

void SqliteSampleBlock::SetSamples(samplePtr src,
                                   size_t numsamples,
                                   sampleFormat srcformat)
{
   mSampleFormat = srcformat;
   mSampleCount = numsamples;
   mSampleBytes = mSampleCount * SAMPLE_SIZE(mSampleFormat);
   mSamples.reinit(mSampleBytes);
   memcpy(mSamples.get(), src, mSampleBytes);

   CalcSummary();

   Commit();
}

void SqliteSampleBlock::SetSilent(size_t numsamples, sampleFormat srcformat)
{
   mSampleFormat = srcformat;

   mSampleCount = numsamples;
   mSampleBytes = mSampleCount * SAMPLE_SIZE(mSampleFormat);
   mSamples.reinit(mSampleBytes);
   memset(mSamples.get(), 0, mSampleBytes);

   CalcSummary();

   mSilent = true;

   Commit();
}

bool SqliteSampleBlock::GetSummary256(float *dest,
                                      size_t frameoffset,
                                      size_t numframes)
{
   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::GetSummary256,
      "SELECT summary256 FROM sampleblocks WHERE blockid = ?1;");

   return GetSummary(dest, frameoffset, numframes, stmt, mSummary256Bytes);
}

bool SqliteSampleBlock::GetSummary64k(float *dest,
                                      size_t frameoffset,
                                      size_t numframes)
{
   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::GetSummary64k,
      "SELECT summary64k FROM sampleblocks WHERE blockid = ?1;");

   return GetSummary(dest, frameoffset, numframes, stmt, mSummary256Bytes);
}

bool SqliteSampleBlock::GetSummary(float *dest,
                                   size_t frameoffset,
                                   size_t numframes,
                                   sqlite3_stmt *stmt,
                                   size_t srcbytes)
{
   return GetBlob(dest,
                  floatSample,
                  stmt,
                  floatSample,
                  frameoffset * 3 * SAMPLE_SIZE(floatSample),
                  numframes * 3 * SAMPLE_SIZE(floatSample)) / 3 / SAMPLE_SIZE(floatSample);
}

double SqliteSampleBlock::GetSumMin() const
{
   return mSumMin;
}

double SqliteSampleBlock::GetSumMax() const
{
   return mSumMax;
}

double SqliteSampleBlock::GetSumRms() const
{
   return mSumRms;
}

/// Retrieves the minimum, maximum, and maximum RMS of the
/// specified sample data in this block.
///
/// @param start The offset in this block where the region should begin
/// @param len   The number of samples to include in the region
MinMaxRMS SqliteSampleBlock::DoGetMinMaxRMS(size_t start, size_t len)
{
   float min = FLT_MAX;
   float max = -FLT_MAX;
   float sumsq = 0;

   if (!mValid && mBlockID)
   {
      Load(mBlockID);
   }

   if (start < mSampleCount)
   {
      len = std::min(len, mSampleCount - start);

      // TODO: actually use summaries
      SampleBuffer blockData(len, floatSample);
      float *samples = (float *) blockData.ptr();

      size_t copied = DoGetSamples((samplePtr) samples, floatSample, start, len);
      for (size_t i = 0; i < copied; ++i, ++samples)
      {
         float sample = *samples;

         if (sample > max)
         {
            max = sample;
         }

         if (sample < min)
         {
            min = sample;
         }

         sumsq += (sample * sample);
      }
   }

   return { min, max, (float) sqrt(sumsq / len) };
}

/// Retrieves the minimum, maximum, and maximum RMS of this entire
/// block.  This is faster than the other GetMinMax function since
/// these values are already computed.
MinMaxRMS SqliteSampleBlock::DoGetMinMaxRMS() const
{
   return { (float) mSumMin, (float) mSumMax, (float) mSumRms };
}

size_t SqliteSampleBlock::GetSpaceUsage() const
{
   return ProjectFileIO::GetDiskUsage(Conn(), mBlockID);
}

size_t SqliteSampleBlock::GetBlob(void *dest,
                                  sampleFormat destformat,
                                  sqlite3_stmt *stmt,
                                  sampleFormat srcformat,
                                  size_t srcoffset,
                                  size_t srcbytes)
{
   auto db = DB();

   wxASSERT(mBlockID > 0);

   if (!mValid && mBlockID)
   {
      Load(mBlockID);
   }

   int rc;
   size_t minbytes = 0;

   // Bind statement paraemters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, mBlockID))
   {
      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_ROW)
   {
      wxLogDebug(wxT("SqliteSampleBlock::GetBlob - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      throw SimpleMessageBoxException{ XO("Failed to retrieve project data") };
   }

   // Retrieve returned data
   samplePtr src = (samplePtr) sqlite3_column_blob(stmt, 0);
   size_t blobbytes = (size_t) sqlite3_column_bytes(stmt, 0);

   srcoffset = std::min(srcoffset, blobbytes);
   minbytes = std::min(srcbytes, blobbytes - srcoffset);

   if (srcoffset != 0)
   {
      srcoffset += 0;
   }

   CopySamples(src + srcoffset,
               srcformat,
               (samplePtr) dest,
               destformat,
               minbytes / SAMPLE_SIZE(srcformat));

   dest = ((samplePtr) dest) + minbytes;

   if (srcbytes - minbytes)
   {
      memset(dest, 0, srcbytes - minbytes);
   }

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);

   return srcbytes;
}

void SqliteSampleBlock::Load(SampleBlockID sbid)
{
   auto db = DB();
   int rc;

   wxASSERT(sbid > 0);

   mValid = false;
   mSummary256Bytes = 0;
   mSummary64kBytes = 0;
   mSampleCount = 0;
   mSampleBytes = 0;
   mSumMin = FLT_MAX;
   mSumMax = -FLT_MAX;
   mSumMin = 0.0;

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::LoadSampleBlock,
      "SELECT sampleformat, summin, summax, sumrms,"
      "       length('summary256'), length('summary64k'), length('samples')"
      "  FROM sampleblocks WHERE blockid = ?1;");

   // Bind statement paraemters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, sbid))
   {
      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_ROW)
   {
      wxLogDebug(wxT("SqliteSampleBlock::Load - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      throw SimpleMessageBoxException{ XO("Failed to retrieve sample block") };
   }

   // Retrieve returned data
   mBlockID = sbid;
   mSampleFormat = (sampleFormat) sqlite3_column_int(stmt, 0);
   mSumMin = sqlite3_column_double(stmt, 1);
   mSumMax = sqlite3_column_double(stmt, 2);
   mSumRms = sqlite3_column_double(stmt, 3);
   mSummary256Bytes = sqlite3_column_int(stmt, 4);
   mSummary64kBytes = sqlite3_column_int(stmt, 5);
   mSampleBytes = sqlite3_column_int(stmt, 6);
   mSampleCount = mSampleBytes / SAMPLE_SIZE(mSampleFormat);

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);

   mValid = true;
}

void SqliteSampleBlock::Commit()
{
   auto db = DB();
   int rc;

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::InsertSampleBlock,
      "INSERT INTO sampleblocks (sampleformat, summin, summax, sumrms,"
      "                          summary256, summary64k, samples)"
      "                         VALUES(?1,?2,?3,?4,?5,?6,?7);");

   // Bind statement paraemters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int(stmt, 1, mSampleFormat) ||
       sqlite3_bind_double(stmt, 2, mSumMin) ||
       sqlite3_bind_double(stmt, 3, mSumMax) ||
       sqlite3_bind_double(stmt, 4, mSumRms) ||
       sqlite3_bind_blob(stmt, 5, mSummary256.get(), mSummary256Bytes, SQLITE_STATIC) ||
       sqlite3_bind_blob(stmt, 6, mSummary64k.get(), mSummary64kBytes, SQLITE_STATIC) ||
       sqlite3_bind_blob(stmt, 7, mSamples.get(), mSampleBytes, SQLITE_STATIC))
   {
      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }
 
   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      wxLogDebug(wxT("SqliteSampleBlock::Commit - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      throw SimpleMessageBoxException{ XO("Failed to add sample block") };
   }

   // Retrieve returned data
   mBlockID = sqlite3_last_insert_rowid(db);

   // Reset local arrays
   mSamples.reset();
   mSummary256.reset();
   mSummary64k.reset();

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);

   mValid = true;
}

void SqliteSampleBlock::Delete()
{
   auto db = DB();
   int rc;

   wxASSERT(mBlockID > 0);

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::DeleteSampleBlock,
      "DELETE FROM sampleblocks WHERE blockid = ?1;");

   // Bind statement paraemters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, mBlockID))
   {
      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      wxLogDebug(wxT("SqliteSampleBlock::Load - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      throw SimpleMessageBoxException{ XO("Failed to delete sample block") };
   }

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);
}

void SqliteSampleBlock::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.WriteAttr(wxT("blockid"), mBlockID);
   xmlFile.WriteAttr(wxT("samplecount"), mSampleCount);
   xmlFile.WriteAttr(wxT("len256"), mSummary256Bytes);
   xmlFile.WriteAttr(wxT("len64k"), mSummary64kBytes);
   xmlFile.WriteAttr(wxT("min"), mSumMin);
   xmlFile.WriteAttr(wxT("max"), mSumMax);
   xmlFile.WriteAttr(wxT("rms"), mSumRms);
}

/// Calculates summary block data describing this sample data.
///
/// This method also has the side effect of setting the mSumMin,
/// mSumMax, and mSumRms members of this class.
///
/// @param buffer A buffer containing the sample data to be analyzed
/// @param len    The length of the sample data
/// @param format The format of the sample data.
void SqliteSampleBlock::CalcSummary()
{
   Floats samplebuffer;
   float *samples;

   if (mSampleFormat == floatSample)
   {
      samples = (float *) mSamples.get();
   }
   else
   {
      samplebuffer.reinit((unsigned) mSampleCount);
      CopySamples(mSamples.get(),
                  mSampleFormat,
                  (samplePtr) samplebuffer.get(),
                  floatSample,
                  mSampleCount);
      samples = samplebuffer.get();
   }

   int fields = 3; /* min, max, rms */
   int bytesPerFrame = fields * sizeof(float);
   int frames64k = (mSampleCount + 65535) / 65536;
   int frames256 = frames64k * 256;
   
   mSummary256Bytes = frames256 * bytesPerFrame;
   mSummary64kBytes = frames64k * bytesPerFrame;

   mSummary256.reinit(mSummary256Bytes);
   mSummary64k.reinit(mSummary64kBytes);

   float *summary256 = (float *) mSummary256.get();
   float *summary64k = (float *) mSummary64k.get();

   float min;
   float max;
   float sumsq;
   double totalSquares = 0.0;
   double fraction = 0.0;

   // Recalc 256 summaries
   int sumLen = (mSampleCount + 255) / 256;
   int summaries = 256;

   for (int i = 0; i < sumLen; ++i)
   {
      min = samples[i * 256];
      max = samples[i * 256];
      sumsq = min * min;

      int jcount = 256;
      if (jcount > mSampleCount - i * 256)
      {
         jcount = mSampleCount - i * 256;
         fraction = 1.0 - (jcount / 256.0);
      }

      for (int j = 1; j < jcount; ++j)
      {
         float f1 = samples[i * 256 + j];
         sumsq += f1 * f1;

         if (f1 < min)
         {
            min = f1;
         }
         else if (f1 > max)
         {
            max = f1;
         }
      }

      totalSquares += sumsq;

      summary256[i * 3] = min;
      summary256[i * 3 + 1] = max;
      // The rms is correct, but this may be for less than 256 samples in last loop.
      summary256[i * 3 + 2] = (float) sqrt(sumsq / jcount);
   }

   for (int i = sumLen; i < frames256; ++i)
   {
      // filling in the remaining bits with non-harming/contributing values
      // rms values are not "non-harming", so keep count of them:
      summaries--;
      summary256[i * 3] = FLT_MAX;        // min
      summary256[i * 3 + 1] = -FLT_MAX;   // max
      summary256[i * 3 + 2] = 0.0f;       // rms
   }

   // Calculate now while we can do it accurately
   mSumRms = sqrt(totalSquares / mSampleCount);

   // Recalc 64K summaries
   sumLen = (mSampleCount + 65535) / 65536;

   for (int i = 0; i < sumLen; ++i)
   {
      min = summary256[3 * i * 256];
      max = summary256[3 * i * 256 + 1];
      sumsq = summary256[3 * i * 256 + 2];
      sumsq *= sumsq;

      for (int j = 1; j < 256; ++j)
      {
         // we can overflow the useful summary256 values here, but have put
         // non-harmful values in them
         if (summary256[3 * (i * 256 + j)] < min)
         {
            min = summary256[3 * (i * 256 + j)];
         }

         if (summary256[3 * (i * 256 + j) + 1] > max)
         {
            max = summary256[3 * (i * 256 + j) + 1];
         }

         float r1 = summary256[3 * (i * 256 + j) + 2];
         sumsq += r1 * r1;
      }

      double denom = (i < sumLen - 1) ? 256.0 : summaries - fraction;
      float rms = (float) sqrt(sumsq / denom);

      summary64k[i * 3] = min;
      summary64k[i * 3 + 1] = max;
      summary64k[i * 3 + 2] = rms;
   }

   for (int i = sumLen; i < frames64k; ++i)
   {
      wxASSERT_MSG(false, wxT("Out of data for mSummaryInfo"));   // Do we ever get here?

      summary64k[i * 3] = 0.0f;     // probably should be FLT_MAX, need a test case
      summary64k[i * 3 + 1] = 0.0f; // probably should be -FLT_MAX, need a test case
      summary64k[i * 3 + 2] = 0.0f; // just padding
   }

   // Recalc block-level summary (mRMS already calculated)
   min = summary64k[0];
   max = summary64k[1];

   for (int i = 1; i < sumLen; ++i)
   {
      if (summary64k[i * 3] < min)
      {
         min = summary64k[i * 3];
      }

      if (summary64k[i * 3 + 1] > max)
      {
         max = summary64k[i * 3 + 1];
      }
   }

   mSumMin = min;
   mSumMax = max;
}

// Inject our database implementation at startup
static struct Injector
{
   Injector()
   {
      // Do this some time before the first project is created
      (void) SampleBlockFactory::RegisterFactoryFactory(
         []( AudacityProject &project )
         {
            return std::make_shared<SqliteSampleBlockFactory>( project );
         }
      );
   }
} injector;
