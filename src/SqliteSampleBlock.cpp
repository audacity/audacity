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
#include "XMLTagHandler.h"

#include "SampleBlock.h" // to inherit

#include "SentryHelper.h"
#include <wx/log.h>

class SqliteSampleBlockFactory;

///\brief Implementation of @ref SampleBlock using Sqlite database
class SqliteSampleBlock final : public SampleBlock
{
public:

   explicit SqliteSampleBlock(
      const std::shared_ptr<SqliteSampleBlockFactory> &pFactory);
   ~SqliteSampleBlock() override;

   void CloseLock() override;

   void SetSamples(
      constSamplePtr src, size_t numsamples, sampleFormat srcformat);

   //! Numbers of bytes needed for 256 and for 64k summaries
   using Sizes = std::pair< size_t, size_t >;
   void Commit(Sizes sizes);

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
   bool IsSilent() const { return mBlockID <= 0; }
   void Load(SampleBlockID sbid);
   bool GetSummary(float *dest,
                   size_t frameoffset,
                   size_t numframes,
                   DBConnection::StatementID id,
                   const char *sql);
   size_t GetBlob(void *dest,
                  sampleFormat destformat,
                  sqlite3_stmt *stmt,
                  sampleFormat srcformat,
                  size_t srcoffset,
                  size_t srcbytes);

   enum {
      fields = 3, /* min, max, rms */
      bytesPerFrame = fields * sizeof(float),
   };
   Sizes SetSizes( size_t numsamples, sampleFormat srcformat );
   void CalcSummary(Sizes sizes);

private:
   //! This must never be called for silent blocks
   /*! @post return value is not null */
   DBConnection *Conn() const;
   sqlite3 *DB() const
   {
      return Conn()->DB();
   }

   friend SqliteSampleBlockFactory;

   const std::shared_ptr<SqliteSampleBlockFactory> mpFactory;
   bool mValid{ false };
   bool mLocked = false;

   SampleBlockID mBlockID{ 0 };

   ArrayOf<char> mSamples;
   size_t mSampleBytes;
   size_t mSampleCount;
   sampleFormat mSampleFormat;

   ArrayOf<char> mSummary256;
   ArrayOf<char> mSummary64k;
   double mSumMin;
   double mSumMax;
   double mSumRms;

#if defined(WORDS_BIGENDIAN)
#error All sample block data is little endian...big endian not yet supported
#endif
};

// Silent blocks use nonpositive id values to encode a length
// and don't occupy any rows in the database; share blocks for repeatedly
// used length values
static std::map< SampleBlockID, std::shared_ptr<SqliteSampleBlock> >
   sSilentBlocks;

///\brief Implementation of @ref SampleBlockFactory using Sqlite database
class SqliteSampleBlockFactory final
   : public SampleBlockFactory
   , public std::enable_shared_from_this<SqliteSampleBlockFactory>
{
public:
   explicit SqliteSampleBlockFactory( AudacityProject &project );

   ~SqliteSampleBlockFactory() override;

   SampleBlockIDs GetActiveBlockIDs() override;

   SampleBlockPtr DoCreate(constSamplePtr src,
      size_t numsamples,
      sampleFormat srcformat) override;

   SampleBlockPtr DoCreateSilent(
      size_t numsamples,
      sampleFormat srcformat) override;

   SampleBlockPtr DoCreateFromXML(
      sampleFormat srcformat,
      const wxChar **attrs) override;

   BlockDeletionCallback SetBlockDeletionCallback(
      BlockDeletionCallback callback ) override;

private:
   friend SqliteSampleBlock;

   const std::shared_ptr<ConnectionPtr> mppConnection;

   // Track all blocks that this factory has created, but don't control
   // their lifetimes (so use weak_ptr)
   // (Must also use weak pointers because the blocks have shared pointers
   // to the factory and we can't have a leaky cycle of shared pointers)
   using AllBlocksMap =
      std::map< SampleBlockID, std::weak_ptr< SqliteSampleBlock > >;
   AllBlocksMap mAllBlocks;

   BlockDeletionCallback mCallback;
};

SqliteSampleBlockFactory::SqliteSampleBlockFactory( AudacityProject &project )
   : mppConnection{ ConnectionPtr::Get(project).shared_from_this() }
{
   
}

SqliteSampleBlockFactory::~SqliteSampleBlockFactory() = default;

SampleBlockPtr SqliteSampleBlockFactory::DoCreate(
   constSamplePtr src, size_t numsamples, sampleFormat srcformat )
{
   auto sb = std::make_shared<SqliteSampleBlock>(shared_from_this());
   sb->SetSamples(src, numsamples, srcformat);
   // block id has now been assigned
   mAllBlocks[ sb->GetBlockID() ] = sb;
   return sb;
}

auto SqliteSampleBlockFactory::GetActiveBlockIDs() -> SampleBlockIDs
{
   SampleBlockIDs result;
   for (auto end = mAllBlocks.end(), it = mAllBlocks.begin(); it != end;) {
      if (it->second.expired())
         // Tighten up the map
         it = mAllBlocks.erase(it);
      else {
         result.insert( it->first );
         ++it;
      }
   }
   return result;
}

SampleBlockPtr SqliteSampleBlockFactory::DoCreateSilent(
   size_t numsamples, sampleFormat )
{
   auto id = -static_cast< SampleBlockID >(numsamples);
   auto &result = sSilentBlocks[ id ];
   if ( !result ) {
      result = std::make_shared<SqliteSampleBlock>(nullptr);
      result->mBlockID = id;

      // Ignore the supplied sample format
      result->SetSizes(numsamples, floatSample);
      result->mValid = true;
   }

   return result;
}


SampleBlockPtr SqliteSampleBlockFactory::DoCreateFromXML(
   sampleFormat srcformat, const wxChar **attrs )
{
   std::shared_ptr<SampleBlock> sb;

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

      if (wxStrcmp(attr, wxT("blockid")) == 0 &&
         XMLValueChecker::IsGoodInt(strValue) && strValue.ToLongLong(&nValue))
      {
         if (nValue <= 0) {
            sb = DoCreateSilent( -nValue, floatSample );
         }
         else {
            // First see if this block id was previously loaded
            auto &wb = mAllBlocks[ nValue ];
            auto pb = wb.lock();
            if (pb)
               // Reuse the block
               sb = pb;
            else {
               // First sight of this id
               auto ssb =
                  std::make_shared<SqliteSampleBlock>(shared_from_this());
               wb = ssb;
               sb = ssb;
               ssb->mSampleFormat = srcformat;
               // This may throw database errors
               // It initializes the rest of the fields
               ssb->Load((SampleBlockID) nValue);
            }
         }
         found++;
      }
   }

  // Were all attributes found?
   if (found != 1)
   {
      return nullptr;
   }

   return sb;
}

auto SqliteSampleBlockFactory::SetBlockDeletionCallback(
   BlockDeletionCallback callback ) -> BlockDeletionCallback
{
   auto result = mCallback;
   mCallback = std::move( callback );
   return result;
}

SqliteSampleBlock::SqliteSampleBlock(
   const std::shared_ptr<SqliteSampleBlockFactory> &pFactory)
:  mpFactory(pFactory)
{
   mSampleFormat = floatSample;
   mSampleBytes = 0;
   mSampleCount = 0;

   mSumMin = 0.0;
   mSumMax = 0.0;
   mSumRms = 0.0;
}

SqliteSampleBlock::~SqliteSampleBlock()
{
   if (mpFactory) {
      auto &callback = mpFactory->mCallback;
      if (callback)
         GuardedCall( [&]{ callback( *this ); } );
   }

   if (IsSilent()) {
      // The block object was constructed but failed to Load() or Commit().
      // Or it's a silent block with no row in the database.
      // Just let the stack unwind.  Don't violate the assertion in
      // Delete(), which may do odd recursive things in debug builds when it
      // yields to the UI to put up a dialog, but then dispatches timer
      // events that try again to finish recording.
      return;
   }

   // See ProjectFileIO::Bypass() for a description of mIO.mBypass
   GuardedCall( [this]{
      if (!mLocked && !Conn()->ShouldBypass())
      {
         // In case Delete throws, don't let an exception escape a destructor,
         // but we can still enqueue the delayed handler so that an error message
         // is presented to the user.
         // The failure in this case may be a less harmful waste of space in the
         // database, which should not cause aborting of the attempted edit.
         Delete();
      }
   } );
}

DBConnection *SqliteSampleBlock::Conn() const
{
   if (!mpFactory)
      return nullptr;

   auto &pConnection = mpFactory->mppConnection->mpConnection;
   if (!pConnection) {
      throw SimpleMessageBoxException
      {
         ExceptionType::Internal,
         XO("Connection to project file is null"),
         XO("Warning"),
         "Error:_Disk_full_or_not_writable"
      };
   }
   return pConnection.get();
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
   if (IsSilent()) {
      auto size = SAMPLE_SIZE(destformat);
      memset(dest, 0, numsamples * size);
      return numsamples;
   }

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

void SqliteSampleBlock::SetSamples(constSamplePtr src,
                                   size_t numsamples,
                                   sampleFormat srcformat)
{
   auto sizes = SetSizes(numsamples, srcformat);
   mSamples.reinit(mSampleBytes);
   memcpy(mSamples.get(), src, mSampleBytes);

   CalcSummary( sizes );

   Commit( sizes );
}

bool SqliteSampleBlock::GetSummary256(float *dest,
                                      size_t frameoffset,
                                      size_t numframes)
{
   return GetSummary(dest, frameoffset, numframes, DBConnection::GetSummary256,
      "SELECT summary256 FROM sampleblocks WHERE blockid = ?1;");
}

bool SqliteSampleBlock::GetSummary64k(float *dest,
                                      size_t frameoffset,
                                      size_t numframes)
{
   return GetSummary(dest, frameoffset, numframes, DBConnection::GetSummary64k,
      "SELECT summary64k FROM sampleblocks WHERE blockid = ?1;");
}

bool SqliteSampleBlock::GetSummary(float *dest,
                                   size_t frameoffset,
                                   size_t numframes,
                                   DBConnection::StatementID id,
                                   const char *sql)
{
   // Non-throwing, it returns true for success
   bool silent = IsSilent();
   if (!silent) {
      // Not a silent block
      try {
         // Prepare and cache statement...automatically finalized at DB close
         auto stmt = Conn()->Prepare(id, sql);
         // Note GetBlob returns a size_t, not a bool
         // REVIEW: An error in GetBlob() will throw an exception.
         GetBlob(dest,
                     floatSample,
                     stmt,
                     floatSample,
                     frameoffset * fields * SAMPLE_SIZE(floatSample),
                     numframes * fields * SAMPLE_SIZE(floatSample));
         return true;
      }
      catch ( const AudacityException & ) {
      }
   }
   memset(dest, 0, 3 * numframes * sizeof( float ));
   // Return true for success only if we didn't catch
   return silent;
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
   if (IsSilent())
      return {};

   float min = FLT_MAX;
   float max = -FLT_MAX;
   float sumsq = 0;

   if (!mValid)
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
   if (IsSilent())
      return 0;
   else
      return ProjectFileIO::GetDiskUsage(*Conn(), mBlockID);
}

size_t SqliteSampleBlock::GetBlob(void *dest,
                                  sampleFormat destformat,
                                  sqlite3_stmt *stmt,
                                  sampleFormat srcformat,
                                  size_t srcoffset,
                                  size_t srcbytes)
{
   auto db = DB();

   wxASSERT(!IsSilent());

   if (!mValid)
   {
      Load(mBlockID);
   }

   int rc;
   size_t minbytes = 0;

   // Bind statement parameters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, mBlockID))
   {
      ADD_EXCEPTION_CONTEXT(
         "sqlite3.rc", std::to_string(sqlite3_errcode(Conn()->DB())));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::GetBlob::bind");

      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_ROW)
   {
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::GetBlob::step");

      wxLogDebug(wxT("SqliteSampleBlock::GetBlob - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      // Actually this can lead to 'Could not read from file' error message
      // but it can also lead to no error message at all and a flat line, 
      // depending on where GetBlob is called from.
      // The latter can happen when repainting the screen.
      // That possibly happens on a very slow machine.  Possibly that's the
      // right trade off when a machine can't keep up?
      // ANSWER-ME: Do we always report an error when we should here?
      Conn()->ThrowException( false );
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

   /*
    Will dithering happen in CopySamples?  Answering this as of 3.0.3 by
    examining all uses.
    
    As this function is called from GetSummary, no, because destination format
    is float.

    There is only one other call to this function, in DoGetSamples.  At one
    call to that function, in DoGetMinMaxRMS, again format is float always.
    
    There is only one other call to DoGetSamples, in SampleBlock::GetSamples().
    In one call to that function, in WaveformView.cpp, again format is float.

    That leaves two calls in Sequence.cpp.  One of those can be proved to be
    used only in copy and paste operations, always supplying the same sample
    format as the samples were stored in, therefore no dither.

    That leaves uses of Sequence::Read().  There are uses of Read() in internal
    operations also easily shown to use only the saved format, and
    GetWaveDisplay() always reads as float.

    The remaining use of Sequence::Read() is in Sequence::Get().  That is used
    by WaveClip::Resample(), always fetching float.  It is also used in
    WaveClip::GetSamples().

    There is only one use of that function not always fetching float, in
    WaveTrack::Get().

    It can be shown that the only paths to WaveTrack::Get() not specifying
    floatSample are in Benchmark, which is only a diagnostic test, and there
    the sample format is the same as what the track was constructed with.

    Therefore, no dithering even there!
    */
   wxASSERT(destformat == floatSample || destformat == srcformat);

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
   mSampleCount = 0;
   mSampleBytes = 0;
   mSumMin = FLT_MAX;
   mSumMax = -FLT_MAX;
   mSumMin = 0.0;

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::LoadSampleBlock,
      "SELECT sampleformat, summin, summax, sumrms,"
      "       length(samples)"
      "  FROM sampleblocks WHERE blockid = ?1;");

   // Bind statement parameters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, sbid))
   {

      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(sqlite3_errcode(Conn()->DB())));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Load::bind");

      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_ROW)
   {

      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Load::step");


      wxLogDebug(wxT("SqliteSampleBlock::Load - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      Conn()->ThrowException( false );
   }

   // Retrieve returned data
   mBlockID = sbid;
   mSampleFormat = (sampleFormat) sqlite3_column_int(stmt, 0);
   mSumMin = sqlite3_column_double(stmt, 1);
   mSumMax = sqlite3_column_double(stmt, 2);
   mSumRms = sqlite3_column_double(stmt, 3);
   mSampleBytes = sqlite3_column_int(stmt, 4);
   mSampleCount = mSampleBytes / SAMPLE_SIZE(mSampleFormat);

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);

   mValid = true;
}

void SqliteSampleBlock::Commit(Sizes sizes)
{
   const auto mSummary256Bytes = sizes.first;
   const auto mSummary64kBytes = sizes.second;

   auto db = DB();
   int rc;

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::InsertSampleBlock,
      "INSERT INTO sampleblocks (sampleformat, summin, summax, sumrms,"
      "                          summary256, summary64k, samples)"
      "                         VALUES(?1,?2,?3,?4,?5,?6,?7);");

   // Bind statement parameters
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

      ADD_EXCEPTION_CONTEXT(
         "sqlite3.rc", std::to_string(sqlite3_errcode(Conn()->DB())));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Commit::bind");


      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }
 
   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Commit::step");

      wxLogDebug(wxT("SqliteSampleBlock::Commit - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      Conn()->ThrowException( true );
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

   wxASSERT(!IsSilent());

   // Prepare and cache statement...automatically finalized at DB close
   sqlite3_stmt *stmt = Conn()->Prepare(DBConnection::DeleteSampleBlock,
      "DELETE FROM sampleblocks WHERE blockid = ?1;");

   // Bind statement parameters
   // Might return SQLITE_MISUSE which means it's our mistake that we violated
   // preconditions; should return SQL_OK which is 0
   if (sqlite3_bind_int64(stmt, 1, mBlockID))
   {
      ADD_EXCEPTION_CONTEXT(
         "sqlite3.rc", std::to_string(sqlite3_errcode(Conn()->DB())));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Delete::bind");

      wxASSERT_MSG(false, wxT("Binding failed...bug!!!"));
   }

   // Execute the statement
   rc = sqlite3_step(stmt);
   if (rc != SQLITE_DONE)
   {
      ADD_EXCEPTION_CONTEXT("sqlite3.rc", std::to_string(rc));
      ADD_EXCEPTION_CONTEXT("sqlite3.context", "SqliteSampleBlock::Delete::step");

      wxLogDebug(wxT("SqliteSampleBlock::Load - SQLITE error %s"), sqlite3_errmsg(db));

      // Clear statement bindings and rewind statement
      sqlite3_clear_bindings(stmt);
      sqlite3_reset(stmt);

      // Just showing the user a simple message, not the library error too
      // which isn't internationalized
      Conn()->ThrowException( true );
   }

   // Clear statement bindings and rewind statement
   sqlite3_clear_bindings(stmt);
   sqlite3_reset(stmt);
}

void SqliteSampleBlock::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.WriteAttr(wxT("blockid"), mBlockID);
}

auto SqliteSampleBlock::SetSizes(
   size_t numsamples, sampleFormat srcformat ) -> Sizes
{
   mSampleFormat = srcformat;
   mSampleCount = numsamples;
   mSampleBytes = mSampleCount * SAMPLE_SIZE(mSampleFormat);

   int frames64k = (mSampleCount + 65535) / 65536;
   int frames256 = frames64k * 256;
   return { frames256 * bytesPerFrame, frames64k * bytesPerFrame };
}

/// Calculates summary block data describing this sample data.
///
/// This method also has the side effect of setting the mSumMin,
/// mSumMax, and mSumRms members of this class.
///
void SqliteSampleBlock::CalcSummary(Sizes sizes)
{
   const auto mSummary256Bytes = sizes.first;
   const auto mSummary64kBytes = sizes.second;

   Floats samplebuffer;
   float *samples;

   if (mSampleFormat == floatSample)
   {
      samples = (float *) mSamples.get();
   }
   else
   {
      samplebuffer.reinit((unsigned) mSampleCount);
      SamplesToFloats(mSamples.get(), mSampleFormat,
         samplebuffer.get(), mSampleCount);
      samples = samplebuffer.get();
   }
   
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

      summary256[i * fields] = min;
      summary256[i * fields + 1] = max;
      // The rms is correct, but this may be for less than 256 samples in last loop.
      summary256[i * fields + 2] = (float) sqrt(sumsq / jcount);
   }

   for (int i = sumLen, frames256 = mSummary256Bytes / bytesPerFrame;
        i < frames256; ++i)
   {
      // filling in the remaining bits with non-harming/contributing values
      // rms values are not "non-harming", so keep count of them:
      summaries--;
      summary256[i * fields] = FLT_MAX;        // min
      summary256[i * fields + 1] = -FLT_MAX;   // max
      summary256[i * fields + 2] = 0.0f;       // rms
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

      summary64k[i * fields] = min;
      summary64k[i * fields + 1] = max;
      summary64k[i * fields + 2] = rms;
   }

   for (int i = sumLen, frames64k = mSummary64kBytes / bytesPerFrame;
        i < frames64k; ++i)
   {
      wxASSERT_MSG(false, wxT("Out of data for mSummaryInfo"));   // Do we ever get here?

      summary64k[i * fields] = 0.0f;     // probably should be FLT_MAX, need a test case
      summary64k[i * fields + 1] = 0.0f; // probably should be -FLT_MAX, need a test case
      summary64k[i * fields + 2] = 0.0f; // just padding
   }

   // Recalc block-level summary (mRMS already calculated)
   min = summary64k[0];
   max = summary64k[1];

   for (int i = 1; i < sumLen; ++i)
   {
      if (summary64k[i * fields] < min)
      {
         min = summary64k[i * fields];
      }

      if (summary64k[i * fields + 1] > max)
      {
         max = summary64k[i * fields + 1];
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
 
