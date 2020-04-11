/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleBlockFile.cpp

  Joshua Haberman
  Markus Meyer

*******************************************************************//**

\file SimpleBlockFile.cpp
\brief Implements SimpleBlockFile and auHeader.

*//****************************************************************//**

\class SimpleBlockFile
\brief A BlockFile that reads and writes uncompressed data using
libsndfile

A block file that writes the audio data to an .au file and reads
it back using libsndfile.

There are two ways to construct a simple block file.  One is to
supply data and have the constructor write the file.  The other
is for when the file already exists and we simply want to create
the data structure to refer to it.

The block file can be cached in two ways. Caching is enabled if the
preference "/Directories/CacheBlockFiles" is set, otherwise disabled. The
default is to disable caching.

* Read-caching: If caching is enabled, all block files will always be
  read-cached. Block files on disk will be read as soon as they are created
  and held in memory. New block files will be written to disk, but held in
  memory, so they are never read from disk in the current session.

* Write-caching: If caching is enabled and the parameter allowDeferredWrite
  is enabled at the block file constructor, NEW block files are held in memory
  and written to disk only when WriteCacheToDisk() is called. This is used
  during recording to prevent disk access. After recording, WriteCacheToDisk()
  will be called on all block files and they will be written to disk. During
  normal editing, no write cache is active, that is, any block files will be
  written to disk instantly.

  Even with write cache, auto recovery during normal editing will work as
  expected. However, auto recovery during recording will not work (not even
  manual auto recovery, because the files are never written physically to
  disk).

*//****************************************************************//**

\class auHeader
\brief The auHeader is a structure used by SimpleBlockFile for .au file
format.  There probably is an 'official' header file we should include
to get its definition, rather than rolling our own.

*//*******************************************************************/

#include "../Audacity.h"
#include "SimpleBlockFile.h"

#include <wx/wx.h>
#include <wx/filefn.h>
#include <wx/ffile.h>
#include <wx/utils.h>
#include <wx/log.h>

#include "../DirManager.h"
#include "../Prefs.h"

#include "../FileFormats.h"

#include "sndfile.h"


static wxUint32 SwapUintEndianess(wxUint32 in)
{
  wxUint32 out;
  unsigned char *p_in = (unsigned char *) &in;
  unsigned char *p_out = (unsigned char *) &out;
  p_out[0] = p_in[3];
  p_out[1] = p_in[2];
  p_out[2] = p_in[1];
  p_out[3] = p_in[0];
  return out;
}

/// Constructs a SimpleBlockFile based on sample data and writes
/// it to disk.
///
/// @param baseFileName The filename to use, but without an extension.
///                     This constructor will add the appropriate
///                     extension (.au in this case).
/// @param sampleData   The sample data to be written to this block.
/// @param sampleLen    The number of samples to be written to this block.
/// @param format       The format of the given samples.
/// @param allowDeferredWrite    Allow deferred write-caching
SimpleBlockFile::SimpleBlockFile(wxFileNameWrapper &&baseFileName,
                                 samplePtr sampleData, size_t sampleLen,
                                 sampleFormat format,
                                 bool allowDeferredWrite /* = false */,
                                 bool bypassCache /* = false */):
   BlockFile {
      (baseFileName.SetExt(wxT("au")), std::move(baseFileName)),
      sampleLen
   }
{
   mFormat = format;

   mCache.active = false;

   bool useCache = GetCache() && (!bypassCache);

   if (!(allowDeferredWrite && useCache) && !bypassCache)
   {
      bool bSuccess = WriteSimpleBlockFile(sampleData, sampleLen, format, NULL);
      if (!bSuccess)
         throw FileException{
            FileException::Cause::Write, GetFileName().name };
   }

   if (useCache) {
      //wxLogDebug("SimpleBlockFile::SimpleBlockFile(): Caching block file data.");
      mCache.active = true;
      mCache.needWrite = true;
      mCache.format = format;
      const auto sampleDataSize = sampleLen * SAMPLE_SIZE(format);
      mCache.sampleData.reinit(sampleDataSize);
      memcpy(mCache.sampleData.get(), sampleData, sampleDataSize);
      ArrayOf<char> cleanup;
      void* summaryData = BlockFile::CalcSummary(sampleData, sampleLen,
                                                format, cleanup);
      mCache.summaryData.reinit(mSummaryInfo.totalSummaryBytes);
      memcpy(mCache.summaryData.get(), summaryData,
             mSummaryInfo.totalSummaryBytes);
    }
}

/// Construct a SimpleBlockFile memory structure that will point to an
/// existing block file.  This file must exist and be a valid block file.
///
/// @param existingFile The disk file this SimpleBlockFile should use.
SimpleBlockFile::SimpleBlockFile(wxFileNameWrapper &&existingFile, size_t len,
                                 float min, float max, float rms):
   BlockFile{ std::move(existingFile), len }
{
   // Set an invalid format to force GetSpaceUsage() to read it from the file.
   mFormat = (sampleFormat) 0;

   mMin = min;
   mMax = max;
   mRMS = rms;

   mCache.active = false;
}

SimpleBlockFile::~SimpleBlockFile()
{
}

bool SimpleBlockFile::WriteSimpleBlockFile(
    samplePtr sampleData,
    size_t sampleLen,
    sampleFormat format,
    void* summaryData)
{
   wxFFile file(mFileName.GetFullPath(), wxT("wb"));
   if( !file.IsOpened() ){
      // Can't do anything else.
      return false;
   }

   auHeader header;

   // AU files can be either big or little endian.  Which it is is
   // determined implicitly by the byte-order of the magic 0x2e736e64
   // (.snd).  We want it to be native-endian, so we write the magic
   // to memory and then let it write that to a file in native
   // endianness
   header.magic = 0x2e736e64;

   // We store the summary data at the end of the header, so the data
   // offset is the length of the summary data plus the length of the header
   header.dataOffset = sizeof(auHeader) + mSummaryInfo.totalSummaryBytes;

   // dataSize is optional, and we opt out
   header.dataSize = 0xffffffff;

   switch(format) {
      case int16Sample:
         header.encoding = AU_SAMPLE_FORMAT_16;
         break;

      case int24Sample:
         header.encoding = AU_SAMPLE_FORMAT_24;
         break;

      case floatSample:
         header.encoding = AU_SAMPLE_FORMAT_FLOAT;
         break;
   }

   // TODO: don't fabricate
   header.sampleRate = 44100;

   // BlockFiles are always mono
   header.channels = 1;

   // Write the file
   ArrayOf<char> cleanup;
   if (!summaryData)
      summaryData = /*BlockFile::*/CalcSummary(sampleData, sampleLen, format, cleanup);
      //mchinen:allowing virtual override of calc summary for ODDecodeBlockFile.
      // PRL: cleanup fixes a possible memory leak!

   size_t nBytesToWrite = sizeof(header);
   size_t nBytesWritten = file.Write(&header, nBytesToWrite);
   if (nBytesWritten != nBytesToWrite)
   {
      wxLogDebug(wxT("Wrote %lld bytes, expected %lld."), (long long) nBytesWritten, (long long) nBytesToWrite);
      return false;
   }

   nBytesToWrite = mSummaryInfo.totalSummaryBytes;
   nBytesWritten = file.Write(summaryData, nBytesToWrite);
   if (nBytesWritten != nBytesToWrite)
   {
      wxLogDebug(wxT("Wrote %lld bytes, expected %lld."), (long long) nBytesWritten, (long long) nBytesToWrite);
      return false;
   }

   if( format == int24Sample )
   {
      // we can't write the buffer directly to disk, because 24-bit samples
      // on disk need to be packed, not padded to 32 bits like they are in
      // memory
      int *int24sampleData = (int*)sampleData;

      for( size_t i = 0; i < sampleLen; i++ )
      {
         nBytesToWrite = 3;
         nBytesWritten =
            #if wxBYTE_ORDER == wxBIG_ENDIAN
               file.Write((char*)&int24sampleData[i] + 1, nBytesToWrite);
            #else
               file.Write((char*)&int24sampleData[i], nBytesToWrite);
            #endif
         if (nBytesWritten != nBytesToWrite)
         {
            wxLogDebug(wxT("Wrote %lld bytes, expected %lld."), (long long) nBytesWritten, (long long) nBytesToWrite);
            return false;
         }
      }
   }
   else
   {
      // for all other sample formats we can write straight from the buffer
      // to disk
      nBytesToWrite = sampleLen * SAMPLE_SIZE(format);
      nBytesWritten = file.Write(sampleData, nBytesToWrite);
      if (nBytesWritten != nBytesToWrite)
      {
         wxLogDebug(wxT("Wrote %lld bytes, expected %lld."), (long long) nBytesWritten, (long long) nBytesToWrite);
         return false;
      }
   }

   return true;
}

// This function should try to fill the cache, but just return without effect
// (not throwing) if there is failure.
void SimpleBlockFile::FillCache()
{
   if (mCache.active)
      return; // cache is already filled

   // Check sample format
   wxFFile file(mFileName.GetFullPath(), wxT("rb"));
   if (!file.IsOpened())
   {
      // Don't read into cache if file not available
      return;
   }

   auHeader header;

   if (file.Read(&header, sizeof(header)) != sizeof(header))
   {
      // Corrupt file
      return;
   }

   wxUint32 encoding;

   if (header.magic == 0x2e736e64)
      encoding = header.encoding; // correct endianness
   else
      encoding = SwapUintEndianess(header.encoding);

   switch (encoding)
   {
   case AU_SAMPLE_FORMAT_16:
      mCache.format = int16Sample;
      break;
   case AU_SAMPLE_FORMAT_24:
      mCache.format = int24Sample;
      break;
   default:
      // floatSample is a safe default (we will never loose data)
      mCache.format = floatSample;
      break;
   }

   file.Close();

   // Read samples into cache
   mCache.sampleData.reinit(mLen * SAMPLE_SIZE(mCache.format));
   if (ReadData(mCache.sampleData.get(), mCache.format, 0, mLen,
                // no exceptions!
                false) != mLen)
   {
      // Could not read all samples
      mCache.sampleData.reset();
      return;
   }

   // Read summary data into cache
   // Fills with zeroes in case of failure:
   ReadSummary(mCache.summaryData);

   // Cache is active but already on disk
   mCache.active = true;
   mCache.needWrite = false;

   //wxLogDebug("SimpleBlockFile::FillCache(): Successfully read simple block file into cache.");
}

/// Read the summary section of the disk file.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool SimpleBlockFile::ReadSummary(ArrayOf<char> &data)
{
   data.reinit( mSummaryInfo.totalSummaryBytes );
   if (mCache.active) {
      //wxLogDebug("SimpleBlockFile::ReadSummary(): Summary is already in cache.");
      memcpy(data.get(), mCache.summaryData.get(), mSummaryInfo.totalSummaryBytes);
      return true;
   }
   else
   {
      //wxLogDebug("SimpleBlockFile::ReadSummary(): Reading summary from disk.");

      wxFFile file(mFileName.GetFullPath(), wxT("rb"));

      {
         Optional<wxLogNull> silence{};
         if (mSilentLog)
            silence.emplace();
         // FIXME: TRAP_ERR no report to user of absent summary files?
         // filled with zero instead.
         if (!file.IsOpened()){
            memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);
            mSilentLog = TRUE;
            return false;
         }
      }
      mSilentLog = FALSE;

      // The offset is just past the au header
      if( !file.Seek(sizeof(auHeader)) ||
          file.Read(data.get(), mSummaryInfo.totalSummaryBytes) !=
             mSummaryInfo.totalSummaryBytes ) {
         memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);
         return false;
      }

      FixSummary(data.get());

      return true;
   }
}

/// Read the data portion of the block file using libsndfile.  Convert it
/// to the given format if it is not already.
///
/// @param data   The buffer where the data will be stored
/// @param format The format the data will be stored in
/// @param start  The offset in this block file
/// @param len    The number of samples to read
size_t SimpleBlockFile::ReadData(samplePtr data, sampleFormat format,
                        size_t start, size_t len, bool mayThrow) const
{
   if (mCache.active)
   {
      //wxLogDebug("SimpleBlockFile::ReadData(): Data are already in cache.");

      auto framesRead = std::min(len, std::max(start, mLen) - start);
      CopySamples(
         (samplePtr)(mCache.sampleData.get() +
            start * SAMPLE_SIZE(mCache.format)),
         mCache.format, data, format, framesRead);

      if ( framesRead < len ) {
         if (mayThrow)
            // Not the best exception class?
            throw FileException{ FileException::Cause::Read, mFileName };
         ClearSamples(data, format, framesRead, len - framesRead);
      }

      return framesRead;
   }
   else
      return CommonReadData( mayThrow,
         mFileName, mSilentLog, nullptr, 0, 0, data, format, start, len);
}

void SimpleBlockFile::SaveXML(XMLWriter &xmlFile)
// may throw
{
   xmlFile.StartTag(wxT("simpleblockfile"));

   xmlFile.WriteAttr(wxT("filename"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("len"), mLen);
   xmlFile.WriteAttr(wxT("min"), mMin);
   xmlFile.WriteAttr(wxT("max"), mMax);
   xmlFile.WriteAttr(wxT("rms"), mRMS);

   xmlFile.EndTag(wxT("simpleblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in ProjectFSCK().
/// static
BlockFilePtr SimpleBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileNameWrapper fileName;
   float min = 0.0f, max = 0.0f, rms = 0.0f;
   size_t len = 0;
   double dblValue;
   long nValue;

   while(*attrs)
   {
      const wxChar *attr =  *attrs++;
      const wxChar *value = *attrs++;
      if (!value)
         break;

      const wxString strValue = value;
      if (!wxStricmp(attr, wxT("filename")) &&
            // Can't use XMLValueChecker::IsGoodFileName here, but do part of its test.
            XMLValueChecker::IsGoodFileString(strValue) &&
            (strValue.length() + 1 + dm.GetProjectDataDir().length() <= PLATFORM_MAX_PATH))
      {
         if (!dm.AssignFile(fileName, strValue, false))
            // Make sure fileName is back to uninitialized state so we can detect problem later.
            fileName.Clear();
      }
      else if (!wxStrcmp(attr, wxT("len")) &&
               XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue) &&
               nValue > 0)
         len = nValue;
      else if (XMLValueChecker::IsGoodString(strValue) && Internat::CompatibleToDouble(strValue, &dblValue))
      {  // double parameters
         if (!wxStricmp(attr, wxT("min")))
            min = dblValue;
         else if (!wxStricmp(attr, wxT("max")))
            max = dblValue;
         else if (!wxStricmp(attr, wxT("rms")) && (dblValue >= 0.0))
            rms = dblValue;
      }
   }

   return make_blockfile<SimpleBlockFile>
      (std::move(fileName), len, min, max, rms);
}

/// Create a copy of this BlockFile, but using a different disk file.
///
/// @param newFileName The name of the NEW file to use.
BlockFilePtr SimpleBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   auto newBlockFile = make_blockfile<SimpleBlockFile>
      (std::move(newFileName), mLen, mMin, mMax, mRMS);

   return newBlockFile;
}

auto SimpleBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   if (mCache.active && mCache.needWrite)
   {
      // We don't know space usage yet
      return 0;
   }

   // Don't know the format, so it must be read from the file
   if (mFormat == (sampleFormat) 0)
   {
      // Check sample format
      wxFFile file(mFileName.GetFullPath(), wxT("rb"));
      if (!file.IsOpened())
      {
         // Don't read into cache if file not available
         return 0;
      }
   
      auHeader header;
   
      if (file.Read(&header, sizeof(header)) != sizeof(header))
      {
         // Corrupt file
         return 0;
      }
   
      wxUint32 encoding;
   
      if (header.magic == 0x2e736e64)
         encoding = header.encoding; // correct endianness
      else
         encoding = SwapUintEndianess(header.encoding);
   
      switch (encoding)
      {
      case AU_SAMPLE_FORMAT_16:
         mFormat = int16Sample;
         break;
      case AU_SAMPLE_FORMAT_24:
         mFormat = int24Sample;
         break;
      default:
         // floatSample is a safe default (we will never loose data)
         mFormat = floatSample;
         break;
      }
   
      file.Close();
   }

   return (
          sizeof(auHeader) +
          mSummaryInfo.totalSummaryBytes +
          (GetLength() * SAMPLE_SIZE_DISK(mFormat))
   );
}

void SimpleBlockFile::Recover(){
   wxFFile file(mFileName.GetFullPath(), wxT("wb"));

   if( !file.IsOpened() ){
      // Can't do anything else.
      return;
   }

   auHeader header;
   header.magic = 0x2e736e64;
   header.dataOffset = sizeof(auHeader) + mSummaryInfo.totalSummaryBytes;

   // dataSize is optional, and we opt out
   header.dataSize = 0xffffffff;
   header.encoding = AU_SAMPLE_FORMAT_16;
   header.sampleRate = 44100;
   header.channels = 1;
   file.Write(&header, sizeof(header));

   for(decltype(mSummaryInfo.totalSummaryBytes) i = 0;
       i < mSummaryInfo.totalSummaryBytes; i++)
      file.Write(wxT("\0"),1);

   for(decltype(mLen) i = 0; i < mLen * 2; i++)
      file.Write(wxT("\0"),1);

}

void SimpleBlockFile::WriteCacheToDisk()
{
   if (!GetNeedWriteCacheToDisk())
      return;

   if (WriteSimpleBlockFile(mCache.sampleData.get(), mLen, mCache.format,
                            mCache.summaryData.get()))
      mCache.needWrite = false;
}

bool SimpleBlockFile::GetNeedWriteCacheToDisk()
{
   return mCache.active && mCache.needWrite;
}

bool SimpleBlockFile::GetCache()
{
#ifdef DEPRECATED_AUDIO_CACHE
   // See http://bugzilla.audacityteam.org/show_bug.cgi?id=545.
   bool cacheBlockFiles = false;
   gPrefs->Read(wxT("/Directories/CacheBlockFiles"), &cacheBlockFiles);
   if (!cacheBlockFiles)
      return false;

   int lowMem = gPrefs->Read(wxT("/Directories/CacheLowMem"), 16l);
   if (lowMem < 16) {
      lowMem = 16;
   }
   lowMem <<= 20;
   return (GetFreeMemory() > lowMem);
#else
   return false;
#endif
}

static DirManager::RegisteredBlockFileDeserializer sRegistration {
   "simpleblockfile",
   []( DirManager &dm, const wxChar **attrs ){
      return SimpleBlockFile::BuildFromXML( dm, attrs );
   }
};
