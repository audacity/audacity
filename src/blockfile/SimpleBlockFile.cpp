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
                                 sampleFormat format):
   BlockFile {
      (baseFileName.SetExt(wxT("au")), std::move(baseFileName)),
      sampleLen
   }
{
   mFormat = format;

   bool bSuccess = WriteSimpleBlockFile(sampleData, sampleLen, format, NULL);
   if (!bSuccess)
      throw FileException{
         FileException::Cause::Write, GetFileName().name };
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

/// Read the summary section of the disk file.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool SimpleBlockFile::ReadSummary(ArrayOf<char> &data)
{
   data.reinit( mSummaryInfo.totalSummaryBytes );
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
   // Don't know the format, so it must be read from the file
   if (mFormat == (sampleFormat) 0)
   {
      // Check sample format
      wxFFile file(mFileName.GetFullPath(), wxT("rb"));
      if (!file.IsOpened())
      {
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

static DirManager::RegisteredBlockFileDeserializer sRegistration {
   "simpleblockfile",
   []( DirManager &dm, const wxChar **attrs ){
      return SimpleBlockFile::BuildFromXML( dm, attrs );
   }
};
