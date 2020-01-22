/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyBlockFile.cpp

  Dominic Mazzoni

******************************************************************//**

\class LegacyBlockFile
\brief Audacity 1.1.0 block file format:

  - Header tag: 20 bytes "AudacityBlockFile110"
  - 64K summaries (min, max, RMS, each a 4-byte float)
  - 256 summaries (min, max, RMS, each a 4-byte float)

*//******************************************************************/


#include "../Audacity.h"
#include "LegacyBlockFile.h"

#include <float.h>
#include <math.h>

#include <wx/filefn.h>
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/utils.h>
#include <wx/log.h>

#include "../FileFormats.h"
#include "../xml/XMLTagHandler.h"

#include "sndfile.h"

void ComputeLegacySummaryInfo(const wxFileName &fileName,
                              size_t summaryLen,
                              sampleFormat format,
                              SummaryInfo *info,
                              bool noRMS,bool Silent,
                              float *min, float *max, float *rms)
{
   int fields = 3; /* min, max, rms */

   if (noRMS)
      fields = 2;

   info->fields = fields;
   info->format = format;
   info->bytesPerFrame =
      SAMPLE_SIZE(info->format) * fields;
   info->totalSummaryBytes = summaryLen;
   info->offset64K = 20; /* legacy header tag len */
   info->frames64K = (summaryLen-20) /
      (info->bytesPerFrame * 256);
   info->offset256 = info->offset64K +
      (info->frames64K * info->bytesPerFrame);
   info->frames256 =
      (summaryLen - 20 -
       (info->frames64K * info->bytesPerFrame)) /
      info->bytesPerFrame;

   //
   // Compute the min, max, and RMS of the block from the
   // 64K summary data
   //

   Floats summary{ info->frames64K * fields };
   SampleBuffer data(info->frames64K * fields,
      info->format);

   int read;
   {
      Optional<wxLogNull> silence{};
      const wxString fullPath{ fileName.GetFullPath() };
      wxFFile summaryFile(fullPath, wxT("rb"));
      if (Silent)
         silence.emplace();

      // FIXME: TRAP_ERR no report to user of absent summary files.
      if (!summaryFile.IsOpened()) {
         wxLogWarning(wxT("Unable to access summary file %s; substituting silence for remainder of session"),
            fullPath);

         read = info->frames64K * info->bytesPerFrame;
         memset(data.ptr(), 0, read);
      }
      else{
         // FIXME: TRAP_ERR Seek in summary file could fail.
         summaryFile.Seek(info->offset64K);
         read = summaryFile.Read(data.ptr(),
            info->frames64K *
            info->bytesPerFrame);
      }
   }

   int count = read / info->bytesPerFrame;

   CopySamples(data.ptr(), info->format,
               (samplePtr)summary.get(), floatSample, count);

   (*min) = FLT_MAX;
   (*max) = FLT_MIN;
   float sumsq = 0;

   for(int i=0; i<count; i++) {
      if (summary[fields*i] < (*min))
         (*min) = summary[fields*i];
      if (summary[fields*i+1] > (*max))
         (*max) = summary[fields*i+1];
      if (fields >= 3)
         sumsq += summary[fields*i+2]*summary[fields*i+2];
   }
   if (fields >= 3)
      (*rms) = sqrt(sumsq / count);
   else
      (*rms) = 0;
}

/// Construct a LegacyBlockFile memory structure that will point to an
/// existing block file.  This file must exist and be a valid block file.
///
/// @param existingFile The disk file this LegacyBlockFile should use.
LegacyBlockFile::LegacyBlockFile(wxFileNameWrapper &&existingFile,
                                 sampleFormat format,
                                 size_t summaryLen,
                                 size_t len,
                                 bool noRMS):
   BlockFile{ std::move(existingFile), len },
   mFormat(format)
{

   sampleFormat summaryFormat;

   if (noRMS)
      summaryFormat = int16Sample;
   else
      summaryFormat = floatSample;

   ComputeLegacySummaryInfo(mFileName,
                            summaryLen, summaryFormat,
                            &mSummaryInfo, noRMS, FALSE,
                            &mMin, &mMax, &mRMS);
}

LegacyBlockFile::~LegacyBlockFile()
{
}

/// Read the summary section of the disk file.
/// Fill with zeroes and return false if data are unavailable for any reason.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool LegacyBlockFile::ReadSummary(ArrayOf<char> &data)
{
   data.reinit( mSummaryInfo.totalSummaryBytes );
   wxFFile summaryFile(mFileName.GetFullPath(), wxT("rb"));
   size_t read;
   {
      Optional<wxLogNull> silence{};
      if (mSilentLog)
         silence.emplace();

      if (!summaryFile.IsOpened()) {

         memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);

         mSilentLog = TRUE;

         return false;
      }

      read = summaryFile.Read(data.get(), mSummaryInfo.totalSummaryBytes);
   }
   mSilentLog = FALSE;

   if (read != mSummaryInfo.totalSummaryBytes) {
      memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);
      return false;
   }

   return true;
}

/// Read the data portion of the block file using libsndfile.  Convert it
/// to the given format if it is not already.
///
/// @param data   The buffer where the data will be stored
/// @param format The format the data will be stored in
/// @param start  The offset in this block file
/// @param len    The number of samples to read
size_t LegacyBlockFile::ReadData(samplePtr data, sampleFormat format,
                              size_t start, size_t len, bool mayThrow) const
{
   sf_count_t origin = (mSummaryInfo.totalSummaryBytes / SAMPLE_SIZE(mFormat));
   return CommonReadData( mayThrow,
      mFileName, mSilentLog, nullptr, origin, 0, data, format, start, len,
      &mFormat, mLen
   );
}

void LegacyBlockFile::SaveXML(XMLWriter &xmlFile)
// may throw
{
   xmlFile.StartTag(wxT("legacyblockfile"));

   xmlFile.WriteAttr(wxT("name"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("len"), mLen);
   if (mSummaryInfo.fields < 3)
      xmlFile.WriteAttr(wxT("norms"), 1);
   xmlFile.WriteAttr(wxT("summarylen"), mSummaryInfo.totalSummaryBytes);

   xmlFile.EndTag(wxT("legacyblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in ProjectFSCK().
/// static
BlockFilePtr LegacyBlockFile::BuildFromXML(const FilePath &projDir, const wxChar **attrs,
                                         size_t len, sampleFormat format)
{
   wxFileNameWrapper fileName;
   size_t summaryLen = 0;
   bool noRMS = false;
   long nValue;

   while(*attrs)
   {
      const wxChar *attr =  *attrs++;
      const wxChar *value = *attrs++;
      if (!value)
         break;

      const wxString strValue = value;
      if (!wxStricmp(attr, wxT("name")) && XMLValueChecker::IsGoodFileName(strValue, projDir))
         //v Should this be
         //    dm.AssignFile(fileName, strValue, false);
         // as in PCMAliasBlockFile::BuildFromXML? Test with an old project.
         fileName.Assign(projDir, strValue);
      else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
      {  // integer parameters
         if (!wxStrcmp(attr, wxT("len")) && (nValue >= 0))
            len = nValue;
         else if (!wxStrcmp(attr, wxT("norms")))
            noRMS = (nValue != 0);
         else if (!wxStrcmp(attr, wxT("format")) && XMLValueChecker::IsValidSampleFormat(nValue))
            format = (sampleFormat)nValue;
         else if (!wxStrcmp(attr, wxT("summarylen")) && (nValue > 0))
            // Note attribute "summarylen" was written as int, no need for 64 bits
            summaryLen = nValue;
      }
   }

   return make_blockfile<LegacyBlockFile>
      (std::move(fileName), format, summaryLen, len, noRMS);
}

/// Create a copy of this BlockFile, but using a different disk file.
///
/// @param newFileName The name of the NEW file to use.
BlockFilePtr LegacyBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   return make_blockfile<LegacyBlockFile>
       (std::move(newFileName),
        mFormat, mSummaryInfo.totalSummaryBytes,
        mLen, mSummaryInfo.fields < 3);
}

auto LegacyBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   wxFFile dataFile(mFileName.GetFullPath());
   return dataFile.Length();
}

void LegacyBlockFile::Recover()
{


}
