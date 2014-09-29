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


#include <float.h>
#include <math.h>

#include <wx/filefn.h>
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/utils.h>
#include <wx/log.h>

#include "LegacyBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

#include "sndfile.h"

void ComputeLegacySummaryInfo(wxFileName fileName,
                              int summaryLen,
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

   float *summary = new float[info->frames64K * fields];
   samplePtr data = NewSamples(info->frames64K * fields,
                               info->format);

   wxLogNull *silence=0;
   wxFFile summaryFile(fileName.GetFullPath(), wxT("rb"));
   int read;
   if(Silent)silence= new wxLogNull();

   if( !summaryFile.IsOpened() ) {
      wxLogWarning(wxT("Unable to access summary file %s; substituting silence for remainder of session"),
                   fileName.GetFullPath().c_str());

      read=info->frames64K * info->bytesPerFrame;
      memset(data,0,read);
   }else{
      summaryFile.Seek(info->offset64K);
      read = summaryFile.Read(data,
                              info->frames64K *
                              info->bytesPerFrame);
   }

   if(silence) delete silence;

   int count = read / info->bytesPerFrame;

   CopySamples(data, info->format,
               (samplePtr)summary, floatSample, count);

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

   DeleteSamples(data);
   delete[] summary;
}

/// Construct a LegacyBlockFile memory structure that will point to an
/// existing block file.  This file must exist and be a valid block file.
///
/// @param existingFile The disk file this LegacyBlockFile should use.
LegacyBlockFile::LegacyBlockFile(wxFileName existingFile,
                                 sampleFormat format,
                                 sampleCount summaryLen,
                                 sampleCount len,
                                 bool noRMS):
   BlockFile(existingFile, len),
   mFormat(format)
{

   sampleFormat summaryFormat;

   if (noRMS)
      summaryFormat = int16Sample;
   else
      summaryFormat = floatSample;

   ComputeLegacySummaryInfo(existingFile,
                            summaryLen, summaryFormat,
                            &mSummaryInfo, noRMS, FALSE,
                            &mMin, &mMax, &mRMS);
}

LegacyBlockFile::~LegacyBlockFile()
{
}

/// Read the summary section of the disk file.
///
/// @param *data The buffer to write the data to.  It must be at least
/// mSummaryinfo.totalSummaryBytes long.
bool LegacyBlockFile::ReadSummary(void *data)
{
   wxFFile summaryFile(mFileName.GetFullPath(), wxT("rb"));
   wxLogNull *silence=0;
   if(mSilentLog)silence= new wxLogNull();

   if( !summaryFile.IsOpened() ){

      memset(data,0,(size_t)mSummaryInfo.totalSummaryBytes);

      if(silence) delete silence;
      mSilentLog=TRUE;

      return true;
   }

   int read = summaryFile.Read(data, (size_t)mSummaryInfo.totalSummaryBytes);

   if(silence) delete silence;
   mSilentLog=FALSE;

   return (read == mSummaryInfo.totalSummaryBytes);
}

/// Read the data portion of the block file using libsndfile.  Convert it
/// to the given format if it is not already.
///
/// @param data   The buffer where the data will be stored
/// @param format The format the data will be stored in
/// @param start  The offset in this block file
/// @param len    The number of samples to read
int LegacyBlockFile::ReadData(samplePtr data, sampleFormat format,
                              sampleCount start, sampleCount len)
{
   SF_INFO info;

   memset(&info, 0, sizeof(info));

   switch(mFormat) {
   case int16Sample:
      info.format =
         SF_FORMAT_RAW | SF_FORMAT_PCM_16 | SF_ENDIAN_CPU;
      break;
   default:
   case floatSample:
      info.format =
         SF_FORMAT_RAW | SF_FORMAT_FLOAT | SF_ENDIAN_CPU;
      break;
   case int24Sample:
      info.format = SF_FORMAT_RAW | SF_FORMAT_PCM_32 | SF_ENDIAN_CPU;
      break;
   }
   info.samplerate = 44100; // Doesn't matter
   info.channels = 1;
   info.frames = mLen + (mSummaryInfo.totalSummaryBytes /
                         SAMPLE_SIZE(mFormat));

   wxFile f;   // will be closed when it goes out of scope
   SNDFILE *sf = NULL;

   if (f.Open(mFileName.GetFullPath())) {
      // Even though there is an sf_open() that takes a filename, use the one that
      // takes a file descriptor since wxWidgets can open a file with a Unicode name and
      // libsndfile can't (under Windows).
      sf = sf_open_fd(f.fd(), SFM_READ, &info, FALSE);
   }

   wxLogNull *silence=0;
   if(mSilentLog)silence= new wxLogNull();

   if (!sf){

      memset(data,0,SAMPLE_SIZE(format)*len);

      if(silence) delete silence;
      mSilentLog=TRUE;

      return len;
   }
   if(silence) delete silence;
   mSilentLog=FALSE;

   sf_count_t seekstart = start +
         (mSummaryInfo.totalSummaryBytes / SAMPLE_SIZE(mFormat));
   sf_seek(sf, seekstart , SEEK_SET);

   samplePtr buffer = NewSamples(len, floatSample);
   int framesRead = 0;

   // If both the src and dest formats are integer formats,
   // read integers from the file (otherwise we would be
   // converting to float and back, which is unneccesary)
   if (format == int16Sample &&
       sf_subtype_is_integer(info.format)) {
      framesRead = sf_readf_short(sf, (short *)data, len);
   }else if (format == int24Sample &&
             sf_subtype_is_integer(info.format)) {
      framesRead = sf_readf_int(sf, (int *)data, len);

         // libsndfile gave us the 3 byte sample in the 3 most
      // significant bytes -- we want it in the 3 least
      // significant bytes.
      int *intPtr = (int *)data;
      for( int i = 0; i < framesRead; i++ )
         intPtr[i] = intPtr[i] >> 8;
   } else {
      // Otherwise, let libsndfile handle the conversion and
      // scaling, and pass us normalized data as floats.  We can
      // then convert to whatever format we want.
      framesRead = sf_readf_float(sf, (float *)buffer, len);
      CopySamples(buffer, floatSample,
                  (samplePtr)data, format, framesRead);
   }

   sf_close(sf);

   DeleteSamples(buffer);

   return framesRead;
}

void LegacyBlockFile::SaveXML(XMLWriter &xmlFile)
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
// as testing will be done in DirManager::ProjectFSCK().
/// static
BlockFile *LegacyBlockFile::BuildFromXML(wxString projDir, const wxChar **attrs,
                                         sampleCount len, sampleFormat format)
{
   wxFileName fileName;
   sampleCount summaryLen = 0;
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
            summaryLen = nValue;
      }
   }

   return new LegacyBlockFile(fileName, format, summaryLen, len, noRMS);
}

/// Create a copy of this BlockFile, but using a different disk file.
///
/// @param newFileName The name of the new file to use.
BlockFile *LegacyBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new LegacyBlockFile(newFileName,
                                                 mFormat,
                                                 mSummaryInfo.totalSummaryBytes,
                                                 mLen,
                                                 mSummaryInfo.fields < 3);

   return newBlockFile;
}

wxLongLong LegacyBlockFile::GetSpaceUsage()
{
   wxFFile dataFile(mFileName.GetFullPath());
   return dataFile.Length();
}

void LegacyBlockFile::Recover()
{


}
