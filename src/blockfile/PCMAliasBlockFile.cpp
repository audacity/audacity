/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include "../Audacity.h"
#include "PCMAliasBlockFile.h"

#include <wx/file.h>
#include <wx/utils.h>
#include <wx/wxchar.h>
#include <wx/log.h>

#include <sndfile.h>

#include "../FileFormats.h"
#include "../Internat.h"
#include "../MemoryX.h"

#include "../ondemand/ODManager.h"
#include "../AudioIO.h"

extern AudioIO *gAudioIO;

PCMAliasBlockFile::PCMAliasBlockFile(
      wxFileNameWrapper &&fileName,
      wxFileNameWrapper &&aliasedFileName,
      sampleCount aliasStart,
      size_t aliasLen, int aliasChannel)
: AliasBlockFile{ std::move(fileName), std::move(aliasedFileName),
                  aliasStart, aliasLen, aliasChannel }
{
   AliasBlockFile::WriteSummary();
}

PCMAliasBlockFile::PCMAliasBlockFile(
      wxFileNameWrapper&& fileName,
      wxFileNameWrapper&& aliasedFileName,
      sampleCount aliasStart,
      size_t aliasLen, int aliasChannel,bool writeSummary)
: AliasBlockFile{ std::move(fileName), std::move(aliasedFileName),
                  aliasStart, aliasLen, aliasChannel }
{
   if(writeSummary)
      AliasBlockFile::WriteSummary();
}

PCMAliasBlockFile::PCMAliasBlockFile(
      wxFileNameWrapper &&existingSummaryFileName,
      wxFileNameWrapper &&aliasedFileName,
      sampleCount aliasStart,
      size_t aliasLen, int aliasChannel,
      float min, float max, float rms)
: AliasBlockFile{ std::move(existingSummaryFileName), std::move(aliasedFileName),
                  aliasStart, aliasLen,
                  aliasChannel, min, max, rms }
{
}

PCMAliasBlockFile::~PCMAliasBlockFile()
{
}

/// Reads the specified data from the aliased file, using libsndfile,
/// and converts it to the given sample format.
///
/// @param data   The buffer to read the sample data into.
/// @param format The format to convert the data into
/// @param start  The offset within the block to begin reading
/// @param len    The number of samples to read
size_t PCMAliasBlockFile::ReadData(samplePtr data, sampleFormat format,
                                size_t start, size_t len) const
{
   SF_INFO info;

   if(!mAliasedFileName.IsOk()){ // intentionally silenced
      memset(data,0,SAMPLE_SIZE(format)*len);
      return len;
   }

   wxFile f;   // will be closed when it goes out of scope
   SFFile sf;
   {
      Maybe<wxLogNull> silence{};
      if (mSilentAliasLog)
         silence.create();

      memset(&info, 0, sizeof(info));

      if (f.Exists(mAliasedFileName.GetFullPath())) { // Don't use Open if file does not exits
         if (f.Open(mAliasedFileName.GetFullPath())) {
            // Even though there is an sf_open() that takes a filename, use the one that
            // takes a file descriptor since wxWidgets can open a file with a Unicode name and
            // libsndfile can't (under Windows).
            sf.reset(SFCall<SNDFILE*>(sf_open_fd, f.fd(), SFM_READ, &info, FALSE));
         }
         // FIXME: TRAP_ERR failure of wxFile open incompletely handled in PCMAliasBlockFile::ReadData.

      }

      if (!sf) {
         memset(data, 0, SAMPLE_SIZE(format)*len);
         silence.reset();
         mSilentAliasLog = TRUE;

         // Set a marker to display an error message for the silence
         if (!wxGetApp().ShouldShowMissingAliasedFileWarning())
            wxGetApp().MarkAliasedFilesMissingWarning(this);
         return len;
      }
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

   return framesRead;
}

/// Construct a NEW PCMAliasBlockFile based on this one, but writing
/// the summary data to a NEW file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFilePtr PCMAliasBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   auto newBlockFile = make_blockfile<PCMAliasBlockFile>
      (std::move(newFileName), wxFileNameWrapper{mAliasedFileName},
       mAliasStart, mLen, mAliasChannel, mMin, mMax, mRMS);

   return newBlockFile;
}

void PCMAliasBlockFile::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("pcmaliasblockfile"));

   xmlFile.WriteAttr(wxT("summaryfile"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("aliasfile"), mAliasedFileName.GetFullPath());
   xmlFile.WriteAttr(wxT("aliasstart"),
                     mAliasStart.as_long_long());
   xmlFile.WriteAttr(wxT("aliaslen"), mLen);
   xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);
   xmlFile.WriteAttr(wxT("min"), mMin);
   xmlFile.WriteAttr(wxT("max"), mMax);
   xmlFile.WriteAttr(wxT("rms"), mRMS);

   xmlFile.EndTag(wxT("pcmaliasblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in DirManager::ProjectFSCK().
BlockFilePtr PCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileNameWrapper summaryFileName;
   wxFileNameWrapper aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   float min = 0.0f, max = 0.0f, rms = 0.0f;
   double dblValue;
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
      else if (!wxStricmp(attr, wxT("aliasfile")))
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
         else if (!wxStricmp(attr, wxT("min")))
            min = nValue;
         else if (!wxStricmp(attr, wxT("max")))
            max = nValue;
         else if (!wxStricmp(attr, wxT("rms")) && (nValue >= 0))
            rms = nValue;
      }
      // mchinen: the min/max can be (are?) doubles as well, so handle those cases.
      // Vaughan: The code to which I added the XMLValueChecker checks
      // used wxAtoi to convert the string to an int.
      // So it's possible some prior project formats used ints (?), so am keeping
      // those above, but yes, we need to handle floats.
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

   return make_blockfile<PCMAliasBlockFile>
      (std::move(summaryFileName), std::move(aliasFileName),
       aliasStart, aliasLen, aliasChannel, min, max, rms);
}

void PCMAliasBlockFile::Recover(void)
{
   WriteSummary();
}

