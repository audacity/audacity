/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#ifdef _WIN32
   #include <windows.h>
#endif

#include <wx/file.h>
#include <wx/utils.h>
#include <wx/wxchar.h>
#include <wx/log.h>

#include <sndfile.h>

#include "PCMAliasBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

#include "../ondemand/ODManager.h"

PCMAliasBlockFile::PCMAliasBlockFile(wxFileName fileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel):
   AliasBlockFile(fileName, aliasedFile, aliasStart, aliasLen, aliasChannel)
{
   AliasBlockFile::WriteSummary();
}

PCMAliasBlockFile::PCMAliasBlockFile(wxFileName fileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,bool writeSummary):
   AliasBlockFile(fileName, aliasedFile, aliasStart, aliasLen, aliasChannel)
{
   if(writeSummary)
      AliasBlockFile::WriteSummary();
}

PCMAliasBlockFile::PCMAliasBlockFile(wxFileName existingFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,
                     float min, float max, float rms):
   AliasBlockFile(existingFileName, aliasedFile, aliasStart, aliasLen,
                  aliasChannel, min, max, rms)
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
int PCMAliasBlockFile::ReadData(samplePtr data, sampleFormat format,
                                sampleCount start, sampleCount len)
{
   SF_INFO info;

   if(!mAliasedFileName.IsOk()){ // intentionally silenced 
      memset(data,0,SAMPLE_SIZE(format)*len);
      return len;
   }

   wxLogNull *silence=0;
   if(mSilentAliasLog)silence= new wxLogNull();

   memset(&info, 0, sizeof(info));

   wxFile f;   // will be closed when it goes out of scope
   SNDFILE *sf = NULL;

   if (f.Exists(mAliasedFileName.GetFullPath())) { // Don't use Open if file does not exits
      if (f.Open(mAliasedFileName.GetFullPath())) {
         // Even though there is an sf_open() that takes a filename, use the one that
         // takes a file descriptor since wxWidgets can open a file with a Unicode name and
         // libsndfile can't (under Windows).
         ODManager::LockLibSndFileMutex();
         sf = sf_open_fd(f.fd(), SFM_READ, &info, FALSE);
         ODManager::UnlockLibSndFileMutex();
      }
   }

   if (!sf){
      memset(data,0,SAMPLE_SIZE(format)*len);
      if(silence) delete silence;
      mSilentAliasLog=TRUE;
      return len;
   }

   if(silence) delete silence;
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
   return framesRead;
}

/// Construct a new PCMAliasBlockFile based on this one, but writing
/// the summary data to a new file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFile *PCMAliasBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new PCMAliasBlockFile(newFileName,
                                                   mAliasedFileName, mAliasStart,
                                                   mLen, mAliasChannel,
                                                   mMin, mMax, mRMS);

   return newBlockFile;
}

void PCMAliasBlockFile::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("pcmaliasblockfile"));

   xmlFile.WriteAttr(wxT("summaryfile"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("aliasfile"), mAliasedFileName.GetFullPath());
   xmlFile.WriteAttr(wxT("aliasstart"), mAliasStart);
   xmlFile.WriteAttr(wxT("aliaslen"), mLen);
   xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);
   xmlFile.WriteAttr(wxT("min"), mMin);
   xmlFile.WriteAttr(wxT("max"), mMax);
   xmlFile.WriteAttr(wxT("rms"), mRMS);

   xmlFile.EndTag(wxT("pcmaliasblockfile"));
}

BlockFile *PCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   float min=0, max=0, rms=0;
   double dblValue;
   long nValue;

   while(*attrs)
   {
      const wxChar *attr =  *attrs++;
      const wxChar *value = *attrs++;
      if (!value) 
         break;

      const wxString strValue = value;
      if( !wxStricmp(attr, wxT("summaryfile")) )
      {
         // Can't use XMLValueChecker::IsGoodFileName here, but do part of its test.
         if (!XMLValueChecker::IsGoodFileString(strValue))
            return NULL;

         #ifdef _WIN32
            if (strValue.Length() + 1 + dm.GetProjectDataDir().Length() > MAX_PATH)
               return NULL;
         #endif

         dm.AssignFile(summaryFileName,value,FALSE);
      }
      else if( !wxStricmp(attr, wxT("aliasfile")) )
      {
         if (XMLValueChecker::IsGoodPathName(strValue))
            aliasFileName.Assign(strValue);
         else if (XMLValueChecker::IsGoodFileName(strValue, dm.GetProjectDataDir()))
            // Allow fallback of looking for the file name, located in the data directory.
            aliasFileName.Assign(dm.GetProjectDataDir(), strValue);
         else 
            return NULL;
      }
      else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) 
      { // integer parameters
         if( !wxStricmp(attr, wxT("aliasstart")) )
            aliasStart = nValue;
         else if( !wxStricmp(attr, wxT("aliaslen")) )
            aliasLen = nValue;
         else if( !wxStricmp(attr, wxT("aliaschannel")) )
            aliasChannel = nValue;
         else if( !wxStricmp(attr, wxT("min")) )
            min = nValue;
         else if( !wxStricmp(attr, wxT("max")) )
            max = nValue;
         else if( !wxStricmp(attr, wxT("rms")) )
            rms = nValue;
      }      
		//the min/max can be (are?) doubles as well, so handle this case:
		else if( !wxStrcmp(attr, wxT("min")) && 
               XMLValueChecker::IsGoodString(strValue) && Internat::CompatibleToDouble(strValue, &dblValue))
         min = dblValue;
      else if( !wxStrcmp(attr, wxT("max")) && 
               XMLValueChecker::IsGoodString(strValue) && Internat::CompatibleToDouble(strValue, &dblValue))
         max = dblValue;
      else if( !wxStrcmp(attr, wxT("rms")) && 
               XMLValueChecker::IsGoodString(strValue) && Internat::CompatibleToDouble(strValue, &dblValue))
         rms = dblValue;

   }

   if (!XMLValueChecker::IsGoodFileName(summaryFileName.GetFullName(), summaryFileName.GetPath(wxPATH_GET_VOLUME)) || 
         !XMLValueChecker::IsGoodFileName(aliasFileName.GetFullName(), aliasFileName.GetPath(wxPATH_GET_VOLUME)) || 
         (aliasLen <= 0) || (aliasLen < 0.0) || !XMLValueChecker::IsValidChannel(aliasChannel) || (rms < 0.0))
      return NULL;

   return new PCMAliasBlockFile(summaryFileName, aliasFileName,
                                aliasStart, aliasLen, aliasChannel,
                                min, max, rms);
}

void PCMAliasBlockFile::Recover(void)
{
   WriteSummary();
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7afeef28-696c-40c6-9558-c1134ac04a66


