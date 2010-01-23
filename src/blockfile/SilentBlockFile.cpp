/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include "SilentBlockFile.h"
#include "../FileFormats.h"

SilentBlockFile::SilentBlockFile(sampleCount sampleLen):
   BlockFile(wxFileName(), sampleLen)
{
   mMin = 0.;
   mMax = 0.;
   mRMS = 0.;
}

SilentBlockFile::~SilentBlockFile()
{
}

bool SilentBlockFile::ReadSummary(void *data)
{
   memset(data, 0, (size_t)mSummaryInfo.totalSummaryBytes);
   return true;
}

int SilentBlockFile::ReadData(samplePtr data, sampleFormat format,
                              sampleCount start, sampleCount len)
{
   ClearSamples(data, format, 0, len);

   return len;
}

void SilentBlockFile::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("silentblockfile"));

   xmlFile.WriteAttr(wxT("len"), mLen);

   xmlFile.EndTag(wxT("silentblockfile"));
}

/// static
BlockFile *SilentBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs)
{
   long nValue;
   sampleCount len = 0;

   while(*attrs)
   {
       const wxChar *attr =  *attrs++;
       const wxChar *value = *attrs++;

       if (!value)
         break;

       const wxString strValue = value;
       if( !wxStrcmp(attr, wxT("len")) && 
            XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue)) 
          len = nValue;
   }

   if (len <= 0)
      return NULL;

   return new SilentBlockFile(len);
}

/// Create a copy of this BlockFile
BlockFile *SilentBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile = new SilentBlockFile(mLen);

   return newBlockFile;
}

wxLongLong SilentBlockFile::GetSpaceUsage()
{
   return 0;
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
// arch-tag: d7d174aa-19f1-4bc8-8bd9-c075dcd1cc1b

