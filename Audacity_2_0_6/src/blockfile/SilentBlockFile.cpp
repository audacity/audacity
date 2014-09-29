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
                              sampleCount WXUNUSED(start), sampleCount len)
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

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in DirManager::ProjectFSCK().
/// static
BlockFile *SilentBlockFile::BuildFromXML(DirManager & WXUNUSED(dm), const wxChar **attrs)
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
       if (!wxStrcmp(attr, wxT("len")) &&
            XMLValueChecker::IsGoodInt(strValue) &&
            strValue.ToLong(&nValue) &&
            nValue > 0)
         len = nValue;
   }

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

