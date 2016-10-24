/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include "../Audacity.h"
#include "SilentBlockFile.h"
#include "../FileFormats.h"

SilentBlockFile::SilentBlockFile(size_t sampleLen):
BlockFile{ wxFileNameWrapper{}, sampleLen }
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

size_t SilentBlockFile::ReadData(samplePtr data, sampleFormat format,
                              size_t WXUNUSED(start), size_t len) const
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
BlockFilePtr SilentBlockFile::BuildFromXML(DirManager & WXUNUSED(dm), const wxChar **attrs)
{
   long nValue;
   size_t len = 0;

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

   return make_blockfile<SilentBlockFile>(len);
}

/// Create a copy of this BlockFile
BlockFilePtr SilentBlockFile::Copy(wxFileNameWrapper &&)
{
   auto newBlockFile = make_blockfile<SilentBlockFile>(mLen);

   return newBlockFile;
}

auto SilentBlockFile::GetSpaceUsage() const -> DiskByteCount
{
   return 0;
}

