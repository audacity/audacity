/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.cpp

  Joshua Haberman

**********************************************************************/

#include "../Audacity.h"
#include "SilentBlockFile.h"

#include "../FileFormats.h"
#include "../DirManager.h"
#include "../xml/XMLTagHandler.h"

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

bool SilentBlockFile::ReadSummary(ArrayOf<char> &data)
{
   data.reinit( mSummaryInfo.totalSummaryBytes );
   memset(data.get(), 0, mSummaryInfo.totalSummaryBytes);
   return true;
}

size_t SilentBlockFile::ReadData(samplePtr data, sampleFormat format,
                              size_t WXUNUSED(start), size_t len, bool) const
{
   ClearSamples(data, format, 0, len);

   return len;
}

void SilentBlockFile::SaveXML(XMLWriter &xmlFile)
// may throw
{
   xmlFile.StartTag(wxT("silentblockfile"));

   xmlFile.WriteAttr(wxT("len"), mLen);

   xmlFile.EndTag(wxT("silentblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in ProjectFSCK().
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

static DirManager::RegisteredBlockFileDeserializer sRegistration {
   "silentblockfile",
   []( DirManager &dm, const wxChar **attrs ){
      return SilentBlockFile::BuildFromXML( dm, attrs );
   }
};
