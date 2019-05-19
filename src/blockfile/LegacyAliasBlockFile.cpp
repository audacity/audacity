/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyAliasBlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"
#include "LegacyAliasBlockFile.h"

#include <wx/utils.h>
#include <wx/wxcrtvararg.h>

#include <sndfile.h>

#include "LegacyBlockFile.h"
#include "../DirManager.h"
#include "../FileFormats.h"
#include "../xml/XMLTagHandler.h"

LegacyAliasBlockFile::LegacyAliasBlockFile(wxFileNameWrapper &&fileName,
                                           wxFileNameWrapper &&aliasedFileName,
                                           sampleCount aliasStart,
                                           size_t aliasLen,
                                           int aliasChannel,
                                           size_t summaryLen,
                                           bool noRMS)
: PCMAliasBlockFile{ std::move(fileName), std::move(aliasedFileName), aliasStart, aliasLen,
                     aliasChannel, 0.0, 0.0, 0.0 }
{
   sampleFormat format;

   if (noRMS)
      format = int16Sample;
   else
      format = floatSample;

   ComputeLegacySummaryInfo(mFileName,
                            summaryLen, format,
                            &mSummaryInfo, noRMS, FALSE,
                            &mMin, &mMax, &mRMS);
}

LegacyAliasBlockFile::~LegacyAliasBlockFile()
{
}

/// Construct a NEW LegacyAliasBlockFile based on this one, but writing
/// the summary data to a NEW file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFilePtr LegacyAliasBlockFile::Copy(wxFileNameWrapper &&newFileName)
{
   auto newBlockFile = make_blockfile<LegacyAliasBlockFile>
      (std::move(newFileName), wxFileNameWrapper{ mAliasedFileName },
       mAliasStart, mLen, mAliasChannel,
       mSummaryInfo.totalSummaryBytes, mSummaryInfo.fields < 3);

   return newBlockFile;
}

void LegacyAliasBlockFile::SaveXML(XMLWriter &xmlFile)
// may throw
{
   xmlFile.StartTag(wxT("legacyblockfile"));

   xmlFile.WriteAttr(wxT("alias"), 1);
   xmlFile.WriteAttr(wxT("name"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("aliaspath"), mAliasedFileName.GetFullPath());
   xmlFile.WriteAttr(wxT("aliasstart"),
                     mAliasStart.as_long_long() );
   xmlFile.WriteAttr(wxT("aliaslen"), mLen);
   xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);
   xmlFile.WriteAttr(wxT("summarylen"), mSummaryInfo.totalSummaryBytes);
   if (mSummaryInfo.fields < 3)
      xmlFile.WriteAttr(wxT("norms"), 1);

   xmlFile.EndTag(wxT("legacyblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in ProjectFSCK().
BlockFilePtr LegacyAliasBlockFile::BuildFromXML(const FilePath &projDir, const wxChar **attrs)
{
   wxFileNameWrapper summaryFileName;
   wxFileNameWrapper aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   int summaryLen=0;
   bool noRMS = false;
   long nValue;
   long long nnValue;

   while(*attrs)
   {
      const wxChar *attr =  *attrs++;
      const wxChar *value = *attrs++;
      if (!value)
         break;

      const wxString strValue = value;
      if (!wxStricmp(attr, wxT("name")) && XMLValueChecker::IsGoodFileName(strValue, projDir))
         //v Should this be
         //    dm.AssignFile(summaryFileName, strValue, false);
         // as in PCMAliasBlockFile::BuildFromXML? Test with an old project.
         summaryFileName.Assign(projDir, strValue, wxT(""));
      else if ( !wxStricmp(attr, wxT("aliaspath")) )
      {
         if (XMLValueChecker::IsGoodPathName(strValue))
            aliasFileName.Assign(strValue);
         else if (XMLValueChecker::IsGoodFileName(strValue, projDir))
            // Allow fallback of looking for the file name, located in the data directory.
            aliasFileName.Assign(projDir, strValue);
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
         else if (!wxStricmp(attr, wxT("summarylen")) && (nValue > 0))
            summaryLen = nValue;
         else if (!wxStricmp(attr, wxT("norms")))
            noRMS = (nValue != 0);
      }
   }

   return make_blockfile<LegacyAliasBlockFile>
      (std::move(summaryFileName), std::move(aliasFileName),
       aliasStart, aliasLen, aliasChannel, summaryLen, noRMS);
}

// regenerates the summary info, doesn't deal with missing alias files
void LegacyAliasBlockFile::Recover(){
   WriteSummary();
}

static const auto sFactory = []( DirManager &dm, const wxChar **attrs ){

   // Support Audacity version 1.1.1 project files

   int i=0;
   bool alias = false;

   while(attrs[i]) {
      if (!wxStricmp(attrs[i], wxT("alias"))) {
         if (wxAtoi(attrs[i+1])==1)
            alias = true;
      }
      i++;
      if (attrs[i])
         i++;
   }

   if (alias)
      return LegacyAliasBlockFile::BuildFromXML(
         dm.GetProjectDataDir(), attrs);
   else
      return LegacyBlockFile::BuildFromXML(dm.GetProjectDataDir(), attrs,
         dm.GetLoadingBlockLength(),
         dm.GetLoadingFormat());
};

static DirManager::RegisteredBlockFileDeserializer sRegistration1 {
   "blockfile", sFactory
};
static DirManager::RegisteredBlockFileDeserializer sRegistration2 {
   "legacyblockfile", sFactory
};
