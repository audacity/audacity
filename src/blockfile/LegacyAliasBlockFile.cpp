/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyAliasBlockFile.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/utils.h>
#include <wx/wxchar.h>

#include <sndfile.h>

#include "LegacyAliasBlockFile.h"
#include "LegacyBlockFile.h"
#include "../FileFormats.h"
#include "../Internat.h"

LegacyAliasBlockFile::LegacyAliasBlockFile(wxFileName fileName,
                                           wxFileName aliasedFileName,
                                           sampleCount aliasStart,
                                           sampleCount aliasLen,
                                           int aliasChannel,
                                           sampleCount summaryLen,
                                           bool noRMS)
: PCMAliasBlockFile(fileName, aliasedFileName, aliasStart, aliasLen,
                    aliasChannel, 0.0, 0.0, 0.0)
{
   sampleFormat format;

   if (noRMS)
      format = int16Sample;
   else
      format = floatSample;

   ComputeLegacySummaryInfo(fileName,
                            summaryLen, format,
                            &mSummaryInfo, noRMS, FALSE,
                            &mMin, &mMax, &mRMS);
}

LegacyAliasBlockFile::~LegacyAliasBlockFile()
{
}

/// Construct a new LegacyAliasBlockFile based on this one, but writing
/// the summary data to a new file.
///
/// @param newFileName The filename to copy the summary data to.
BlockFile *LegacyAliasBlockFile::Copy(wxFileName newFileName)
{
   BlockFile *newBlockFile =
      new LegacyAliasBlockFile(newFileName,
                               mAliasedFileName, mAliasStart,
                               mLen, mAliasChannel,
                               mSummaryInfo.totalSummaryBytes,
                               mSummaryInfo.fields < 3);

   return newBlockFile;
}

void LegacyAliasBlockFile::SaveXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("legacyblockfile"));

   xmlFile.WriteAttr(wxT("alias"), 1);
   xmlFile.WriteAttr(wxT("name"), mFileName.GetFullName());
   xmlFile.WriteAttr(wxT("aliaspath"), mAliasedFileName.GetFullPath());
   xmlFile.WriteAttr(wxT("aliasstart"), mAliasStart);
   xmlFile.WriteAttr(wxT("aliaslen"), mLen);
   xmlFile.WriteAttr(wxT("aliaschannel"), mAliasChannel);
   xmlFile.WriteAttr(wxT("summarylen"), mSummaryInfo.totalSummaryBytes);
   if (mSummaryInfo.fields < 3)
      xmlFile.WriteAttr(wxT("norms"), 1);

   xmlFile.EndTag(wxT("legacyblockfile"));
}

// BuildFromXML methods should always return a BlockFile, not NULL,
// even if the result is flawed (e.g., refers to nonexistent file),
// as testing will be done in DirManager::ProjectFSCK().
BlockFile *LegacyAliasBlockFile::BuildFromXML(wxString projDir, const wxChar **attrs)
{
   wxFileName summaryFileName;
   wxFileName aliasFileName;
   int aliasStart=0, aliasLen=0, aliasChannel=0;
   int summaryLen=0;
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
      else if (XMLValueChecker::IsGoodInt(strValue) && strValue.ToLong(&nValue))
      {  // integer parameters
         if (!wxStricmp(attr, wxT("aliasstart")) && (nValue >= 0))
            aliasStart = nValue;
         else if (!wxStricmp(attr, wxT("aliaslen")) && (nValue >= 0))
            aliasLen = nValue;
         else if (!wxStricmp(attr, wxT("aliaschannel")) && XMLValueChecker::IsValidChannel(aliasChannel))
            aliasChannel = nValue;
         else if (!wxStricmp(attr, wxT("summarylen")) && (nValue > 0))
            summaryLen = nValue;
         else if (!wxStricmp(attr, wxT("norms")))
            noRMS = (nValue != 0);
      }
   }

   return new LegacyAliasBlockFile(summaryFileName, aliasFileName,
                                   aliasStart, aliasLen, aliasChannel,
                                   summaryLen, noRMS);
}

// regenerates the summary info, doesn't deal with missing alias files
void LegacyAliasBlockFile::Recover(){
   WriteSummary();
}
