/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyAliasBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LEGACYALIASBLOCKFILE__
#define __AUDACITY_LEGACYALIASBLOCKFILE__

#include "../BlockFile.h"
#include "PCMAliasBlockFile.h"

/// An AliasBlockFile that references uncompressed data in an existing file
class LegacyAliasBlockFile : public PCMAliasBlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs a LegacyAliasBlockFile, writing the summary to disk
   LegacyAliasBlockFile(wxFileName fileName,
                        wxFileName aliasedFile,
                        sampleCount aliasStart,
                        sampleCount aliasLen,
                        int aliasChannel,
                        sampleCount summaryLen,
                        bool noRMS);
   virtual ~LegacyAliasBlockFile();

   virtual void SaveXML(XMLWriter &xmlFile);
   virtual BlockFile *Copy(wxFileName fileName);
   virtual void Recover();

   static BlockFile *BuildFromXML(wxString projDir, const wxChar **attrs);
};

#endif


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: dde00f06-1208-4d92-9c39-e263b13c66ba

