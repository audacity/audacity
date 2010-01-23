/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PCMALIASBLOCKFILE__
#define __AUDACITY_PCMALIASBLOCKFILE__

#include "../BlockFile.h"
#include "../DirManager.h"

/// An AliasBlockFile that references uncompressed data in an existing file
class PCMAliasBlockFile : public AliasBlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs a PCMAliasBlockFile, writing the summary to disk
   PCMAliasBlockFile(wxFileName baseFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel);
   ///Constructs a PCMAliasBlockFile with the option of not writing to disk
   PCMAliasBlockFile(wxFileName fileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,bool writeSummary);                  
                     
   PCMAliasBlockFile(wxFileName existingFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,
                     float min, float max, float rms);
   virtual ~PCMAliasBlockFile();

   // Reading

   /// Reads the specified data from the aliased file using libsndfile
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len);
   virtual void SaveXML(XMLWriter &xmlFile);
   virtual BlockFile *Copy(wxFileName fileName);
   virtual void Recover();

   static BlockFile *BuildFromXML(DirManager &dm, const wxChar **attrs);
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
// arch-tag: a61d1bbb-662b-4abe-8995-149df0bd24f4

