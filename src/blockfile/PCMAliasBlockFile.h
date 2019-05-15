/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PCMALIASBLOCKFILE__
#define __AUDACITY_PCMALIASBLOCKFILE__

#include "../BlockFile.h"

class DirManager;

/// An AliasBlockFile that references uncompressed data in an existing file
class PCMAliasBlockFile /* not final */ : public AliasBlockFile
{
 public:
   /// Constructs a PCMAliasBlockFile, writing the summary to disk
   PCMAliasBlockFile(wxFileNameWrapper &&baseFileName,
                     wxFileNameWrapper &&aliasedFileName,
                     sampleCount aliasStart,
                     size_t aliasLen, int aliasChannel);
   ///Constructs a PCMAliasBlockFile with the option of not writing to disk
   PCMAliasBlockFile(wxFileNameWrapper &&fileName,
                     wxFileNameWrapper &&aliasedFileName,
                     sampleCount aliasStart,
                     size_t aliasLen, int aliasChannel,bool writeSummary);

   PCMAliasBlockFile(wxFileNameWrapper &&existingSummaryFileName,
                     wxFileNameWrapper &&aliasedFileName,
                     sampleCount aliasStart,
                     size_t aliasLen, int aliasChannel,
                     float min, float max, float rms);
   virtual ~PCMAliasBlockFile();

   /// Reads the specified data from the aliased file using libsndfile
   size_t ReadData(samplePtr data, sampleFormat format,
                        size_t start, size_t len, bool mayThrow) const override;

   void SaveXML(XMLWriter &xmlFile) override;
   BlockFilePtr Copy(wxFileNameWrapper &&fileName) override;
   void Recover() override;

   static BlockFilePtr BuildFromXML(DirManager &dm, const wxChar **attrs);
};

#endif

