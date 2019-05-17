/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   SilentBlockFile.h

   Dominic Mazzoni
   Vaughan Johnson

**********************************************************************/

#ifndef __AUDACITY_SILENT_BLOCKFILE__
#define __AUDACITY_SILENT_BLOCKFILE__

#include "../BlockFile.h"

class DirManager;

/// A BlockFile containing nothing but silence.  Saves disk space.
class SilentBlockFile final : public BlockFile {
 public:

   // Constructor / Destructor

   SilentBlockFile(size_t sampleLen);

   virtual ~SilentBlockFile();

   // Reading

   /// Read the summary section of the disk file
   bool ReadSummary(ArrayOf<char> &data) override;
   /// Read the data section of the disk file
   size_t ReadData(samplePtr data, sampleFormat format,
                        size_t start, size_t len, bool mayThrow) const override;

   /// Create a NEW block file identical to this one
   BlockFilePtr Copy(wxFileNameWrapper &&newFileName) override;
   /// Write an XML representation of this file
   void SaveXML(XMLWriter &xmlFile) override;
   DiskByteCount GetSpaceUsage() const override;
   void Recover() override { };

   static BlockFilePtr BuildFromXML(DirManager &dm, const wxChar **attrs);
};

#endif

