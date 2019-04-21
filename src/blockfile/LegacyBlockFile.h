/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LEGACY_BLOCKFILE__
#define __AUDACITY_LEGACY_BLOCKFILE__

#include "../BlockFile.h"

class wxFileName;

void ComputeLegacySummaryInfo(const wxFileName &fileName,
                              size_t summaryLen,
                              sampleFormat format,
                              SummaryInfo *info,
                              bool noRMS,bool Silent,
                              float *min, float *max, float *rms);


//
// This class supports loading BlockFiles in one of the old
// Audacity BlockFile formats (versions 0.98 through 1.0, or
// versions 1.1.0 through 1.1.2).  You can load a BlockFile
// in this format, and you can save information about it
// back to disk, but you can't create a NEW one from NEW
// sample data.
//
class LegacyBlockFile final : public BlockFile {
 public:

   // Constructor / Destructor

   /// Create the memory structure to refer to the given block file
   LegacyBlockFile(wxFileNameWrapper &&existingFile,
                   sampleFormat format,
                   size_t summaryLen,
                   size_t len,
                   bool noRMS);
   virtual ~LegacyBlockFile();

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
   void Recover() override;

   static BlockFilePtr BuildFromXML(const FilePath &dir, const wxChar **attrs,
                                  size_t len,
                                  sampleFormat format);

 protected:
   sampleFormat mFormat;
};

#endif // __AUDACITY_LEGACY_BLOCKFILE__
