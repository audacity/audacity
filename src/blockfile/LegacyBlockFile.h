/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LEGACY_BLOCKFILE__
#define __AUDACITY_LEGACY_BLOCKFILE__

#include <wx/string.h>
#include <wx/filename.h>

#include "../BlockFile.h"

void ComputeLegacySummaryInfo(wxFileName fileName,
                              int summaryLen,
                              sampleFormat format,
                              SummaryInfo *info,
                              bool noRMS,bool Silent,
                              float *min, float *max, float *rms);


//
// This class supports loading BlockFiles in one of the old
// Audacity BlockFile formats (versions 0.98 through 1.0, or
// versions 1.1.0 through 1.1.2).  You can load a BlockFile
// in this format, and you can save information about it
// back to disk, but you can't create a NEW one from new
// sample data.
//
class LegacyBlockFile final : public BlockFile {
 public:

   // Constructor / Destructor

   /// Create the memory structure to refer to the given block file
   LegacyBlockFile(wxFileName existingFile,
                   sampleFormat format,
                   sampleCount summaryLen,
                   sampleCount len,
                   bool noRMS);
   virtual ~LegacyBlockFile();

   // Reading

   /// Read the summary section of the disk file
   bool ReadSummary(void *data) override;
   /// Read the data section of the disk file
   int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len) override;

   /// Create a NEW block file identical to this one
   BlockFile *Copy(wxFileName newFileName) override;
   /// Write an XML representation of this file
   void SaveXML(XMLWriter &xmlFile) override;
   wxLongLong GetSpaceUsage() override;
   void Recover() override;

   static BlockFile *BuildFromXML(const wxString &dir, const wxChar **attrs,
                                  sampleCount len,
                                  sampleFormat format);

 protected:
   sampleFormat mFormat;
};

#endif // __AUDACITY_LEGACY_BLOCKFILE__
