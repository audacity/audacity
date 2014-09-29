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
// back to disk, but you can't create a new one from new
// sample data.
//
class LegacyBlockFile : public BlockFile {
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
   virtual bool ReadSummary(void *data);
   /// Read the data section of the disk file
   virtual int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len);

   /// Create a new block file identical to this one
   virtual BlockFile *Copy(wxFileName newFileName);
   /// Write an XML representation of this file
   virtual void SaveXML(XMLWriter &xmlFile);
   virtual wxLongLong GetSpaceUsage();
   virtual void Recover();

   static BlockFile *BuildFromXML(wxString dir, const wxChar **attrs,
                                  sampleCount len,
                                  sampleFormat format);

 protected:
   sampleFormat mFormat;
};

#endif // __AUDACITY_LEGACY_BLOCKFILE__
