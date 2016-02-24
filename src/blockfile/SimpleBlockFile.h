/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleBlockFile.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SIMPLE_BLOCKFILE__
#define __AUDACITY_SIMPLE_BLOCKFILE__

#include <wx/string.h>
#include <wx/filename.h>

#include "../BlockFile.h"
#include "../DirManager.h"
#include "../xml/XMLWriter.h"

struct SimpleBlockFileCache {
   bool active;
   bool needWrite;
   sampleFormat format;
   samplePtr sampleData;
   void* summaryData;
};

// The AU formats we care about
enum {
   AU_SAMPLE_FORMAT_16 = 3,
   AU_SAMPLE_FORMAT_24 = 4,
   AU_SAMPLE_FORMAT_FLOAT = 6,
};

typedef struct {
   wxUint32 magic;      // magic number
   wxUint32 dataOffset; // byte offset to start of audio data
   wxUint32 dataSize;   // data length, in bytes (optional)
   wxUint32 encoding;   // data encoding enumeration
   wxUint32 sampleRate; // samples per second
   wxUint32 channels;   // number of interleaved channels
} auHeader;

class PROFILE_DLL_API SimpleBlockFile /* not final */ : public BlockFile {
 public:

   // Constructor / Destructor

   /// Create a disk file and write summary and sample data to it
   SimpleBlockFile(wxFileName baseFileName,
                   samplePtr sampleData, sampleCount sampleLen,
                   sampleFormat format,
                   bool allowDeferredWrite = false,
                   bool bypassCache = false );
   /// Create the memory structure to refer to the given block file
   SimpleBlockFile(wxFileName existingFile, sampleCount len,
                   float min, float max, float rms);

   virtual ~SimpleBlockFile();

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

   static BlockFile *BuildFromXML(DirManager &dm, const wxChar **attrs);

   bool GetNeedWriteCacheToDisk() override;
   void WriteCacheToDisk() override;

   bool GetNeedFillCache() override { return !mCache.active; }
   void FillCache() override;

 protected:

   bool WriteSimpleBlockFile(samplePtr sampleData, sampleCount sampleLen,
                             sampleFormat format, void* summaryData);
   static bool GetCache();
   void ReadIntoCache();

   SimpleBlockFileCache mCache;

   sampleFormat mFormat;
};

#endif
