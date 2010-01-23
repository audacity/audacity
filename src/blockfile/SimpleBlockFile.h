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

class SimpleBlockFile : public BlockFile {
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

   static BlockFile *BuildFromXML(DirManager &dm, const wxChar **attrs);
   
   virtual bool GetNeedWriteCacheToDisk();
   virtual void WriteCacheToDisk();

   virtual bool GetNeedFillCache() { return !mCache.active; }
   virtual void FillCache();

 protected:

   bool WriteSimpleBlockFile(samplePtr sampleData, sampleCount sampleLen,
                             sampleFormat format, void* summaryData);
   static bool GetCache();
   void ReadIntoCache();
   
   SimpleBlockFileCache mCache;
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
// arch-tag: 58e2a052-2894-4c0c-b35d-0a08b3c61cd7

