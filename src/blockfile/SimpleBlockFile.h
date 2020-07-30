/**********************************************************************

  Audacity: A Digital Audio Editor

  SimpleBlockFile.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_SIMPLE_BLOCKFILE__
#define __AUDACITY_SIMPLE_BLOCKFILE__

#include "../BlockFile.h"

class DirManager;

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
   SimpleBlockFile(wxFileNameWrapper &&baseFileName,
                   samplePtr sampleData, size_t sampleLen,
                   sampleFormat format);
   /// Create the memory structure to refer to the given block file
   SimpleBlockFile(wxFileNameWrapper &&existingFile, size_t len,
                   float min, float max, float rms);

   virtual ~SimpleBlockFile();

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

   static BlockFilePtr BuildFromXML(DirManager &dm, const wxChar **attrs);

 protected:

   bool WriteSimpleBlockFile(samplePtr sampleData, size_t sampleLen,
                             sampleFormat format, void* summaryData);

 private:
   mutable sampleFormat mFormat; // may be found lazily
};

#endif
