/**********************************************************************

  Audacity: A Digital Audio Editor

  SilentBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_SILENT_BLOCKFILE__
#define __AUDACITY_SILENT_BLOCKFILE__

#include <wx/string.h>
#include <wx/filename.h>

#include "../BlockFile.h"
#include "../DirManager.h"

/// A BlockFile containing nothing but silence.  Saves disk space.
class SilentBlockFile : public BlockFile {
 public:

   // Constructor / Destructor

   SilentBlockFile(sampleCount sampleLen);

   virtual ~SilentBlockFile();

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
   virtual void Recover() { };

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
// arch-tag: 2d820790-7fce-4a08-80ae-1858f0957c11

