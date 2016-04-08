/**********************************************************************

  Audacity: A Digital Audio Editor

  FileIO.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_FILEIO__
#define __AUDACITY_FILEIO__

#include "MemoryX.h"
#include <wx/object.h>
#include <wx/wfstream.h>

class FileIO
{
 public:
   typedef enum FileIOMode
   {
      Input,
      Output
   } FileIOMode;

 public:
   FileIO(const wxString & name, FileIOMode mode);
   ~FileIO();

   bool IsOpened();

   void Close();

   wxInputStream & Read(void *buffer, size_t size);
   wxOutputStream & Write(const void *buffer, size_t size);

 private:
   wxString mName;
   FileIOMode mMode;
   std::unique_ptr<wxInputStream> mInputStream;
   std::unique_ptr<wxOutputStream> mOutputStream;
   bool mOpen;
};

#endif
