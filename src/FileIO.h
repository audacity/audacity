/**********************************************************************

  Audacity: A Digital Audio Editor
  
  FileIO.h
 
  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_FILEIO__
#define __AUDACITY_FILEIO__

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
   FileIO(const wxString name, FileIOMode mode);
   ~FileIO();

   bool IsOpened();
   
   void Close();

   wxInputStream & Read(void *buffer, size_t size);
   wxOutputStream & Write(const void *buffer, size_t size);

   // On the Mac, this sets the file type and creator.  It does nothing on
   // other platforms.
   void SetCatalogInfo();
   void SetCatalogInfo(wxUint32 type);

 private:
   wxString mName;
   FileIOMode mMode;
   wxInputStream *mInputStream;
   wxOutputStream *mOutputStream;
   bool mOpen;
};

#endif
