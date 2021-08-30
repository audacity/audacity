/**********************************************************************

  Audacity: A Digital Audio Editor

  FileHistory.h

  Leland Lucius

**********************************************************************/

#ifndef __AUDACITY_WIDGETS_FILEHISTORY__
#define __AUDACITY_WIDGETS_FILEHISTORY__

#include <vector>
#include <algorithm>
#include <wx/defs.h>
#include <wx/weakref.h> // member variable

#include "Identifier.h"
#include "Observer.h"
#include "wxArrayStringEx.h"

class wxConfigBase;
class wxMenu;

class AUDACITY_DLL_API FileHistory
   : public Observer::Publisher<>
{
 public:
   enum { MAX_FILES = 12 };
   FileHistory(size_t maxfiles = MAX_FILES);
   virtual ~FileHistory();
   FileHistory( const FileHistory& ) = delete;
   FileHistory &operator =( const FileHistory & ) = delete;

   static FileHistory &Global();

   void Append( const FilePath &file )
   { AddFileToHistory( file, true ); }
   void Remove( size_t i );
   void Clear();

   void Load(wxConfigBase& config, const wxString & group = wxEmptyString);
   void Save(wxConfigBase& config);

   // stl-style accessors
   using const_iterator = FilePaths::const_iterator;
   const_iterator begin() const { return mHistory.begin(); }
   const_iterator end() const { return mHistory.end(); }
   const FilePath &operator[] ( size_t ii ) const { return mHistory[ ii ]; }
   bool empty() const { return mHistory.empty(); }
   size_t size() const { return mHistory.size(); }

 private:
   void AddFileToHistory(const FilePath & file, bool update);
   void NotifyMenus();

   size_t mMaxFiles;

   FilePaths mHistory;

   wxString mGroup;
};

#endif
