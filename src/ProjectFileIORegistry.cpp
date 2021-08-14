/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIORegistry.cpp

  Paul Licameli

**********************************************************************/

#include "ProjectFileIORegistry.h"

#include "Identifier.h"
#include <wx/string.h>


ProjectFileIORegistry::
ObjectReaderEntry::ObjectReaderEntry( const wxString &tag, ObjectAccessor fn )
{
   Get().mTagTable[ tag ] = move(fn);
}

XMLTagHandler *ProjectFileIORegistry::CallObjectAccessor(
   const wxString &tag, AudacityProject &project )
{
   const auto &table = mTagTable;
   if (auto iter = table.find( tag ); iter != table.end())
      if (auto &fn = iter->second)
         return fn(project);
   return nullptr;
}

ProjectFileIORegistry &ProjectFileIORegistry::Get()
{
   static ProjectFileIORegistry instance;
   return instance;
}
