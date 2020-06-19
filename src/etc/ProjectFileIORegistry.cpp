/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIORegistry.cpp

  Paul Licameli

**********************************************************************/

#include "ProjectFileIORegistry.h"

#include "audacity/Types.h"
#include <unordered_map>
#include <wx/string.h>


namespace ProjectFileIORegistry {

namespace {
   using TagTable = std::unordered_map< wxString, TagHandlerFactory >;
   static TagTable &sTagTable()
   {
      static TagTable theTable;
      return theTable;
   }
}

Entry::Entry(
   const wxString &tag, const TagHandlerFactory &factory )
{
   sTagTable()[ tag ] = factory;
}

TagHandlerFactory Lookup( const wxString &tag )
{
   const auto &table = sTagTable();
   auto iter = table.find( tag );
   if ( iter == table.end() )
      return {};
   return iter->second;
}

}
