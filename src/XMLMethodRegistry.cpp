/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLMethodRegistry.cpp

  Paul Licameli

**********************************************************************/

#include "XMLMethodRegistry.h"

#include "Identifier.h"
#include <wx/string.h>

XMLMethodRegistryBase::XMLMethodRegistryBase() = default;
XMLMethodRegistryBase::~XMLMethodRegistryBase() = default;

void XMLMethodRegistryBase::Register(
   const wxString &tag, TypeErasedObjectAccessor accessor )
{
   mTagTable[ tag ] = move( accessor );
}

XMLTagHandler *XMLMethodRegistryBase::CallObjectAccessor(
   const wxString &tag, void *p )
{
   const auto &table = mTagTable;
   if (auto iter = table.find( tag ); iter != table.end())
      if (auto &fn = iter->second)
         return fn( p );
   return nullptr;
}

void XMLMethodRegistryBase::Register( TypeErasedWriter writer )
{
   mWriterTable.emplace_back( move( writer ) );
}

void XMLMethodRegistryBase::CallWriters( const void *p, XMLWriter &writer )
{
   const auto &table = mWriterTable;
   for ( auto &fn : table )
      if (fn)
         fn( p, writer );
}
