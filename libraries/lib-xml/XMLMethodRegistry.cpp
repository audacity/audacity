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

void XMLMethodRegistryBase::PushAccessor( TypeErasedAccessor accessor )
{
   mAccessors.emplace_back( move( accessor ) );
}

void XMLMethodRegistryBase::Register(
   const wxString &tag, TypeErasedMutator mutator )
{
   mMutatorTable[ tag ] = { mAccessors.size() - 1, move( mutator ) };
}

bool XMLMethodRegistryBase::CallAttributeHandler( const wxString &tag,
      void *p, const wchar_t *value )
{
   const auto &table = mMutatorTable;
   if (auto iter = table.find( tag ); iter != table.end())
      // Tag is known
      if (auto &pair = iter->second;
          pair.second && pair.first < mAccessors.size() )
         // Mutator is not null and accessor exists
         if (auto &accessor = mAccessors[pair.first])
            // Accessor is not null; compose accessor and mutator
            return pair.second( accessor( p ), value ), true;
   return false;
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
