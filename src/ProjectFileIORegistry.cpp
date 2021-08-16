/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIORegistry.cpp

  Paul Licameli

**********************************************************************/

#include "ProjectFileIORegistry.h"

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

ProjectFileIORegistry &ProjectFileIORegistry::Get()
{
   static ProjectFileIORegistry instance;
   return instance;
}
