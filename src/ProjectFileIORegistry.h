/**********************************************************************

  Audacity: A Digital Audio Editor

  ProjectFileIORegistry.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_PROJECT_FILE_IO_REGISTRY__
#define __AUDACITY_PROJECT_FILE_IO_REGISTRY__

#include <wx/string.h>
#include <functional>
#include <unordered_map>

class AudacityProject;
class XMLTagHandler;

//! Implementation helper for ProjectFileIORegistry.
/*! It makes most of the work non-inline and is used by derived classes that
 supply thin inline type-erasing wrappers. */
class AUDACITY_DLL_API XMLMethodRegistryBase {
public:
   XMLMethodRegistryBase();
   ~XMLMethodRegistryBase();
protected:
   using TypeErasedObjectAccessor = std::function< XMLTagHandler *( void* ) >;
   using TagTable =
      std::unordered_map< wxString, TypeErasedObjectAccessor >;
   TagTable mTagTable;

   void Register( const wxString &tag, TypeErasedObjectAccessor accessor );
   XMLTagHandler *CallObjectAccessor( const wxString &tag, void *p );
};

class AUDACITY_DLL_API ProjectFileIORegistry : public XMLMethodRegistryBase {
public:

   // Typically statically constructed
struct ObjectReaderEntry {
   template <
/*!
 This "accessor" may or may not act as a "factory" that builds a new object and
 may return nullptr.  Caller of the accessor is not responsible for the object
 lifetime, which is assumed to outlast the project loading procedure.
 */
      typename ObjectAccessor /*!< Often a lambda.
         A function from AudacityProject& to XMLTagHandler */
   >
   ObjectReaderEntry( const wxString &tag, ObjectAccessor fn )
   {
      // Remember the function, type-erased
      Get().Register( tag, [ fn = std::move(fn) ] (void *p) {
         // CallObjectAccessor will guarantee p is not null
         return fn( *static_cast<AudacityProject *>(p) );
      } );
   }
};

XMLTagHandler *CallObjectAccessor(
   const wxString &tag, AudacityProject &project )
{
   return XMLMethodRegistryBase::CallObjectAccessor( tag, &project );
}

//! Get the unique instance
static ProjectFileIORegistry &Get();

};

#endif
