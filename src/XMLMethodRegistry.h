/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLMethodRegistry.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_XML_METHOD_REGISTRY__
#define __AUDACITY_XML_METHOD_REGISTRY__

#include <wx/string.h>
#include <functional>
#include <unordered_map>

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

/*! A class template of inline type-erasing wrapper functions, but one function
 with linkage is also needed.  See the macros that help generate that function.
*/
template< typename Host >
class XMLMethodRegistry : public XMLMethodRegistryBase {
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
         A function from Host& to XMLTagHandler*, maybe returning null */
   >
   ObjectReaderEntry( const wxString &tag, ObjectAccessor fn )
   {
      // Remember the function, type-erased
      Get().Register( tag, [ fn = std::move(fn) ] (void *p) {
         // CallObjectAccessor will guarantee p is not null
         return fn( *static_cast<Host *>(p) );
      } );
   }
};

XMLTagHandler *CallObjectAccessor(
   const wxString &tag, Host &host )
{
   return XMLMethodRegistryBase::CallObjectAccessor( tag, &host );
}

//! Get the unique instance
static XMLMethodRegistry &Get();

};

/*! Typically follows the `using` declaration of an XMLMethodRegistry
   specialization; DECLSPEC is for linkage visibility */
#define DECLARE_XML_METHOD_REGISTRY(DECLSPEC, Name) \
   template<> auto DECLSPEC Name::Get() -> Name &;

/*! Typically in the companion .cpp file */
#define DEFINE_XML_METHOD_REGISTRY(Name)  \
   template<> auto Name::Get() -> Name &  \
   {                                      \
      static Name registry;               \
      return registry;                    \
   }

#endif
