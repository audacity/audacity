/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLMethodRegistry.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_XML_METHOD_REGISTRY__
#define __AUDACITY_XML_METHOD_REGISTRY__

#include <wx/string.h>
#include <functional>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

class XMLTagHandler;
class XMLWriter;

//! Implementation helper for ProjectFileIORegistry.
/*! It makes most of the work non-inline and is used by derived classes that
 supply thin inline type-erasing wrappers. */
class XML_API XMLMethodRegistryBase {
public:

   //! A helper type alias for a function taking a structure and a string value
template< typename Substructure >
using Mutator = std::function< void(Substructure&, const wchar_t *) >;

//! A helper type alias for a list of mutators, associated with tag strings
template< typename Substructure >
using Mutators = std::vector< std::pair< wxString, Mutator<Substructure> > >;

   XMLMethodRegistryBase();
   ~XMLMethodRegistryBase();
protected:
   using TypeErasedObjectAccessor = std::function< XMLTagHandler *( void* ) >;
   using TagTable =
      std::unordered_map< wxString, TypeErasedObjectAccessor >;
   TagTable mTagTable;

   void Register( const wxString &tag, TypeErasedObjectAccessor accessor );
   XMLTagHandler *CallObjectAccessor( const std::string_view &tag, void *p );

   using TypeErasedAccessor = std::function< void*( void* ) >;
   using TypeErasedAccessors = std::vector< TypeErasedAccessor >;
   TypeErasedAccessors mAccessors;

   void PushAccessor( TypeErasedAccessor accessor );

   using TypeErasedMutator = std::function< void( void*, const wchar_t* ) >;
   //! From attribute name, to index in accessor table with a mutator
   using MutatorTable =
      std::unordered_map< wxString, std::pair< size_t, TypeErasedMutator > >;
   MutatorTable mMutatorTable;

   void Register( const wxString &tag, TypeErasedMutator mutator );

   bool CallAttributeHandler( const wxString &tag,
      void *p, const wchar_t *value );

   using TypeErasedWriter = std::function< void(const void *, XMLWriter &) >;
   using WriterTable = std::vector< TypeErasedWriter >;
   WriterTable mWriterTable;

   void Register( TypeErasedWriter writer );
   void CallWriters( const void *p, XMLWriter &writer );
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

XMLTagHandler *CallObjectAccessor(const std::string_view& tag, Host& host)
{
   return XMLMethodRegistryBase::CallObjectAccessor( tag, &host );
}

/*! Typically statically constructed */
/*!
   Registers procedures to update the host for certain attributes, in two
   steps:  first the `accessor` which fetches some substructure owned by the
   host, which is the common step; then, the `mutators` that rewrite the
   substructure, each of them associated with one attribute string, and
   taking a reference to the substructure and a value string.
 */
struct AttributeReaderEntries {
   template<
      typename Accessor, /*!< Often a lambda.
         Takes const Host& and returns Substructure& */
      typename Substructure //<! Type deduction of the return of Accessor
         = std::remove_reference_t< decltype(
            std::declval<Accessor>()( std::declval<Host &>() )
         ) >
   >
   AttributeReaderEntries( Accessor fn, Mutators<Substructure> pairs )
   {
      // Remember the functions, type-erased
      auto &registry = Get();
      registry.PushAccessor(
         [ fn = std::move(fn) ] ( void *p ) {
            // CallAttributeHandler will guarantee p is not null
            return &fn( *static_cast<Host *>(p) ); }
      );
      for (auto &pair : pairs)
         registry.Register( pair.first,
            [ fn = move(pair.second) ]( auto p, auto value ){
               fn( *static_cast<Substructure*>(p), value ); }
         );
   }
};

// @return whether any function was found and called for the tag
bool CallAttributeHandler(
   const wxString &tag, Host &host, const wchar_t *value )
{
   return XMLMethodRegistryBase::CallAttributeHandler( tag, &host, value );
}

//! Typically statically constructed
struct WriterEntry {
   template <
/*!
 The Writer may write any number of XML attributes or tags or both.
 So there should be some reader entries corresponding to each writer, and that
 may be many-to-one.
 The readers must not make assumptions about the sequence in which they will
 be called.
 */
      typename Writer /*!< Often a lambda.
         Takes const Host& and XMLWriter& and returns void */
   >
   explicit WriterEntry( Writer fn )
   {
      // Remember the function, type-erased
      Get().Register(
         [ fn = std::move(fn) ] ( const void *p, XMLWriter &writer ) {
            // CallObjectAccessor will guarantee p is not null
            return fn( *static_cast<const Host *>(p), writer );
         } );
   }
};

void CallWriters( const Host &host, XMLWriter &writer )
{
   XMLMethodRegistryBase::CallWriters( &host, writer );
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
