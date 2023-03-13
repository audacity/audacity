/*!********************************************************************

Audacity: A Digital Audio Editor

@file ClientData.h
@brief Utility ClientData::Site to register hooks into a host class that attach client data

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CLIENT_DATA__
#define __AUDACITY_CLIENT_DATA__

#include "ClientDataHelpers.h"

#include <functional>
#include <iterator>
#include <utility>
#include <vector>
#include "InconsistencyException.h"

//! @copydoc ClientData.h
namespace ClientData {

//! A convenient default parameter for class template @b Site
struct REGISTRIES_API Base
{
   virtual ~Base();
};

//! A one-argument alias template for the default template-template parameter of ClientData::Site
/*! (std::unique_ptr has two, the second is defaulted) */
template< typename Object > using UniquePtr = std::unique_ptr< Object >;

//! This template-template parameter for ClientData::Site risks dangling pointers, so be careful
template< typename Object > using BarePtr = Object*;

//! A convenient base class defining abstract virtual Clone() for a given kind of pointer
/*!
 @tparam Owner template-template parameter for the kind of smart pointer, like std::shared_ptr, returned by Clone()
 
 @sa ClientData::DeepCopying
 */
template<
   typename Covariant = void, // CRTP derived class when not void
   template<typename> class Owner = UniquePtr
> struct
#ifdef _WIN32
__declspec(dllexport)
#endif
Cloneable
{
   using Base = std::conditional_t<
      std::is_void_v<Covariant>, Cloneable, Covariant
   >;
   using PointerType = Owner< Base >;

   virtual ~Cloneable() {};
   virtual PointerType Clone() const = 0;
};

//! Utility to register hooks into a host class that attach client data
/*!
   This allows the host object to be the root of an ownership tree of sub-objects at
   run-time, but inverting the compile-time dependency among implementation files:
   The host's implementation is in low-level files, and cyclic file dependencies are avoided.
   The set of client objects attached to each host object is not fixed in the definition of
   the host class, but instead a system of registration of factories of client objects lets it
   be open-ended.
 
   Besides mere storage and retrieval, this can also implement the [observer pattern](https://en.wikipedia.org/wiki/Observer_pattern),
   in which the host pushes notifications to some virtual function defined in
   each attached item.

   @par Host side usage pattern

 ```
class Host;
class AbstractClientData // Abstract base class for attached data
{
   virtual ~AbstractClientData(); // minimum for memory management

   // optional extra observer protocols
   virtual void NotificationMethod(
      // maybe host passes reference to self, maybe not
      // Host &host
   ) = 0;
};

class Host
   : public ClientData::Site< Host, AbstractClientData >
   // That inheritance is a case of CRTP
   // (the "curiously recurring template pattern")
   // in which the base class takes the derived class as a template argument
{
public:
   Host()
   {
      // If using an Observer protocol, force early creation of all client
      // data:
      BuildAll();
   }

   void NotifyAll()
   {
      // Visit all non-null objects
      ForEach( []( AbstractClientData &data ){
         data.NotificationMethod(
           // *this
         );
      } );
   }
}
```

  @par Client side usage pattern
 
 ```
class MyClientData : public AbstractClientData
{
public:
   MyClientData( Host &host )
   {
      // ... use host, maybe keep a back pointer to it, maybe not,
      // depending how Host declares NotificationMethod ...
      // ... Maybe Host too is an abstract class and we invoke some
      // virtual function of it ...
   }
   void NotificationMethod(
      // Host &host
   ) override
   {
      // ... Observer actions
      // (If there is more than one separately compiled module using this
      // protocol, beware that the sequence of notifications is unspecified)
   }

private:
   int mExtraStuff;
};

// Registration of a factory at static initialization time, to be called
// when a Host uses BuildAll, or else lazily when client code uses
// Host::Get()
static const Host::RegisteredFactory key{
   []( Host &host ){ return std::make_unique< MyClientData >( host ); }
};

// Use of that key at other times, not dependent on notifications from
// the core
void DoSomething( Host &host )
{
   // This may force lazy construction of MyClientData, always returning
   // an object (or else throwing)
   auto &data = host.Get< MyClientData >( key );
   auto val = pData->mExtraStuff;
   // ...
}

void DoAnotherThing( Host &host )
{
   // Does not force lazy construction of MyClientData
   auto *pData = host.Find< MyClientData >( key );
   if ( pData ) {
      auto val = data.mExtraStuff;
      // ...
   }
}

void DoYetAnotherThing( Host &host )
{
   // Reassign the pointer in this client's slot
   host.Assign( key, MyReplacementObject( host ) );
}
```

   @par Lazy or eager construction

   If the client only needs retrieval, it might need
   construction of data only on demand.  But if the host is meant to push
   notifications to the clients, then the host class is responsible for forcing
   early building of all ClientData when host is constructed, as in the example
   above.

   @par Unusual registration sequences

   If registration of a factory
   happens after some host objects are already in existence, their associated
   client data fail to be created if you rely only on BuildAll in the @B Host
   constructor.  Early deregistration of factories is permitted, in which case
   any later constructed host objects will carry null pointers in the associated
   slot, and a small "leak" in the space of per-host slots will persist until
   the program exits.  All such usage is not recommended.
 
 @tparam Host
 Type that derives from this base class; it
 supports hooks to invoke attached object factories.  This is an example of the
 [curiously recurring template pattern](https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern#General_form)

 @tparam ClientData Common base class of attachments; must have a virtual destructor
 
 @tparam CopyingPolicy @ref CopyingPolicy value Chooses deep, shallow, or (default) no-op copying of attachments
 
 @tparam Pointer
 The kind of pointer @b Host will hold to ClientData; default is std::unique_ptr.
 You might want to use std::shared_ptr, std::weak_ptr, or wxWeakRef instead
 
 @tparam ObjectLockingPolicy @ref LockingPolicy value chooses thread safety policy for array of attachments in each @b Host, default is unsafe
 
 @tparam RegistryLockingPolicy @ref LockingPolicy value chooses thread safety policy for the static table of attachment factory functions, default is unsafe
*/
template<
   typename Host,
   typename ClientData = Base,

   // Remaining parameters are often defaulted

   CopyingPolicy ObjectCopyingPolicy = SkipCopying,

   template<typename> class Pointer = UniquePtr,

   LockingPolicy ObjectLockingPolicy = NoLocking,
   LockingPolicy RegistryLockingPolicy = NoLocking
>
class Site
{
public:
   ~Site()
   {
      static_assert( std::has_virtual_destructor<ClientData>::value,
         "ClientData::Site requires a data class with a virtual destructor" );
   }

   using DataType = ClientData;
   using DataPointer = Pointer< ClientData >;
   //! Type of function from which RegisteredFactory is constructed; it builds attachments
   using DataFactory = std::function< DataPointer( Host& ) >;

   Site()
   {
      auto factories = GetFactories();
      auto size = factories.mObject.size();
      mData.reserve( size );
   }
   Site( const Site &other )
      : mData( other.mData )
      { }
   Site& operator =( const Site & other )
      { mData = other.mData; return *this; }
   Site( Site && other )
      : mData( std::move(other.mData) )
      { }
   Site& operator =( Site && other )
      { mData = std::move(other.mData); return *this; }

   //! How many attachment pointers are in the Site
   size_t size() const { return mData.size(); }

   //! How many static factories have been registered with this specialization of Site
   /*!
    Usually agrees with the size() of each site unless some registrations happened later
    than some Site's construction.
    */
   static size_t slots() { return GetFactories().mObject.size(); }

   //! Client code makes static instance from a factory of attachments; passes it to @ref Get or @ref Find as a retrieval key
   /*!
   It can be destroyed to de-register the factory, but usually not before
   destruction of statics at program exit.
   */
   class RegisteredFactory
   {
   public:
      RegisteredFactory(
         DataFactory factory
      )
      {
         auto factories = GetFactories();
         mIndex = factories.mObject.size();
         factories.mObject.emplace_back( std::move( factory ) );
      }
      RegisteredFactory( RegisteredFactory &&other )
      {
         mIndex = other.mIndex;
         mOwner = other.mOwner;
         other.mOwner = false;
      }
      ~RegisteredFactory()
      {
         if (mOwner) {
            auto factories = GetFactories();
            // Should always be true, the factory vector never shrinks:
            if ( mIndex < factories.mObject.size() )
               factories.mObject[mIndex] = nullptr;
         }
      }
   private:
      friend Site;
      bool mOwner{ true };
      size_t mIndex;
   };

   //! @name Retrieval and reassignment of attachments
   //! @{

   //! Get reference to an attachment, creating on demand if not present, down-cast it to @b Subclass
   /*!
    Uses static_cast.  Throws on failure to create it.
    (Lifetime of the object may depend on the host's lifetime and also on the
    client's use of Assign(). Site is not responsible for guarantees.)
    @tparam Subclass Expected actual type of attachment, assumed to be correct
    @param key Reference to static object created in client code
    */
   template< typename Subclass = ClientData >
   Subclass &Get( const RegisteredFactory &key )
   {
      auto data = GetData();
      return DoGet< Subclass >( data, key );
   }

   //! @copydoc Get
   /*! const overload returns const references only. */
   template< typename Subclass = const ClientData >
   auto Get( const RegisteredFactory &key ) const ->
      std::enable_if_t< std::is_const< Subclass >::value, Subclass & >
   {
      auto data = GetData();
      return DoGet< Subclass >( data, key );
   }

   //!Get a (bare) pointer to an attachment, or null, down-cast it to @b Subclass *; will not create on demand
   /*!
    (Lifetime of the object may depend on the host's lifetime and also on the
    client's use of Assign(). Site is not responsible for guarantees.)
    @tparam Subclass Expected actual type of attachment, assumed to be correct
    @param key  Reference to static object created in client code
    */
   template< typename Subclass = ClientData >
   Subclass *Find( const RegisteredFactory &key )
   {
      auto data = GetData();
      return DoFind< Subclass >( data, key );
   }

   //! @copydoc Find
   /*! const overload returns pointers to const only. */
   template< typename Subclass = const ClientData >
   auto Find( const RegisteredFactory &key ) const ->
      std::enable_if_t< std::is_const< Subclass >::value, Subclass * >
   {
      auto data = GetData();
      return DoFind< Subclass >( data, key );
   }

   //! Reassign Site's pointer to ClientData.
   /*!
   If @b ObjectLockingPolicy isn't default, then reassignments are serialized.
   @tparam ReplacementPointer @b Pointer<Subclass> where @b Subclass derives ClientData
   */
   template< typename ReplacementPointer >
   void Assign(
      const RegisteredFactory &key, //!< Reference to static object created in client code
      ReplacementPointer &&replacement //!< A temporary or std::move'd pointer
   )
   {
      auto index = key.mIndex;
      auto data = GetData();
      EnsureIndex( data, index );
      auto iter = GetIterator( data, index );
      // Copy or move as appropriate:
      *iter = std::forward< ReplacementPointer >( replacement );
   }
   
   //! @}

protected:
   //! @name member functions for use by @b Host
   //! @{

   //! Invoke function on each ClientData object that has been created in @c this
   /*!
   @tparam Function takes reference to ClientData, return value is ignored
   @param function of type @b Function
    */
   template< typename Function >
   void ForEach( const Function &function )
   {
      auto data = GetData();
      for( auto &pObject : data.mObject ) {
         const auto &ptr = Dereferenceable(pObject);
         if ( ptr )
            function( *ptr );
      }
   }

   //! @copydoc ForEach
   /*! const overload only compiles with a function that takes reference to const ClientData. */
   template< typename Function >
   void ForEach( const Function &function ) const
   {
      auto data = GetData();
      for( auto &pObject : data.mObject ) {
         const auto &ptr = Dereferenceable(pObject);
         if ( ptr ) {
            const auto &c_ref = *ptr;
            function( c_ref );
         }
      }
   }

   //! Return pointer to first attachment in @c this that is not null and satisfies a predicate, or nullptr
   /*!
   Beware that the sequence of visitation is not specified.
   @tparam Function takes reference to ClientData, returns value convertible to bool
   @param function of type @b Function
    */
   template< typename Function >
   ClientData *FindIf( const Function &function )
   {
      auto data = GetData();
      for( auto &pObject : data.mObject ) {
         const auto &ptr = Dereferenceable(pObject);
         if ( ptr && function ( *ptr ) )
            return &*ptr;
      }
      return nullptr;
   }

   //! @copydoc FindIf
   /*! const overload only compiles with a function callable with a const reference to ClientData. */
   template< typename Function >
   const ClientData *FindIf( const Function &function ) const
   {
      auto data = GetData();
      for( auto &pObject : data.mObject ) {
         const auto &ptr = Dereferenceable(pObject);
         if ( ptr ) {
            const auto &c_ref = *ptr;
            if ( function( c_ref ) )
               return &*c_ref;
         }
      }
      return nullptr;
   }

   //! For each RegisteredFactory, if the corresponding attachment is absent in @c this, build and store it
   void BuildAll()
   {
      // Note that we cannot call this function in the Site constructor as we
      // might wish, because we pass *this to the factories, but this is not yet
      // fully constructed as the ultimate derived class.  So delayed calls to
      // this function are needed.
      size_t size;
      {
         auto factories = GetFactories();
         size = factories.mObject.size();
         // Release lock on factories before getting one on data -- otherwise
         // there would be a deadlock possibility inside Ensure
      }
      auto data = GetData();
      EnsureIndex( data, size - 1 );
      auto iter = GetIterator( data, 0 );
      for ( size_t ii = 0; ii < size; ++ii, ++iter )
         static_cast< void >( Build( data, iter, ii ) );
   }

   //! @}

private:
   using DataFactories =
      Lockable< std::vector< DataFactory >, RegistryLockingPolicy >;
   using DataContainer =
      Lockable<
         Copyable< std::vector< DataPointer >, ObjectCopyingPolicy >,
         ObjectLockingPolicy
      >;

   decltype( Dereferenceable( std::declval<DataPointer&>() ) )
   Slot( Locked<DataContainer> &data, const RegisteredFactory &key, bool create )
   {
      auto index = key.mIndex;
      EnsureIndex( data, index );
      auto iter = GetIterator( data, index );
      auto &pointer = create ? Build( data, iter, index ) : *iter;
      return Dereferenceable( pointer );
   }

   template< typename Subclass >
   Subclass &DoGet( Locked<DataContainer> &data, const RegisteredFactory &key )
   {
      const auto &d = Slot( data, key, true );
      if (!d)
         // Oops, a factory was deregistered too soon, or returned a null, or
         // the pointer was reassigned null
         THROW_INCONSISTENCY_EXCEPTION;
      return static_cast< Subclass& >( *d );
   }

   template< typename Subclass >
   Subclass *DoFind( Locked<DataContainer> &data, const RegisteredFactory &key )
   {
      const auto &d = Slot( data, key, false );
      if (!d)
         return nullptr;
      else
         return static_cast< Subclass* >( &*d );
   }

   static Locked< DataFactories > GetFactories()
   {
      // C++11 does not need extra thread synch for static initialization
      // Note that linker eliminates duplicates of this function
      static DataFactories factories;

      // But give back a scoped lock to the user of this function, in case
      // there is contention to resize the vector
      return Locked< DataFactories >{ factories };
   }

   Locked<DataContainer> GetData()
   {
      return Locked< DataContainer >{ mData };
   }
   
   Locked<const DataContainer> GetData() const
   {
      return Locked< const DataContainer >{ mData };
   }
   
   static void EnsureIndex( Locked<DataContainer> &data, size_t index )
   {
      if (data.mObject.size() <= index)
         data.mObject.resize(index + 1);
   }

   static typename DataContainer::iterator inline
   GetIterator( Locked<DataContainer> &data, size_t index )
   {
      // This function might help generalize Site with different kinds of
      // containers for pointers to ClientData that are not random-access.
      // Perhaps non-relocation of elements will be needed.
      // Perhaps another template-template parameter could vary the kind of
      // container.
      auto result = data.mObject.begin();
      std::advance( result, index );
      return result;
   }

   DataPointer &Build( Locked<DataContainer> &,
      typename DataContainer::iterator iter, size_t index )
   {
      // If there is no object at index, then invoke the factory, else do
      // nothing.
      // The factory may be null or may return null, in which case do nothing.
      auto &result = *iter;
      if (!Dereferenceable(result)) {
         // creation on demand
         auto factories = GetFactories();
         auto &factory = factories.mObject[index];
         result = factory
            ? factory( static_cast< Host& >( *this ) )
            : DataPointer{};
      }
      return result;
   }

   //! Container of pointers returned by factories, per instance of @b Host class
   /*! This is the only non-static data member that Site injects into the derived class. */
   DataContainer mData;
};

}

#endif
