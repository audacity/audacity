/**********************************************************************

Audacity: A Digital Audio Editor

ClientData.h

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

namespace ClientData {

// A convenient default parameter for class template Site below
struct AUDACITY_DLL_API Base
{
   virtual ~Base() {}
};

// Need a truly one-argument alias template below for the default
// template-template argument of Site
// (unique_ptr has two, the second is defaulted)
template< typename Object > using UniquePtr = std::unique_ptr< Object >;

// Risk of dangling pointers, so be careful
template< typename Object > using BarePtr = Object*;

// A convenient base class defining abstract virtual Clone() for a given kind
// of pointer
template<
   template<typename> class Owner = UniquePtr
> struct Cloneable
{
   using Base = Cloneable;
   using PointerType = Owner< Base >;

   virtual ~Cloneable() {}
   virtual PointerType Clone() const = 0;
};

/*
   \brief ClientData::Site class template enables decoupling of the
   implementation of core data structures, inheriting it, from compilation and
   link dependency on the details of other user interface modules, while also
   allowing the latter to associate their own extra data caches with each
   instance of the core class, and the caches can have a coterminous lifetime.

   This can implement an "observer pattern" in which the core object pushes
   notifications to client-side handlers, or it can just implement storage
   and retrieval services for the client.

   typical usage pattern in core code:

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

   typical usage pattern in client module -- observer pattern, and retrieval
 
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
      // This may force lazy construction of ClientData, always returning
      // an object (or else throwing)
      auto &data = host.Get< MyClientData >( key );
      auto val = data.mExtraStuff;
      // ...
   }

   void DoAnotherThing( Host &host )
   {
      // Does not force lazy construction of ClientData
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

   About laziness:  If the client only needs retrieval, it might need
   construction of data only on demand.  But if the host is meant to push
   notifications to the clients, then the host class is responsible for forcing
   early building of all ClientData when host is constructed, as in the example
   above.

   About unusual registration sequences:  if registration of a factory
   happens after some host objects are already in existence, their associated
   client data fail to be created if you rely only on BuildAll in the Host
   constructor.  Early deregistration of factories is permitted, in which case
   any later constructed host objects will carry null pointers in the associated
   slot, and a small "leak" in the space of per-host slots will persist until
   the program exits.  All such usage is not recommended.
*/

template<
   typename Host,
   typename ClientData = Base,

   // Remaining parameters are often defaulted

   CopyingPolicy ObjectCopyingPolicy = SkipCopying,

   // The kind of pointer Host will hold to ClientData; you might want to
   // use std::shared_ptr, std::weak_ptr, or wxWeakRef instead
   template<typename> class Pointer = UniquePtr,

   // Thread safety policies for the tables of ClientData objects in each Host
   // object, and for the static factory function registry
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

   // Associated type aliases
   using DataType = ClientData;
   using DataPointer = Pointer< ClientData >;
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

   size_t size() const { return mData.size(); }

   static size_t slots() { return GetFactories().mObject.size(); }

   /// \brief a type meant to be stored by client code in a static variable,
   /// and used as a retrieval key to get the manufactured client object back
   /// from the host object.
   /// It can be destroyed to de-register the factory, but usually not before
   /// destruction of statics at program exit.
   class RegisteredFactory
   {
   public:
      RegisteredFactory( DataFactory factory )
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

   // member functions for use by clients

   // \brief Get a reference to an object, creating it on demand if needed, and
   // down-cast it with static_cast.  Throws on failure to create it.
   // (Lifetime of the object may depend on the Host's lifetime and also on the
   // client's use of Assign(). Site is not responsible for guarantees.)
   template< typename Subclass = ClientData >
   Subclass &Get( const RegisteredFactory &key )
   {
      auto data = GetData();
      return DoGet< Subclass >( data, key );
   }

   // const counterpart of the previous
   template< typename Subclass = const ClientData >
   auto Get( const RegisteredFactory &key ) const -> typename
      std::enable_if< std::is_const< Subclass >::value, Subclass & >::type
   {
      auto data = GetData();
      return DoGet< Subclass >( data, key );
   }

   // \brief Get a (bare) pointer to an object, or null, and down-cast it with
   // static_cast.  Do not create any object.
   // (Lifetime of the object may depend on the Host's lifetime and also on the
   // client's use of Assign(). Site is not responsible for guarantees.)
   template< typename Subclass = ClientData >
   Subclass *Find( const RegisteredFactory &key )
   {
      auto data = GetData();
      return DoFind< Subclass >( data, key );
   }

   // const counterpart of the previous
   template< typename Subclass = const ClientData >
   auto Find( const RegisteredFactory &key ) const -> typename
      std::enable_if< std::is_const< Subclass >::value, Subclass * >::type
   {
      auto data = GetData();
      return DoFind< Subclass >( data, key );
   }

  // \brief Reassign Host's pointer to ClientData.
   // If there is object locking, then reassignments to the same slot in the
   // same host object are serialized.
   template< typename ReplacementPointer >
   void Assign( const RegisteredFactory &key, ReplacementPointer &&replacement )
   {
      auto index = key.mIndex;
      auto data = GetData();
      EnsureIndex( data, index );
      auto iter = GetIterator( data, index );
      // Copy or move as appropriate:
      *iter = std::forward< ReplacementPointer >( replacement );
   }

protected:
   // member functions for use by Host

   // \brief Invoke function on each ClientData object that has been created in
   // this, but do not cause the creation of any.
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

   // const counterpart of previous, only compiles with a function that takes
   // a value or const reference argument
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

   // \brief Invoke predicate on the ClientData objects that have been created in
   // this, but do not cause the creation of any.  Stop at the first for which
   // the predicate returns true, and return a pointer to the corresponding
   // object, or return nullptr if no return values were true.
   // Beware that the sequence of visitation is not specified.
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

   // const counterpart of previous, only compiles with a function that takes
   // a value or const reference argument
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

   // \brief For each registered factory, if the corresponding object in this
   // is absent, then invoke the factory and store the result.
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

   // Container of pointers returned by factories, per instance of Host class
   // This is the only non-static data member that Site injects into the
   // derived class.
   DataContainer mData;
};

}

#endif
