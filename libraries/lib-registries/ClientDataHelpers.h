/*!********************************************************************

Audacity: A Digital Audio Editor

@file ClientDataHelpers.h
@brief Some implementation details for ClientData

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CLIENT_DATA_HELPERS__
#define __AUDACITY_CLIENT_DATA_HELPERS__

#include <memory>
#include <mutex>
#include <type_traits>

namespace ClientData {
   // Helpers to define ClientData::Site class template

//! Statically specify whether there is mutual exclusion (separately for the table of factories, and for the per-host container of client objects).
/*! Used as non-type template parameter of ClientData::Site */
enum LockingPolicy {
   NoLocking,
   NonrecursiveLocking, //!< using std::mutex
   RecursiveLocking,    //!< using std::recursive_mutex
};

//! Statically specify how the ClientData::Site implements its copy constructor and assignment.
/*! (Move construction and assignment always work.)
 Used as non-type template parameter of ClientData::Site */
enum CopyingPolicy {
   SkipCopying,     //!< ignore the source and leave empty
   ShallowCopying,  //!< copy pointers only; won't compile for std::unique_ptr
   DeepCopying,     //!< point to new sub-objects; these must define a Clone() member; won't compile for std::weak_ptr
};

// forward declarations
struct Base;
template<
   typename Covariant,
   template<typename> class Owner
> struct Cloneable;

//! Conversion allowing operator * on any @b Pointer parameter of ClientData::Site
/*! Return value should be bound to a const reference */
template< typename Ptr > static inline
   const Ptr &Dereferenceable( Ptr &p )
      { return p; }
//! Overload of ClientData::Dereferenceable returns an rvalue
template< typename Obj > static inline
   std::shared_ptr<Obj> Dereferenceable( std::weak_ptr<Obj> &p )
      { return p.lock(); }

//! Decorator template injects type Lock and method lock() into interface of @b Object
/*!
 @tparam Object decorated class
 @tparam LockingPolicy one of ClientData::LockingPolicy
 */
template< typename Object, LockingPolicy > struct Lockable{};
//! Specialization for trivial, non-locking policy
template< typename Object > struct Lockable< Object, NoLocking >
: Object {
   //! Empty class
   struct Lock{};
   Lock lock() const { return {}; }
};
//! Specialization for real locking with std::mutex
template< typename Object > struct Lockable< Object, NonrecursiveLocking >
: Object, std::mutex {
   using Lock = std::unique_lock< std::mutex >;
   Lock lock() const { return Lock{ *this }; }
};
//! Specialization for real locking with std::recursive_mutex
template< typename Object > struct Lockable< Object, RecursiveLocking >
: Object, std::recursive_mutex {
   using Lock = std::unique_lock< std::recursive_mutex >;
   Lock lock() const { return Lock{ *this }; }
};

//! Decorated reference to a ClientData::Lockable, with a current lock on it
/*! Uses inheritance to benefit from the empty base class optimization if possible */
template< typename Lockable > struct Locked
   : private Lockable::Lock
{
   explicit Locked( Lockable &object )
      : Lockable::Lock( object.lock() )
      , mObject( object )
   {}
   Lockable &mObject;
};

//! Decorator template injects copy and move operators for container of pointers
template< typename Container, CopyingPolicy > struct Copyable{};
//! Specialization that ignores contents of the source when copying (not when moving).
template< typename Container > struct Copyable< Container, SkipCopying >
: Container {
   Copyable() = default;
   Copyable( const Copyable & ) {}
   Copyable &operator=( const Copyable & ) { return *this; }
   Copyable( Copyable && ) = default;
   Copyable &operator=( Copyable&& ) = default;
};
//! Specialization that copies pointers, not sub-objects; [strong guarantee](@ref Strong-guarantee) for assignment
template< typename Container > struct Copyable< Container, ShallowCopying >
: Container {
   Copyable() = default;
   //! Call through to operator =
   Copyable( const Copyable &other )
   { *this = other;  }
   //! @excsafety{Strong}
   Copyable &operator=( const Copyable &other )
   {
      if (this != &other) {
         // Build then swap for strong exception guarantee
         Copyable temp;
         for ( auto &&ptr : other )
            temp.push_back( ptr );
         this->swap( temp );
      }
      return *this;
   }
   Copyable( Copyable && ) = default;
   Copyable &operator=( Copyable&& ) = default;
};
//! Specialization that clones sub-objects when copying;  [strong guarantee](@ref Strong-guarantee) for assignment
template< typename Container > struct Copyable< Container, DeepCopying >
: Container {
   Copyable() = default;
   //! Call through to operator =
   Copyable( const Copyable &other )
   { *this = other;  }
   //! @excsafety{Strong}
   Copyable &operator=( const Copyable &other )
   {
      if (this != &other) {
         // Build then swap for strong exception guarantee
         Copyable temp;
         for ( auto &&p : other ) {
            using Ptr = decltype( p->Clone() );
            temp.push_back( p ? p->Clone() : Ptr{} );
         }
         this->swap( temp );
      }
      return *this;
   }
   Copyable( Copyable && ) = default;
   Copyable &operator=( Copyable&& ) = default;
};

}

#endif
