/**********************************************************************

Audacity: A Digital Audio Editor

ClientDataHelpers.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CLIENT_DATA_HELPERS__
#define __AUDACITY_CLIENT_DATA_HELPERS__

#include <memory>
#include <mutex>
#include <type_traits>

namespace ClientData {
   // Helpers to define ClientData::Site class template

// To specify (separately for the table of factories, and for the per-Site
// container of client data objects) whether to ensure mutual exclusion.
enum LockingPolicy {
   NoLocking,
   NonrecursiveLocking, // using std::mutex
   RecursiveLocking,    // using std::recursive_mutex
};

// To specify how the Site implements its copy constructor and assignment.
// (Move construction and assignment always work.)
enum CopyingPolicy {
   SkipCopying,     // copy ignores the argument and constructs empty
   ShallowCopying,  // just copy smart pointers; won't compile for unique_ptr
   DeepCopying,     // requires ClientData to define a Clone() member;
                    //    won't compile for weak_ptr (and wouldn't work)
};

// forward declarations
struct Base;
template<
   template<typename> class Owner
> struct Cloneable;

// A conversion so we can use operator * in all the likely cases for the
// template parameter Pointer.  (Return value should be bound only to const
// lvalue references)
template< typename Ptr > static inline
   const Ptr &Dereferenceable( Ptr &p )
      { return p; } // returns an lvalue
template< typename Obj > static inline
   std::shared_ptr<Obj> Dereferenceable( std::weak_ptr<Obj> &p )
      { return p.lock(); } // overload returns a prvalue

// Decorator template to implement locking policies
template< typename Object, LockingPolicy > struct Lockable;
template< typename Object > struct Lockable< Object, NoLocking >
: Object {
   // implement trivial non-locking policy
   struct Lock{};
   Lock lock() const { return {}; }
};
template< typename Object > struct Lockable< Object, NonrecursiveLocking >
: Object, std::mutex {
   // implement real locking
   using Lock = std::unique_lock< std::mutex >;
   Lock lock() const { return Lock{ *this }; }
};
template< typename Object > struct Lockable< Object, RecursiveLocking >
: Object, std::recursive_mutex {
   // implement real locking
   using Lock = std::unique_lock< std::recursive_mutex >;
   Lock lock() const { return Lock{ *this }; }
};

// Pairing of a reference to a Lockable and a lock on it
template< typename Lockable > struct Locked
   // inherit, maybe empty base class optimization applies:
   : private Lockable::Lock
{
   explicit Locked( Lockable &object )
      : Lockable::Lock( object.lock() )
      , mObject( object )
   {}
   Lockable &mObject;
};

// Decorator template implements the copying policy
template< typename Container, CopyingPolicy > struct Copyable;
template< typename Container > struct Copyable< Container, SkipCopying >
: Container {
   Copyable() = default;
   Copyable( const Copyable & ) {}
   Copyable &operator=( const Copyable & ) { return *this; }
   Copyable( Copyable && ) = default;
   Copyable &operator=( Copyable&& ) = default;
};
template< typename Container > struct Copyable< Container, ShallowCopying >
: Container {
   Copyable() = default;
   Copyable( const Copyable &other )
   { *this = other;  }
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
template< typename Container > struct Copyable< Container, DeepCopying >
: Container {
   Copyable() = default;
   Copyable( const Copyable &other )
   { *this = other;  }
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
