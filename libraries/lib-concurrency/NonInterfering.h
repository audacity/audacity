/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 NonInterfering.h
 
 Paul Licameli split from MemoryX.h
 
 **********************************************************************/
#ifndef __AUDACITY_NON_INTERFERING
#define __AUDACITY_NON_INTERFERING

#include <memory>
#include <new> // align_val_t and hardware_destructive_interference_size

//! Non-template helper for class template NonInterfering
/*!
 If a structure contains any members with large alignment, this base class may also allow it to work in
 macOS builds under current limitations of the C++17 standard implementation.
 */
struct CONCURRENCY_API alignas(
#if defined(_WIN32) && defined(_MSC_VER)
   // MSVC supports this symbol in std, but MinGW uses libstdc++, which it does not.
   std::hardware_destructive_interference_size
#else
   // That constant isn't defined for the other builds yet
   64 /* ? */
#endif
)

NonInterferingBase {
   static void *operator new(std::size_t count, std::align_val_t al);
   static void operator delete(void *ptr, std::align_val_t al);

#if defined (_MSC_VER) && defined(_DEBUG)
   // Versions that work in the presence of the DEBUG_NEW macro.
   // Ignore the arguments supplied by the macro and forward to the
   // other overloads.
   static void *operator new(
      std::size_t count, std::align_val_t al, int, const char *, int)
   { return operator new(count, al); }
   static void operator delete(
      void *ptr, std::align_val_t al, int, const char *, int)
   { return operator delete(ptr, al); }
#endif
};

//! Workaround for std::make_shared not working on macOs with over-alignment
/*!
 Defines a static member function to use as an alternative to that in std::
 */
template<typename T> // CRTP
struct SharedNonInterfering : NonInterferingBase
{
   template<typename... Args>
   static std::shared_ptr<T> make_shared(Args &&...args)
   {
      return std::
#ifdef __APPLE__
         // shared_ptr must be constructed from unique_ptr on Mac
         make_unique
#else
         make_shared
#endif
                    <T>(std::forward<Args>(args)...);
   }
};

/*! Given a structure type T, derive a structure with sufficient padding so that there is not false sharing of
 cache lines between successive elements of an array of those structures.
 */
template< typename T > struct NonInterfering
   : NonInterferingBase // Inherit operators; use empty base class optimization
   , T
{
   using T::T;

   //! Allow assignment from default-aligned base type
   void Set(const T &other)
   {
      T::operator =(other);
   }

   //! Allow assignment from default-aligned base type
   void Set(T &&other)
   {
      T::operator =(std::move(other));
   }
};

#endif
