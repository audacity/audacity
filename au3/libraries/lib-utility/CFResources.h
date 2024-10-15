/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file CFResources.h

 @brief Wrap resource pointers from Apple Core SDK for RAII
 
 Paul Licameli
 
 **********************************************************************/

#ifndef __AUDACITY_CFRESOURCES__
#define __AUDACITY_CFRESOURCES__

#ifdef __APPLE__

#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>
#include <CoreFoundation/CFBase.h>

template<typename T = void *> struct CFReleaser{
   void operator () (T p) const noexcept { if (p) CFRelease(p); } };

//! Smart pointer to a resource requiring CFRelease to clean up
/*!
 Specializes std::unique_ptr with a custom deleter.  Destructor decrements a
 reference count, which does not necessarily destroy the object.  The unique_ptr
 represents a unique obligation to reduce that reference count and can move or
 be released like any other unique_ptr.  It can't be copied.

 It does not increase the reference count when it is constructed.

 @tparam T A pointer typedef from Core SDK headers -- not the type pointed to!
 Parametrizing this way lets you avoid mention of the pointee typedef names
 which begin with __ and shouldn't really be mentioned by client code.
 */
template<typename T> using CF_ptr =
   std::unique_ptr<std::remove_pointer_t<T>, CFReleaser<T>>;

extern template struct CFReleaser<>;

#endif
#endif
