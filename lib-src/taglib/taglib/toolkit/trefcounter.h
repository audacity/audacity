/***************************************************************************
    copyright            : (C) 2013 by Tsuda Kageyu
    email                : tsuda.kageyu@gmail.com
 ***************************************************************************/

/***************************************************************************
 *   This library is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU Lesser General Public License version   *
 *   2.1 as published by the Free Software Foundation.                     *
 *                                                                         *
 *   This library is distributed in the hope that it will be useful, but   *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   Lesser General Public License for more details.                       *
 *                                                                         *
 *   You should have received a copy of the GNU Lesser General Public      *
 *   License along with this library; if not, write to the Free Software   *
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *
 *   02110-1301  USA                                                       *
 *                                                                         *
 *   Alternatively, this file is available under the Mozilla Public        *
 *   License Version 1.1.  You may obtain a copy of the License at         *
 *   http://www.mozilla.org/MPL/                                           *
 ***************************************************************************/

#ifndef TAGLIB_REFCOUNTER_H
#define TAGLIB_REFCOUNTER_H

#include "taglib_export.h"
#include "taglib.h"

#ifdef __APPLE__
#  include <libkern/OSAtomic.h>
#  define TAGLIB_ATOMIC_MAC
#elif defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#  define NOMINMAX
#  include <windows.h>
#  define TAGLIB_ATOMIC_WIN
#elif defined (__GNUC__) && (__GNUC__ * 100 + __GNUC_MINOR__ >= 401)    \
      && (defined(__i386__) || defined(__i486__) || defined(__i586__) || \
          defined(__i686__) || defined(__x86_64) || defined(__ia64)) \
      && !defined(__INTEL_COMPILER)
#  define TAGLIB_ATOMIC_GCC
#elif defined(__ia64) && defined(__INTEL_COMPILER)
#  include <ia64intrin.h>
#  define TAGLIB_ATOMIC_GCC
#endif

#ifndef DO_NOT_DOCUMENT // Tell Doxygen to skip this class.
/*!
  * \internal
  * This is just used as a base class for shared classes in TagLib.
  *
  * \warning This <b>is not</b> part of the TagLib public API!
  */
namespace TagLib
{

  class TAGLIB_EXPORT RefCounter
  {
  public:
    RefCounter();
    virtual ~RefCounter();

    void ref();
    bool deref();
    int count() const;

  private:
    class RefCounterPrivate;
    RefCounterPrivate *d;
  };

  // BIC this old class is needed by tlist.tcc and tmap.tcc
  class RefCounterOld
  {
  public:
    RefCounterOld() : refCount(1) {}

#ifdef TAGLIB_ATOMIC_MAC
    void ref() { OSAtomicIncrement32Barrier(const_cast<int32_t*>(&refCount)); }
    bool deref() { return ! OSAtomicDecrement32Barrier(const_cast<int32_t*>(&refCount)); }
    int32_t count() { return refCount; }
  private:
    volatile int32_t refCount;
#elif defined(TAGLIB_ATOMIC_WIN)
    void ref() { InterlockedIncrement(&refCount); }
    bool deref() { return ! InterlockedDecrement(&refCount); }
    long count() { return refCount; }
  private:
    volatile long refCount;
#elif defined(TAGLIB_ATOMIC_GCC)
    void ref() { __sync_add_and_fetch(&refCount, 1); }
    bool deref() { return ! __sync_sub_and_fetch(&refCount, 1); }
    int count() { return refCount; }
  private:
    volatile int refCount;
#else
    void ref() { refCount++; }
    bool deref() { return ! --refCount; }
    int count() { return refCount; }
  private:
    uint refCount;
#endif
  };

}

#endif // DO_NOT_DOCUMENT
#endif

