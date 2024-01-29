/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 AtomicUniquePointer.h
 
 Paul Licameli split from NonInterfering.h
 
 **********************************************************************/
#ifndef __AUDACITY_ATOMIC_UNIQUE_POINTER__
#define __AUDACITY_ATOMIC_UNIQUE_POINTER__

//! Atomic unique pointer (for nonarray type only) with a destructor;
//! It doesn't copy or move
template<typename T>
struct AtomicUniquePointer : public std::atomic<T*> {
   static_assert(AtomicUniquePointer::is_always_lock_free);
   using std::atomic<T*>::atomic;
   //! Reassign the pointer with release ordering,
   //! then destroy any previously held object
   /*!
    Like `std::unique_ptr`, does not check for reassignment of the same pointer */
   void reset(T *p = nullptr) {
      delete this->exchange(p, std::memory_order_release);
   }
   //! reset to a pointer to a new object with given ctor arguments
   template<typename... Args> void emplace(Args &&... args) {
      reset(safenew T(std::forward<Args>(args)...));
   }
   ~AtomicUniquePointer() { reset(); }
private:
   //! Disallow pointer arithmetic
   using std::atomic<T*>::fetch_add;
   using std::atomic<T*>::fetch_sub;
};

#endif
