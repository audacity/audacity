#ifndef __AUDACITY_MEMORY_X_H__
#define __AUDACITY_MEMORY_X_H__

// C++ standard header <memory> with a few extensions
#include <memory>

// Conditional compilation switch indicating whether to rely on
// std:: containers knowing about rvalue references
#undef __AUDACITY_OLD_STD__

#if defined(__MAC_OS_X_VERSION_MIN_REQUIRED) && __MAC_OS_X_VERSION_MIN_REQUIRED <= __MAC_10_6

#define __AUDACITY_OLD_STD__

#include <math.h>
inline long long int llrint(float __x) { return __builtin_llrintf(__x); }
inline long long int llrint(double __x) { return __builtin_llrintl(__x); }
inline long long int llrint(long double __x) { return __builtin_llrintl(__x); }

#include <cmath>
using std::isnan;
using std::isinf;

// Need this to define move() and forward()
#include <tr1/type_traits>

// To define make_shared
#include <tr1/memory>

namespace std {
   using std::tr1::shared_ptr;
   using std::tr1::remove_reference;

   struct nullptr_t
   {
      void* __lx;

      struct __nat {int __for_bool_;};

      nullptr_t() : __lx(0) {}
      nullptr_t(int __nat::*) : __lx(0) {}

      operator int __nat::*() const {return 0;}

      template <class _Tp>
         operator _Tp* () const {return 0;}

      template <class _Tp, class _Up>
         operator _Tp _Up::* () const {return 0;}

      friend bool operator==(nullptr_t, nullptr_t) {return true;}
      friend bool operator!=(nullptr_t, nullptr_t) {return false;}
      friend bool operator<(nullptr_t, nullptr_t) {return false;}
      friend bool operator<=(nullptr_t, nullptr_t) {return true;}
      friend bool operator>(nullptr_t, nullptr_t) {return false;}
      friend bool operator>=(nullptr_t, nullptr_t) {return true;}
   };

   inline nullptr_t __get_nullptr_t() {return nullptr_t(0);}

   #define nullptr std::__get_nullptr_t()

   template<typename T> class unique_ptr {
   public:
      // Default constructor
      unique_ptr() {}

      // Explicit constructor from pointer
      explicit unique_ptr(T *p_) : p{ p_ } {}
      // Template constructor for upcasting
      template<typename U>
      explicit unique_ptr(U* p_) : p{ p_ } {}

      // Copy is disallowed
      unique_ptr(const unique_ptr &) PROHIBITED;
      unique_ptr& operator= (const unique_ptr &) PROHIBITED;

      // But move is allowed!
      unique_ptr(unique_ptr &&that) : p{ that.release() } { }
      unique_ptr& operator= (unique_ptr &&that)
      {
         if (this != &that) {
            delete p;
            p = that.release();
         }
         return *this;
      }

      // Assign null
      unique_ptr& operator= (nullptr_t)
      {
         delete p;
         p = nullptr;
         return *this;
      }

      // Template versions of move for upcasting
      template<typename U>
      unique_ptr(unique_ptr<U> &&that) : p{ that.release() } { }
      template<typename U>
      unique_ptr& operator= (unique_ptr<U> &&that)
      {
         // Skip the self-assignment test -- self-assignment should go to the non-template overload
         delete p;
         p = that.release();
         return *this;
      }

      ~unique_ptr() { delete p; }

      T* operator -> () const { return p; }
      T& operator * () const { return *p; }
      T* get() const { return p; }

      // So you can say if(p)
      explicit operator bool() const { return p != nullptr; }

      // Give up ownership, don't destroy
      T* release() { T* result = p; p = nullptr; return result; }

      void reset(T* __p = nullptr)
      {
         T* old__p = p;
         p = __p;
         if (old__p != nullptr)
         {
            delete old__p;
         }
      }


      // Equality tests
      bool operator== (nullptr_t) const { return p != nullptr; }
      template<typename U> friend bool operator== (nullptr_t, const unique_ptr<U>&);
      template<typename U, typename V> friend bool operator== (const unique_ptr<U>&, const unique_ptr<V>&);

   private:
      T *p{};
   };

   // Now specialize the class for array types
   template<typename T> class unique_ptr<T[]> {
   public:
      // Default constructor
      unique_ptr() {}

      // Explicit constructor from pointer
      explicit unique_ptr(T *p_) : p{ p_ } {}
      // NO template constructor for upcasting!

      // Copy is disallowed
      unique_ptr(const unique_ptr &) PROHIBITED;
      unique_ptr& operator= (const unique_ptr &)PROHIBITED;

      // But move is allowed!
      unique_ptr(unique_ptr &&that) : p{ that.release() } { }
      unique_ptr& operator= (unique_ptr &&that)
      {
         if (this != &that) {
            delete[] p;
            p = that.release();
         }
         return *this;
      }

      // Assign null
      unique_ptr& operator= (nullptr_t)
      {
         delete[] p;
         p = nullptr;
         return *this;
      }

      // NO template versions of move for upcasting!

      // delete[] not delete!
      ~unique_ptr() { delete[] p; }

      // No operator ->, but [] instead
      T& operator [] (ptrdiff_t n) const { return p[n]; }

      T& operator * () const { return *p; }
      T* get() const { return p; }

      // So you can say if(p)
      explicit operator bool() const { return p != nullptr; }

      // Give up ownership, don't destroy
      T* release() { T* result = p; p = nullptr; return result; }

      void reset(T* __p = nullptr)
      {
         T* old__p = p;
         p = __p;
         if (old__p != nullptr)
         {
            delete[] old__p;
         }
      }

      // Equality tests
      bool operator== (nullptr_t) const { return p != nullptr; }
      template<typename U> friend bool operator== (nullptr_t, const unique_ptr<U>&);
      template<typename U, typename V> friend bool operator== (const unique_ptr<U>&, const unique_ptr<V>&);

   private:
      T *p{};
   };

   // Equality operators for unique_ptr, don't need the specializations for array case
   template<typename U>
   inline bool operator== (nullptr_t, const unique_ptr<U>& ptr)
   {
      return ptr == nullptr;
   }

   template<typename U> inline bool operator != (nullptr_t, const unique_ptr<U> &ptr) { return !(ptr == nullptr); }
   template<typename U> inline bool operator != (const unique_ptr<U> &ptr, nullptr_t) { return !(ptr == nullptr); }
   template<typename U, typename V> inline bool operator != (const unique_ptr<U>& ptr1, const unique_ptr<V> &ptr2)
   { return !(ptr1 == ptr2); }

   // Forward -- pass along rvalue references as rvalue references, anything else as it is
   // (Because the appropriate overload is taken, and "reference collapse" applies to the return type)
   template<typename T> inline T&& forward(typename remove_reference<T>::type& t)
   { return static_cast<T&&>(t); }
   template<typename T> inline T&& forward(typename remove_reference<T>::type&& t)
   { return static_cast<T&&>(t); }

   // "Cast" anything as an rvalue reference.
   template<typename T> inline typename remove_reference<T>::type&& move(T&& t)
   { return static_cast<typename std::remove_reference<T>::type&&>(t); }

   // We need make_shared for ourselves, because the library doesn't use variadics
   template<typename X, typename... Args> inline shared_ptr<X> make_shared(Args&&... args)
   {
      return shared_ptr<X>{ safenew X(forward<Args>(args)...) };
   }
   // From LLVM c++11 and modified

   #include <cstddef>

   template<class _Ep>
   class initializer_list
   {
       const _Ep* __begin_;
       size_t    __size_;

       initializer_list(const _Ep* __b, size_t __s)
           : __begin_(__b),
             __size_(__s)
           {}
   public:
       typedef _Ep        value_type;
       typedef const _Ep& reference;
       typedef const _Ep& const_reference;
       typedef size_t    size_type;

       typedef const _Ep* iterator;
       typedef const _Ep* const_iterator;

       initializer_list() : __begin_(nullptr), __size_(0) {}

       size_t    size()  const {return __size_;}

       const _Ep* begin() const {return __begin_;}

       const _Ep* end()   const {return __begin_ + __size_;}
   };

   template<class _Ep>
   inline
   const _Ep*
   begin(initializer_list<_Ep> __il)
   {
       return __il.begin();
   }

   template<class _Ep>
   inline
   const _Ep*
   end(initializer_list<_Ep> __il)
   {
       return __il.end();
   }
}

#endif

#if !(_MSC_VER >= 1800 || __cplusplus >= 201402L)
/* replicate the very useful C++14 make_unique for those build environments
that don't implement it yet.
typical useage:
auto p = std::make_unique<Myclass>(ctorArg1, ctorArg2, ... ctorArgN);
p->DoSomething();
auto q = std::make_unique<Myclass[]>(count);
q[0].DoSomethingElse();

The first hides naked NEW and DELETE from the source code.
The second hides NEW[] and DELETE[].  Both of course ensure destruction if
you don't use something like std::move(p) or q.release().  Both expressions require
that you identify the type only once, which is brief and less error prone.

(Whereas this omission of [] might invite a runtime error:
std::unique_ptr<Myclass> q { new Myclass[count] }; )

Some C++11 tricks needed here are (1) variadic argument lists and
(2) making the compile-time dispatch work correctly.  You can't have
a partially specialized template function, but you get the effect of that
by other metaprogramming means.
*/

namespace std {
   // For overloading resolution
   template <typename X> struct __make_unique_result {
      using scalar_case = unique_ptr<X>;
   };

   // Partial specialization of the struct for array case
   template <typename X> struct __make_unique_result<X[]> {
      using array_case = unique_ptr<X[]>;
      using element = X;
   };

   // Now the scalar version of unique_ptr
   template<typename X, typename... Args> inline
      typename __make_unique_result<X>::scalar_case
      make_unique(Args&&... args)
   {
      return typename __make_unique_result<X>::scalar_case
      { safenew X(forward<Args>(args)...) };
   }

   // Now the array version of unique_ptr
   // The compile-time dispatch trick is that the non-existence
   // of the scalar_case type makes the above overload
   // unavailable when the template parameter is explicit
   template<typename X> inline
      typename __make_unique_result<X>::array_case
      make_unique(size_t count)
   {
      return typename __make_unique_result<X>::array_case
      { safenew typename __make_unique_result<X>::element[count] };
   }
}
#endif

/*
* template class Maybe<X>
* Can be used for monomorphic objects that are stack-allocable, but only conditionally constructed.
* You might also use it as a member.
* Initialize with create(), then use like a smart pointer,
* with *, ->, get(), reset(), or in if()
*/

// Placement-new is used below, and that does not cooperate with the DEBUG_NEW for Visual Studio
#ifdef _DEBUG
#ifdef _MSC_VER
#undef new
#endif
#endif

template<typename X>
class Maybe {
public:

   // Construct as NULL
   Maybe() {}

   // Supply the copy and move, so you might use this as a class member too
   Maybe(const Maybe &that)
   {
      if (that.get())
         create(*that);
   }

   Maybe& operator= (const Maybe &that)
   {
      if (this != &that) {
         if (that.get())
            create(*that);
         else
            reset();
      }
      return *this;
   }

   Maybe(Maybe &&that)
   {
      if (that.get())
         create(::std::move(*that));
   }

   Maybe& operator= (Maybe &&that)
   {
      if (this != &that) {
         if (that.get())
            create(::std::move(*that));
         else
            reset();
      }
      return *this;
   }

   // Make an object in the buffer, passing constructor arguments,
   // but destroying any previous object first
   // Note that if constructor throws, we remain in a consistent
   // NULL state -- giving exception safety but only weakly
   // (previous value was lost if present)
   template<typename... Args>
   void create(Args... args)
   {
      // Lose any old value
      reset();
      // Create new value
      pp = safenew(address()) X(std::forward<Args>(args)...);
   }

   // Destroy any object that was built in it
   ~Maybe()
   {
      reset();
   }

   // Pointer-like operators

   // Dereference, with the usual bad consequences if NULL
   X &operator* () const
   {
      return *pp;
   }

   X *operator-> () const
   {
      return pp;
   }

   X* get() const
   {
      return pp;
   }

   void reset()
   {
      if (pp)
         pp->~X(), pp = nullptr;
   }

   // So you can say if(ptr)
   explicit operator bool() const
   {
      return pp != nullptr;
   }

private:
   X* address()
   {
      return reinterpret_cast<X*>(&storage);
   }

   // Data
#if 0
   typename ::std::aligned_storage<
      sizeof(X)
      // , alignof(X) // Not here yet in all compilers
   >::type storage{};
#else
   union {
      double d;
      char storage[sizeof(X)];
   };
#endif
   X* pp{ nullptr };
};

// Restore definition of debug new
#ifdef _DEBUG
#ifdef _MSC_VER
#undef THIS_FILE
static char*THIS_FILE = __FILE__;
#define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
#endif
#endif

#endif // __AUDACITY_MEMORY_X_H__
