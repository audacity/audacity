#ifndef __AUDACITY_MEMORY_X_H__
#define __AUDACITY_MEMORY_X_H__

// C++ standard header <memory> with a few extensions
#include <memory>

#if !(defined(__WXMSW__) || defined(__WXMAC__))
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
#undef new

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
   typename ::std::aligned_storage<
      sizeof(X)
      // , alignof(X) // Not here yet in all compilers
   >::type storage{};
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
