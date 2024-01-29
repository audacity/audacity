#ifndef __AUDACITY_MEMORY_X_H__
#define __AUDACITY_MEMORY_X_H__

// C++ standard header <memory> with a few extensions
#include <memory>
#include <cstdlib> // Needed for free.
#ifndef safenew
#define safenew new
#endif

#include <functional>
#include <limits>

/*
 * ArrayOf<X>
 * Not to be confused with std::array (which takes a fixed size) or std::vector
 * This maintains a pointer allocated by NEW X[].  It's cheap: only one pointer,
 * with no size and capacity information for resizing as for vector, and if X is
 * a built-in numeric or pointer type, by default there is no zero filling at
 * allocation time.
 */

template<typename X>
class ArrayOf : public std::unique_ptr<X[]>
{
public:
   ArrayOf() {}

   template<typename Integral>
   explicit ArrayOf(Integral count, bool initialize = false)
   {
      static_assert(std::is_unsigned<Integral>::value, "Unsigned arguments only");
      reinit(count, initialize);
   }

   //ArrayOf(const ArrayOf&) = delete;
   ArrayOf(const ArrayOf&) = delete;
   ArrayOf(ArrayOf&& that)
      : std::unique_ptr < X[] >
         (std::move((std::unique_ptr < X[] >&)(that)))
   {
   }
   ArrayOf& operator= (ArrayOf &&that)
   {
      std::unique_ptr<X[]>::operator=(std::move(that));
      return *this;
   }
   ArrayOf& operator= (std::unique_ptr<X[]> &&that)
   {
      std::unique_ptr<X[]>::operator=(std::move(that));
      return *this;
   }

   template< typename Integral >
   void reinit(Integral count,
               bool initialize = false)
   {
      static_assert(std::is_unsigned<Integral>::value, "Unsigned arguments only");
      if (initialize)
         // Initialize elements (usually, to zero for a numerical type)
         std::unique_ptr<X[]>::reset(safenew X[count]{});
      else
         // Avoid the slight initialization overhead
         std::unique_ptr<X[]>::reset(safenew X[count]);
   }
};

/**
  \class ArrayOf

  ArraysOf<X>

  \brief This simplifies arrays of arrays, each array separately allocated with NEW[]
  But it might be better to use std::Array<ArrayOf<X>, N> for some small constant N
  Or use just one array when sub-arrays have a common size and are not large.
 */
template<typename X>
class ArraysOf : public ArrayOf<ArrayOf<X>>
{
public:
   ArraysOf() {}

   template<typename Integral>
   explicit ArraysOf(Integral N)
      : ArrayOf<ArrayOf<X>>( N )
   {}

   template<typename Integral1, typename Integral2 >
   ArraysOf(Integral1 N, Integral2 M, bool initialize = false)
   : ArrayOf<ArrayOf<X>>( N )
   {
      static_assert(std::is_unsigned<Integral1>::value, "Unsigned arguments only");
      static_assert(std::is_unsigned<Integral2>::value, "Unsigned arguments only");
      for (size_t ii = 0; ii < N; ++ii)
         (*this)[ii] = ArrayOf<X>{ M, initialize };
   }

   //ArraysOf(const ArraysOf&) = delete;
   ArraysOf(const ArraysOf&) =delete;
   ArraysOf& operator= (ArraysOf&& that)
   {
      ArrayOf<ArrayOf<X>>::operator=(std::move(that));
      return *this;
   }

   template< typename Integral >
   void reinit(Integral count)
   {
      ArrayOf<ArrayOf<X>>::reinit( count );
   }

   template< typename Integral >
   void reinit(Integral count, bool initialize)
   {
      ArrayOf<ArrayOf<X>>::reinit( count, initialize );
   }

   template<typename Integral1, typename Integral2 >
   void reinit(Integral1 countN, Integral2 countM, bool initialize = false)
   {
      static_assert(std::is_unsigned<Integral1>::value, "Unsigned arguments only");
      static_assert(std::is_unsigned<Integral2>::value, "Unsigned arguments only");
      reinit(countN, false);
      for (size_t ii = 0; ii < countN; ++ii)
         (*this)[ii].reinit(countM, initialize);
   }
};

/**
  A deleter for pointers obtained with malloc
 */
struct freer { void operator() (void *p) const { free(p); } };

/**
  A useful alias for holding the result of malloc
 */
template< typename T >
using MallocPtr = std::unique_ptr< T, freer >;

/**
  A useful alias for holding the result of strup and similar
 */
template <typename Character = char>
using MallocString = std::unique_ptr< Character[], freer >;

/**
  \brief A deleter class to supply the second template parameter of unique_ptr for
  classes like wxWindow that should be sent a message called Destroy rather
  than be deleted directly
 */
template <typename T>
struct Destroyer {
   void operator () (T *p) const { if (p) p->Destroy(); }
};

/**
  \brief a convenience for using Destroyer
 */
template <typename T>
using Destroy_ptr = std::unique_ptr<T, Destroyer<T>>;

/**
  \brief "finally" as in The C++ Programming Language, 4th ed., p. 358
  Useful for defining ad-hoc RAII actions.
  typical usage:
  auto cleanup = finally([&]{ ... code; ... });
 */

// Construct this from any copyable function object, such as a lambda
template <typename F>
struct Finally {
   Finally(F f) : clean( f ) {}
   ~Finally() { clean(); }
   F clean;
};

/// \brief Function template with type deduction lets you construct Finally
/// without typing any angle brackets
template <typename F>
[[nodiscard]] Finally<F> finally (F f)
{
   return Finally<F>(f);
}

//! C++17 deduction guide allows even simpler syntax:
//! `Finally Do{[&]{ Stuff(); }};`
/*!
 Don't omit `Do` or some other variable name!  Otherwise, the execution of the
 body is immediate, not delayed to the end of the enclosing scope.
 */
template<typename F> Finally(F) -> Finally<F>;

#include <algorithm>

/**
  \brief Structure used by ValueRestorer 
 */
template< typename T >
struct RestoreValue {
   T oldValue;
   void operator () ( T *p ) const { if (p) *p = oldValue; }
};


/**
  \brief Set a variable temporarily in a scope
  */
template< typename T >
class ValueRestorer : public std::unique_ptr< T, RestoreValue<T> >
{
   using std::unique_ptr< T, RestoreValue<T> >::reset; // make private
   // But release() remains public and can be useful to commit a changed value
public:
   explicit ValueRestorer( T &var )
      : std::unique_ptr< T, RestoreValue<T> >( &var, { var } )
   {}
   explicit ValueRestorer( T &var, const T& newValue )
      : std::unique_ptr< T, RestoreValue<T> >( &var, { var } )
   { var = newValue; }
   ValueRestorer(ValueRestorer &&that)
      : std::unique_ptr < T, RestoreValue<T> > ( std::move(that) ) {};
   ValueRestorer & operator= (ValueRestorer &&that)
   {
      if (this != &that)
         std::unique_ptr < T, RestoreValue<T> >::operator=(std::move(that));
      return *this;
   }
};

/*!
 Like ValueRestorer but copy-constructible
 */
template< typename T >
struct CopyableValueRestorer {
   explicit CopyableValueRestorer(T& var)
      : pointer{ &var, RestoreValue<T>{ var } }
   {}
   CopyableValueRestorer(T& var, const T& newValue)
      : pointer{ &var, RestoreValue<T>{ var } }
   {
      var = newValue;
   }

   std::shared_ptr<T> pointer;
};

/// inline functions provide convenient parameter type deduction
template< typename T >
ValueRestorer< T > valueRestorer( T& var )
{ return ValueRestorer< T >{ var }; }

template< typename T >
ValueRestorer< T > valueRestorer( T& var, const T& newValue )
{ return ValueRestorer< T >{ var, newValue }; }

// These macros are used widely, so declared here.
#define QUANTIZED_TIME(time, rate) (floor(((double)(time) * (rate)) + 0.5) / (rate))
// dB - linear amplitude conversions
#define DB_TO_LINEAR(x) (pow(10.0, (x) / 20.0))
#define LINEAR_TO_DB(x) (20.0 * log10(x))

#define MAX_AUDIO (1. - 1./(1<<15))

#endif // __AUDACITY_MEMORY_X_H__
