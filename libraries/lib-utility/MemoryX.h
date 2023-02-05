#ifndef __AUDACITY_MEMORY_X_H__
#define __AUDACITY_MEMORY_X_H__

// C++ standard header <memory> with a few extensions
#include <iterator>
#include <memory>
#include <new> // align_val_t and hardware_destructive_interference_size
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

/**
  \brief A convenience for defining iterators that return rvalue types, so that
  they cooperate correctly with stl algorithms and std::reverse_iterator
 */
template< typename Value, typename Category = std::forward_iterator_tag >
struct ValueIterator{
   using iterator_category = Category;
   using value_type = Value;
   using difference_type = ptrdiff_t;
   // void pointer type so that operator -> is disabled
   using pointer = void;
   // make "reference type" really the same as the value type
   using reference = const Value;
};

/**
  \brief A convenience for use with range-for
 */
template <typename Iterator>
struct IteratorRange : public std::pair<Iterator, Iterator> {
   using iterator = Iterator;
   using reverse_iterator = std::reverse_iterator<Iterator>;

   IteratorRange (const Iterator &a, const Iterator &b)
   : std::pair<Iterator, Iterator> ( a, b ) {}

   IteratorRange (Iterator &&a, Iterator &&b)
   : std::pair<Iterator, Iterator> ( std::move(a), std::move(b) ) {}

   IteratorRange< reverse_iterator > reversal () const
   { return { this->rbegin(), this->rend() }; }

   Iterator begin() const { return this->first; }
   Iterator end() const { return this->second; }

   reverse_iterator rbegin() const { return reverse_iterator{ this->second }; }
   reverse_iterator rend() const { return reverse_iterator{ this->first }; }

   bool empty() const { return this->begin() == this->end(); }
   explicit operator bool () const { return !this->empty(); }
   size_t size() const { return std::distance(this->begin(), this->end()); }

   template <typename T> iterator find(const T &t) const
   { return std::find(this->begin(), this->end(), t); }

   template <typename T> long index(const T &t) const
   {
      auto iter = this->find(t);
      if (iter == this->end())
         return -1;
      return std::distance(this->begin(), iter);
   }

   template <typename T> bool contains(const T &t) const
   { return this->end() != this->find(t); }

   template <typename F> iterator find_if(const F &f) const
   { return std::find_if(this->begin(), this->end(), f); }

   template <typename F> long index_if(const F &f) const
   {
      auto iter = this->find_if(f);
      if (iter == this->end())
         return -1;
      return std::distance(this->begin(), iter);
   }

   // to do: use std::all_of, any_of, none_of when available on all platforms
   template <typename F> bool all_of(const F &f) const
   {
      auto notF =
         [&](typename std::iterator_traits<Iterator>::reference v)
            { return !f(v); };
      return !this->any_of( notF );
   }

   template <typename F> bool any_of(const F &f) const
   { return this->end() != this->find_if(f); }

   template <typename F> bool none_of(const F &f) const
   { return !this->any_of(f); }

   template<typename T> struct identity
      { const T&& operator () (T &&v) const { return std::forward(v); } };

   // Like std::accumulate, but the iterators implied, and with another
   // unary operation on the iterator value, pre-composed
   template<
      typename R,
      typename Binary = std::plus< R >,
      typename Unary = identity< decltype( *std::declval<Iterator>() ) >
   >
   R accumulate(
      R init,
      Binary binary_op = {},
      Unary unary_op = {}
   ) const
   {
      R result = init;
      for (auto&& v : *this)
         result = binary_op(result, unary_op(v));
      return result;
   }

   // An overload making it more convenient to use with pointers to member
   // functions
   template<
      typename R,
      typename Binary = std::plus< R >,
      typename R2, typename C
   >
   R accumulate(
      R init,
      Binary binary_op,
      R2 (C :: * pmf) () const
   ) const
   {
      return this->accumulate( init, binary_op, std::mem_fn( pmf ) );
   }

   // Some accumulations frequent enough to be worth abbreviation:
   template<
      typename Unary = identity< decltype( *std::declval<Iterator>() ) >,
      typename R = decltype( std::declval<Unary>()( *std::declval<Iterator>() ) )
   >
   R min( Unary unary_op = {} ) const
   {
      return this->accumulate(
         std::numeric_limits< R >::max(),
         (const R&(*)(const R&, const R&)) std::min,
         unary_op
      );
   }

   template<
      typename R2, typename C,
      typename R = R2
   >
   R min( R2 (C :: * pmf) () const ) const
   {
      return this->min( std::mem_fn( pmf ) );
   }

   template<
      typename Unary = identity< decltype( *std::declval<Iterator>() ) >,
      typename R = decltype( std::declval<Unary>()( *std::declval<Iterator>() ) )
   >
   R max( Unary unary_op = {} ) const
   {
      return this->accumulate(
         std::numeric_limits< R >::lowest(),
         (const R&(*)(const R&, const R&)) std::max,
         unary_op
      );
   }

   template<
      typename R2, typename C,
      typename R = R2
   >
   R max( R2 (C :: * pmf) () const ) const
   {
      return this->max( std::mem_fn( pmf ) );
   }

   template<
      typename Unary = identity< decltype( *std::declval<Iterator>() ) >,
      typename R = decltype( std::declval<Unary>()( *std::declval<Iterator>() ) )
   >
   R sum( Unary unary_op = {} ) const
   {
      return this->accumulate(
         R{ 0 },
         std::plus< R >{},
         unary_op
      );
   }

   template<
      typename R2, typename C,
      typename R = R2
   >
   R sum( R2 (C :: * pmf) () const ) const
   {
      return this->sum( std::mem_fn( pmf ) );
   }
};

template< typename Iterator>
IteratorRange< Iterator >
make_iterator_range( const Iterator &i1, const Iterator &i2 )
{
   return { i1, i2 };
}

template< typename Container >
IteratorRange< typename Container::iterator >
make_iterator_range( Container &container )
{
   return { container.begin(), container.end() };
}

template< typename Container >
IteratorRange< typename Container::const_iterator >
make_iterator_range( const Container &container )
{
   return { container.begin(), container.end() };
}

// A utility function building a container of results
template< typename Container, typename Iterator, typename Function >
Container transform_range( Iterator first, Iterator last, Function &&fn )
{
   Container result;
   std::transform( first, last, std::back_inserter( result ), fn );
   return result;
}
// A utility function, often constructing a vector from another vector
template< typename OutContainer, typename InContainer, typename Function >
OutContainer transform_container( InContainer &inContainer, Function &&fn )
{
   return transform_range<OutContainer>(
      inContainer.begin(), inContainer.end(), fn );
}

//! Non-template helper for class template NonInterfering
/*!
 If a structure contains any members with large alignment, this base class may also allow it to work in
 macOS builds under current limitations of the C++17 standard implementation.
 */
struct UTILITY_API alignas(
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
   T& Assign(const T &other)
   {
      return T::operator =(other);
   }

   //! Allow assignment from default-aligned base type
   T& Assign(T &&other)
   {
      return T::operator =(std::move(other));
   }
};

// These macros are used widely, so declared here.
#define QUANTIZED_TIME(time, rate) (floor(((double)(time) * (rate)) + 0.5) / (rate))
// dB - linear amplitude conversions
#define DB_TO_LINEAR(x) (pow(10.0, (x) / 20.0))
#define LINEAR_TO_DB(x) (20.0 * log10(x))

#define MAX_AUDIO (1. - 1./(1<<15))

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

#endif // __AUDACITY_MEMORY_X_H__
