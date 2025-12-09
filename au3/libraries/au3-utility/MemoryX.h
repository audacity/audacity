#ifndef __AUDACITY_MEMORY_X_H__
#define __AUDACITY_MEMORY_X_H__

// C++ standard header <memory> with a few extensions
#include <memory>
#include <new> // align_val_t and hardware_destructive_interference_size
#include <cstdlib> // Needed for free.
#include <cmath>
#ifndef safenew
#define safenew new
#endif

#include <functional>
#include <limits>

#include <cstdint>

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

    ArrayOf& operator=(ArrayOf&& that)
    {
        std::unique_ptr<X[]>::operator=(std::move(that));
        return *this;
    }

    ArrayOf& operator=(std::unique_ptr<X[]>&& that)
    {
        std::unique_ptr<X[]>::operator=(std::move(that));
        return *this;
    }

    template< typename Integral >
    void reinit(Integral count,
                bool initialize = false)
    {
        static_assert(std::is_unsigned<Integral>::value, "Unsigned arguments only");
        if (initialize) {
            // Initialize elements (usually, to zero for a numerical type)
            std::unique_ptr<X[]>::reset(safenew X[count]{});
        } else {
            // Avoid the slight initialization overhead
            std::unique_ptr<X[]>::reset(safenew X[count]);
        }
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
class ArraysOf : public ArrayOf<ArrayOf<X> >
{
public:
    ArraysOf() {}

    template<typename Integral>
    explicit ArraysOf(Integral N)
        : ArrayOf<ArrayOf<X> >(N)
    {}

    template<typename Integral1, typename Integral2 >
    ArraysOf(Integral1 N, Integral2 M, bool initialize = false)
        : ArrayOf<ArrayOf<X> >(N)
    {
        static_assert(std::is_unsigned<Integral1>::value, "Unsigned arguments only");
        static_assert(std::is_unsigned<Integral2>::value, "Unsigned arguments only");
        for (size_t ii = 0; ii < N; ++ii) {
            (*this)[ii] = ArrayOf<X> { M, initialize };
        }
    }

    //ArraysOf(const ArraysOf&) = delete;
    ArraysOf(const ArraysOf&) = delete;
    ArraysOf& operator=(ArraysOf&& that)
    {
        ArrayOf<ArrayOf<X> >::operator=(std::move(that));
        return *this;
    }

    template< typename Integral >
    void reinit(Integral count)
    {
        ArrayOf<ArrayOf<X> >::reinit(count);
    }

    template< typename Integral >
    void reinit(Integral count, bool initialize)
    {
        ArrayOf<ArrayOf<X> >::reinit(count, initialize);
    }

    template<typename Integral1, typename Integral2 >
    void reinit(Integral1 countN, Integral2 countM, bool initialize = false)
    {
        static_assert(std::is_unsigned<Integral1>::value, "Unsigned arguments only");
        static_assert(std::is_unsigned<Integral2>::value, "Unsigned arguments only");
        reinit(countN, false);
        for (size_t ii = 0; ii < countN; ++ii) {
            (*this)[ii].reinit(countM, initialize);
        }
    }
};

/**
  A deleter for pointers obtained with malloc
 */
struct freer {
    void operator()(void* p) const
    {
        free(p);
    }
};

/**
  A useful alias for holding the result of malloc
 */
template< typename T >
using MallocPtr = std::unique_ptr< T, freer >;

/**
  A useful alias for holding the result of strup and similar
 */
template<typename Character = char>
using MallocString = std::unique_ptr< Character[], freer >;

/**
  \brief A deleter class to supply the second template parameter of unique_ptr for
  classes like wxWindow that should be sent a message called Destroy rather
  than be deleted directly
 */
template<typename T>
struct Destroyer {
    void operator ()(T* p) const
    {
        if (p) {
            p->Destroy();
        }
    }
};

/**
  \brief a convenience for using Destroyer
 */
template<typename T>
using Destroy_ptr = std::unique_ptr<T, Destroyer<T> >;

/**
  \brief "finally" as in The C++ Programming Language, 4th ed., p. 358
  Useful for defining ad-hoc RAII actions.
  typical usage:
  auto cleanup = finally([&]{ ... code; ... });
 */

// Construct this from any copyable function object, such as a lambda
template<typename F>
struct Finally {
    Finally(F f)
        : clean(f) {}
    ~Finally() { clean(); }
    F clean;
};

/// \brief Function template with type deduction lets you construct Finally
/// without typing any angle brackets
template<typename F>
[[nodiscard]] Finally<F> finally(F f)
{
    return Finally<F>(f);
}

//! C++17 deduction guide allows even simpler syntax:
//! `Finally Do{[&]{ Stuff(); }};`
/*!
 Don't omit `Do` or some other variable name!  Otherwise, the execution of the
 body is immediate, not delayed to the end of the enclosing scope.
 */
template<typename F> Finally(F)->Finally<F>;

#include <algorithm>

/**
  \brief Structure used by ValueRestorer
 */
template< typename T >
struct RestoreValue {
    T oldValue;
    void operator ()(T* p) const
    {
        if (p) {
            *p = oldValue;
        }
    }
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
    explicit ValueRestorer(T& var)
        : std::unique_ptr< T, RestoreValue<T> >(&var, { var })
    {}
    explicit ValueRestorer(T& var, const T& newValue)
        : std::unique_ptr< T, RestoreValue<T> >(&var, { var })
    { var = newValue; }
    ValueRestorer(ValueRestorer&& that)
        : std::unique_ptr < T, RestoreValue<T> >(std::move(that)) {}
    ValueRestorer& operator=(ValueRestorer&& that)
    {
        if (this != &that) {
            std::unique_ptr < T, RestoreValue<T> >::operator=(std::move(that));
        }
        return *this;
    }
};

/*!
 Like ValueRestorer but copy-constructible
 */
template< typename T >
struct CopyableValueRestorer {
    explicit CopyableValueRestorer(T& var)
        : pointer{&var, RestoreValue<T> { var }}
    {}
    CopyableValueRestorer(T& var, const T& newValue)
        : pointer{&var, RestoreValue<T> { var }}
    {
        var = newValue;
    }

    std::shared_ptr<T> pointer;
};

/// inline functions provide convenient parameter type deduction
template< typename T >
ValueRestorer< T > valueRestorer(T& var)
{ return ValueRestorer< T > { var }; }

template< typename T >
ValueRestorer< T > valueRestorer(T& var, const T& newValue)
{ return ValueRestorer< T > { var, newValue }; }

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
    static void* operator new(std::size_t count, std::align_val_t al);
    static void operator delete(void* ptr, std::align_val_t al);

#if defined (_MSC_VER) && defined(_DEBUG)
    // Versions that work in the presence of the DEBUG_NEW macro.
    // Ignore the arguments supplied by the macro and forward to the
    // other overloads.
    static void* operator new(
        std::size_t count, std::align_val_t al, int, const char*, int)
    { return operator new(count, al); }
    static void operator delete(
        void* ptr, std::align_val_t al, int, const char*, int)
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
    template<typename ... Args>
    static std::shared_ptr<T> make_shared(Args&&... args)
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
template< typename T > struct NonInterfering : NonInterferingBase, // Inherit operators; use empty base class optimization
                                               T
{
    using T::T;

    //! Allow assignment from default-aligned base type
    void Set(const T& other)
    {
        T::operator =(other);
    }

    //! Allow assignment from default-aligned base type
    void Set(T&& other)
    {
        T::operator =(std::move(other));
    }
};

// These macros are used widely, so declared here.
#define QUANTIZED_TIME(time, rate) (floor(((double)(time) * (rate)) + 0.5) / (rate))
// dB - linear amplitude conversions
#define DB_TO_LINEAR(x) (pow(10.0, (x) / 20.0))
#define LINEAR_TO_DB(x) (20.0 * log10(x))

#define MAX_AUDIO (1. - 1. / (1 << 15))

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
    void reset(T* p = nullptr)
    {
        delete this->exchange(p, std::memory_order_release);
    }

    //! reset to a pointer to a new object with given ctor arguments
    template<typename ... Args> void emplace(Args&&... args)
    {
        reset(safenew T(std::forward<Args>(args)...));
    }

    ~AtomicUniquePointer() { reset(); }
private:
    //! Disallow pointer arithmetic
    using std::atomic<T*>::fetch_add;
    using std::atomic<T*>::fetch_sub;
};

//! Check that machine is little-endian
inline bool IsLittleEndian() noexcept
{
    const std::uint32_t x = 1u;
    return static_cast<const unsigned char*>(static_cast<const void*>(&x))[0];
    // We will assume the same for other widths!
}

//! Swap bytes in an integer
template<typename IntType>
constexpr IntType SwapIntBytes(IntType value) noexcept
{
    static_assert(std::is_integral<IntType>::value, "Integral type required");

    constexpr auto size = sizeof(IntType);

    static_assert(
        size == 1 || size == 2 || size == 4 || size == 8, "Unsupported size");

    if constexpr (size == 1) {
        return value;
    }

    if constexpr (size == 2) {
        return (value >> 8) | (value << 8);
    }

    if constexpr (size == 4) {           // On x86, this (and 64 bit version) is a single instruction! (At least, clang is smart enough to do that)
        return ((value >> 24) & 0xFF) | ((value >> 8) & 0xFF00)
               | ((value << 8) & 0xFF0000) | ((value << 24) & 0xFF000000);
    }

    if constexpr (size == 8) {
        return ((value >> 56) & 0xFF) | ((value >> 40) & 0xFF00)
               | ((value >> 24) & 0xFF0000) | ((value >> 8) & 0xFF000000)
               | ((value << 8) & 0xFF00000000) | ((value << 24) & 0xFF0000000000)
               | ((value << 40) & 0xFF000000000000)
               | ((value << 56) & 0xFF00000000000000);
    }

    // Unreachable
    return value;
}

#endif // __AUDACITY_MEMORY_X_H__
