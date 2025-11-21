/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file PackedArray.h

 @brief Smart pointer for a header contiguous with an array holding a
 dynamically determined number of elements

 Paul Licameli

 **********************************************************************/

#ifndef __AUDACITY_PACKED_ARRAY__
#define __AUDACITY_PACKED_ARRAY__

#include <type_traits>

namespace PackedArray {
template<typename T> struct Traits;

namespace detail {
//! Primary template of metafunction deducing other things from Traits<T>
template<typename T, typename = void> struct ExtendedTraits : Traits<T> {
    using element_type = typename Traits<T>::element_type;
    using iterated_type = T;
    static iterated_type* begin(T* p) { return p; }
    static element_type* element_ptr(T* p) { return p; }
    static size_t element_offset() { return 0; }
};

//! Partial specialization used when Traits<T>::array_member is defined
template<typename T> struct ExtendedTraits<T, std::void_t<decltype(
                                                              std::declval<T>().*(Traits<T>::array_member)
                                                              )> > : Traits<T> {
    using element_type = typename Traits<T>::element_type;
    using member_type = std::remove_reference_t<decltype(
                                                    std::declval<T>().*(Traits<T>::array_member)
                                                    )>;
    // Check restrictions on usage.  The member is expected to have dimension 1
    static_assert(std::extent_v<member_type> == 1);
    using iterated_type = std::remove_extent_t<member_type>;
    // Check that any overlay of a type with a destructor is sensible
    static_assert(sizeof(iterated_type) == sizeof(element_type));

    static iterated_type* begin(T* p)
    {
        return &(p->*(Traits<T>::array_member))[0];
    }

    static element_type* element_ptr(T* p)
    {
        return reinterpret_cast<element_type*>(begin(p));
    }

    static size_t element_offset()
    {
        return reinterpret_cast<size_t>(begin(0));
    }
};
}

//! Primary template used in Deleter that can be specialized
/*!
 Specializations that define a pointer-to-data-member of T called array_member
 are treated specially
 */
template<typename T> struct Traits {
    // Default assumption is that there is no header and no need to overlay
    // the element with a type that performs nontrivial destruction
    struct header_type {};
    using element_type = T;
};

//! Deleter for an array of elements and optional contiguous header structure
/*!
 @tparam Type is the type pointed to, and an explicit specialization of
 Traits<Type> may redefine the nested types for a non-empty header:
  - header_type, which overlays the members of Type except the last member;
   may be non-empty and define a destructor
  - element_type must be such that the last member of Type is Element[1]; may
    define a destructor
 And Traits<Type> may also define a pointer-to-data-member of T called
 array_member
 @tparam BaseDeleter manages the main deallocation
 */
template<typename Type,
         template<typename> typename BaseDeleter = std::default_delete>
struct Deleter : BaseDeleter<Type> {
    using managed_type = Type;
    using base_type = BaseDeleter<managed_type>;
    using traits_type = detail::ExtendedTraits<managed_type>;
    using header_type = typename traits_type::header_type;
    using element_type = typename traits_type::element_type;

    //! Nontrivial, implicit constructor of the deleter takes a size, which
    //! defaults to 0 to allow default contruction of the unique_ptr
    Deleter(size_t size = 0) noexcept
        : mCount{size
                 ? (size - traits_type::element_offset()) / sizeof(element_type)
                 : 0
                 }
    {}

    void operator()(Type* p) const noexcept(noexcept(
                                                (void) std::declval<element_type>().~element_type(),
                                                (void) std::declval<header_type>().~header_type(),
                                                (void) std::declval<base_type>()(p)
                                                ))
    {
        if (!p) {
            return;
        }
        // Do nested deallocations for elements by decreasing subscript
        auto pE = traits_type::element_ptr(p) + mCount;
        for (auto count = mCount; count--;) {
            (--pE)->~element_type();
        }
        // Do nested deallocations for main structure
        reinterpret_cast<const header_type*>(p)->~header_type();
        // main deallocation
        ((base_type&)*this)(p);
    }

    size_t GetCount() const { return mCount; }
private:
    size_t mCount = 0;
};

//! Smart pointer type that deallocates with Deleter
template<typename Type,
         template<typename> typename BaseDeleter = std::default_delete>
struct Ptr : std::unique_ptr<Type, Deleter<Type, BaseDeleter> >
{
    using
    std::unique_ptr<Type, Deleter<Type, BaseDeleter> >::unique_ptr;

    //! Enables subscripting.  Does not check for null!
    auto& operator[](size_t ii) const
    {
        return *(begin(*this) + ii);
    }
};

//! Find out how many elements were allocated with a Ptr
template<typename Type, template<typename> typename BaseDeleter>
inline size_t Count(const Ptr<Type, BaseDeleter>& p)
{
    return p.get_deleter().GetCount();
}

//! Enables range-for
template<typename Type, template<typename> typename BaseDeleter>
inline auto begin(const Ptr<Type, BaseDeleter>& p)
{
    using traits_type = detail::ExtendedTraits<Type>;
    auto ptr = p.get();
    return ptr ? traits_type::begin(ptr) : nullptr;
}

//! Enables range-for
template<typename Type, template<typename> typename BaseDeleter>
inline auto end(const Ptr<Type, BaseDeleter>& p)
{
    auto result = begin(p);
    if (result) {
        result += Count(p);
    }
    return result;
}
}

struct PackedArray_t {};
//! Tag argument to distinguish an overload of ::operator new
static constexpr inline PackedArray_t PackedArrayArg;

//! Allocator for objects managed by PackedArray::Ptr enlarges the size request
//! that the compiler makes
inline void* operator new(size_t size, PackedArray_t, size_t enlarged)
{
    return ::operator new(std::max(size, enlarged));
}

//! Complementary operator delete in case of exceptions in a new-expression
inline void operator delete(void* p, PackedArray_t, size_t)
{
    ::operator delete(p);
}

namespace PackedArray {
//! Allocate a Ptr<Type> holding at least `enlarged` bytes
/*!
 Usage: AllocateBytes<Type>(bytes)(constructor arguments for Type)
 Meant to resemble placement-new syntax with separation of allocator and
 constructor arguments
 */
template<typename Type> auto AllocateBytes(size_t enlarged)
{
    return [enlarged](auto&&... args) {
        return Ptr<Type> {
            safenew(PackedArrayArg, enlarged)
            Type{ std::forward<decltype(args)>(args)... },
            std::max(sizeof(Type), enlarged) // Deleter's constructor argument
        };
    };
}

//! Allocate a Ptr<Type> holding `count` elements
/*!
 Usage: AllocateCount<Type>(count)(constructor arguments for Type)
 Meant to resemble placement-new syntax with separation of allocator and
 constructor arguments

 If Traits<Type> defines a nonempty header, the result always holds
 at least one element
 */
template<typename Type> auto AllocateCount(size_t count)
{
    using traits_type = detail::ExtendedTraits<Type>;
    const auto bytes = traits_type::element_offset()
                       + count * sizeof(typename traits_type::element_type);
    return AllocateBytes<Type>(bytes);
}
}

#endif
