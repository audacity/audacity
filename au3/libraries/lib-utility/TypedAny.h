/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file TypedAny.h
  @brief Wrapper class for std::any, making type distinctions

  Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_TYPED_ANY__
#define __AUDACITY_TYPED_ANY__

#include <any>
#include <type_traits>
#include <utility>

namespace audacity {
//! Generates distinct, non-interconvertible types wrapping std::any
/*!
 @tparam Tag discriminates generated classes; a "CRTP" parameter
 */
template<typename Tag> class TypedAny
{
    // Define a trait of argument packs
    // Primary template:
    template<typename ... Args> struct Good_args : std::true_type {};
    // Partial specialization:
    template<typename Arg> struct Good_args<Arg>: std::bool_constant< !std::is_base_of_v<TypedAny,
                                                                                         std::remove_const_t< std::remove_reference_t<Arg> >
                                                                                         > > {};
public:
    //! Constructor with arguments just as for std::any, but it is explicit
    template<typename ... Args,
             typename restriction = std::enable_if_t< Good_args<Args...>::value >
             >
    explicit TypedAny(Args&&... args)
        : mAny(std::forward<Args>(args)...)
    {}

    TypedAny(const TypedAny&) = default;
    TypedAny& operator=(const TypedAny&) = default;
    TypedAny(TypedAny&&) = default;
    TypedAny& operator=(TypedAny&&) = default;

    /*!
     @name Pass-through member functions

     Supply an imitation of std::any's interface, except for reference-valued
     any_cast
     @{
    */

    template<typename ValueType, typename ... Args>
    std::decay_t<ValueType>& emplace(Args&&... args)
    {
        return mAny.emplace<ValueType>(std::forward<Args>(args)...);
    }

    void reset() noexcept { mAny.reset(); }
    void swap(TypedAny& other) noexcept { mAny.swap(other.mAny); }
    bool has_value() const noexcept { return mAny.has_value(); }
    const std::type_info& type() const noexcept { return mAny.type(); }

    //! Like pointer-valued any_cast but a non-static member function
    template<typename T>
    const T* cast() const noexcept { return std::any_cast<T>(&mAny); }

    //! Like pointer-valued any_cast but a non-static member function
    template<typename T>
    T* cast() noexcept { return std::any_cast<T>(&mAny); }

    //! Like make_any but a static member function
    template<typename T, typename ... Args>
    static TypedAny make(Args&&... args)
    {
        return TypedAny(std::in_place_type<T>, std::forward<Args>(args)...);
    }

    //! @}

private:
    std::any mAny;
};

//! Non-member swap
template<typename Tag>
inline void swap(TypedAny<Tag>& x, TypedAny<Tag>& y) { x.swap(y); }
}

#endif
