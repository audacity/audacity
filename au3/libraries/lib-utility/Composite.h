/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file Composite.h
 @brief Support for the Composite pattern

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_COMPOSITE__
#define __AUDACITY_COMPOSITE__

#include <algorithm>
#include <cassert>
#include <iterator>
#include <type_traits>
#include <utility>
#include <vector>

namespace Composite {
//! Generates a base class for internal nodes of tree structures, which acts
//! like a standard container (including compatibility with range-for and
//! back_inserter)
/*!
 @tparam Component common base class of nodes and leaves (and of Base itself),
 which must have a virtual destructor
 @tparam ComponentPointer type of pointer to Components to be stored
 @tparam ComponentArgs... passed to constructor of Component
 */
template<
    typename Component,
    typename ComponentPointer,
    typename ... ComponentArgs
    >
class Base : public Component
{
public:
    using value_type = ComponentPointer;
    using Items = std::vector<value_type>;

    explicit Base(ComponentArgs... args)
        : Component{std::forward<ComponentArgs>(args)...}
    {
        static_assert(std::has_virtual_destructor_v<Component>);
    }

    Base(const Base&) = delete;
    Base& operator =(const Base&) = delete;

    auto begin() const { return items.begin(); }
    auto end() const { return items.end(); }
    auto cbegin() const { return items.cbegin(); }
    auto cend() const { return items.cend(); }
    auto rbegin() const { return items.rbegin(); }
    auto rend() const { return items.rend(); }
    auto crbegin() const { return items.crbegin(); }
    auto crend() const { return items.crend(); }

    void push_back(value_type ptr) { items.push_back(move(ptr)); }
    auto size() const noexcept { return items.size(); }

    [[nodiscard]] bool empty() const { return items.empty(); }

protected:
    Items items;
};

//! You may specialize this template, defining ItemBuilder that converts
//! other types to Base::value_type
/*!
 A trait lets the Builder and Extension templates have parameter packs for other
 purposes.
 enables_item_type_v may be redefined to return false for certain types, which
 removes the overload resolution candidate for push_back taking those types.
 */
template<typename Base, typename Derived> struct Traits {
    static constexpr auto ItemBuilder = std::move<typename Base::value_type>;
    template<typename T> static constexpr auto enables_item_type_v = true;
};

namespace detail {
template<bool Deprecate> struct This;
template<> struct This<false> {};
template<> struct [[deprecated(
   "Composite::Builder specialization does not enable Base::value_type"
)]]
This<true> {};
}

//! Specialization of Traits can wrap the push_back of the base class
//! with a type-erasing gate-keeper, which can also do other value
//! transformations
/*!
 This class comes between Base and Derived in the inheritance graph
 */
template<typename Base, typename Derived, typename ... BaseArgs>
struct Builder : Base
{
    using BaseType = Base;

    //! Define a push_back, which makes this work also with back_inserter
    //! Traits can determine whether to enable the overloads
    template<typename Arg> auto push_back(Arg&& arg)
    -> std::enable_if_t<Traits<Base, Derived>
                        ::template enables_item_type_v<Arg>,
                        void
                        >
    {
        static constexpr auto ItemBuilder = Traits<Base, Derived>::ItemBuilder;
        Base::push_back(ItemBuilder(std::forward<Arg>(arg)));
    }

    //! This non-template overload is also needed so overload resolution does
    //! not select the private inherited function
    void push_back(typename Base::value_type arg)
    {
        constexpr auto enable = Traits<Base, Derived>
                                ::template enables_item_type_v<typename Base::value_type>;
        // We can't sfinae this overload away when traits want to disable it, but
        // we can make a deprecation
        static const detail::This<!enable> deprecator;
        if constexpr (!enable) {
            assert(false);
        } else {
            static constexpr auto ItemBuilder = Traits<Base, Derived>::ItemBuilder;
            Base::push_back(ItemBuilder(move(arg)));
        }
    }

    //! Variadic constructor
    template<typename ... Items>
    Builder(BaseArgs... args, Items&&... items)
        : Base{std::forward<BaseArgs>(args)...}
    {
        (..., push_back(std::forward<Items>(items)));
    }

    //! Iterator range constructor, with default transformer
    template<typename InputIterator,
             typename Arg = decltype(*std::declval<InputIterator>()),
             typename ItemBuilder = decltype(Traits<Base, Derived>::ItemBuilder),
             typename sfinae = std::enable_if_t<std::is_invocable_v<ItemBuilder, Arg> > >
    Builder(BaseArgs... args, InputIterator begin, InputIterator end)
        : Builder{
                  std::forward<BaseArgs>(args)..., begin, end,
                  [](Arg&& arg) -> decltype(auto) { return std::forward<Arg>(arg); }
                  }
    {}

    //! Iterator range constructor, with explicit transformer
    template<typename InputIterator, typename Transformer,
             typename Arg = decltype(*std::declval<InputIterator>()),
             typename ItemBuilder = decltype(Traits<Base, Derived>::ItemBuilder),
             typename sfinae = std::enable_if_t<std::is_invocable_v<
                                                    ItemBuilder, std::invoke_result_t<Transformer, Arg> > > >
    Builder(BaseArgs... args,
            InputIterator begin, InputIterator end, Transformer transformer)
        : Base{std::forward<BaseArgs>(args)...}
    {
        std::for_each(begin, end,
                      [this, &transformer](Arg&& arg){ push_back(transformer(arg)); });
    }

private:
    using BaseType::push_back;
};

//! Extend Base with extra fields, in a second, protected base class
template<
    typename Base,
    typename Base2,
    typename ... RequiredBaseArgs
    >
struct Extension : public Base, protected Base2
{
    template<typename ... OtherBaseArgs>
    Extension(RequiredBaseArgs... args, Base2 arg2,
              OtherBaseArgs&&... otherArgs)
        : Base{std::forward<RequiredBaseArgs>(args)...,
               std::forward<OtherBaseArgs>(otherArgs)...
               }
        , Base2{std::move(arg2)}
    {}
};

//! Specialization when there is no need for the second base
template<
    typename Base,
    typename ... RequiredBaseArgs
    > struct Extension<
    Base,
    void,
    RequiredBaseArgs...
    >  : public Base
{
    template<typename ... OtherBaseArgs>
    Extension(RequiredBaseArgs... args, OtherBaseArgs&&... otherArgs)
        : Base{std::forward<RequiredBaseArgs>(args)...,
               std::forward<OtherBaseArgs>(otherArgs)...
               }
    {}
};
}

#endif
