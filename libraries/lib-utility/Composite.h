/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Composite.h
 @brief Support for the Composite pattern
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_COMPOSITE__
#define __AUDACITY_COMPOSITE__

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
   typename... ComponentArgs
>
class Base : public Component
{
public:
   using value_type = ComponentPointer;
   using Items = std::vector<value_type>;

   explicit Base(ComponentArgs... args)
      : Component{ std::forward<ComponentArgs>(args)... }
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

   void push_back(value_type ptr){ items.push_back(move(ptr)); }

   [[nodiscard]] bool empty() const { return items.empty(); }

protected:
   Items items;
};

//! Extend Base with extra fields, in a second, protected base class
template<
   typename Base,
   typename Base2,
   typename... RequiredBaseArgs
>
struct Extension
   : public Base
   , protected Base2
{
   template<typename... OtherBaseArgs>
   Extension(RequiredBaseArgs... args, Base2 arg2,
      OtherBaseArgs &&...otherArgs
   )  : Base{ std::forward<RequiredBaseArgs>(args)...,
         std::forward<OtherBaseArgs>(otherArgs)...
      }
      , Base2{ std::move(arg2) }
   {}
};

//! Specialization when there is no need for the second base
template<
   typename Base,
   typename... RequiredBaseArgs
> struct Extension<
   Base,
   void,
   RequiredBaseArgs...
>
   : public Base
{
   template<typename... OtherBaseArgs>
   Extension(RequiredBaseArgs... args, OtherBaseArgs &&...otherArgs)
      : Base{ std::forward<RequiredBaseArgs>(args)...,
         std::forward<OtherBaseArgs>(otherArgs)...
      }
   {}
};

}

#endif
