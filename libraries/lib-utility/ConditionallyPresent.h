/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file ConditionallyPresent.h

 @brief Useful to control static construction of objects, conditionally
 on a compile-time boolean value
 
 Paul Licameli
 
 **********************************************************************/
#ifndef __AUDACITY_CONDITIONALLY_PRESENT__
#define __AUDACITY_CONDITIONALLY_PRESENT__

#include <optional>
#include <type_traits>

//! Primary template ignores constructor arguments (though they are evaluated)
//! and constructs nullopt
template<typename T, bool = false>
struct ConditionallyPresent : std::optional<T>
{
   template <typename... Args,
      typename sfinae = std::enable_if<std::is_constructible_v<T, Args...>>
   >
   constexpr ConditionallyPresent(const Args &...) {}
};

//! Specialization for true passes constructor arguments to emplace an object
template<typename T>
struct ConditionallyPresent<T, true> : std::optional<T>
{
public:
   template <typename... Args>
   constexpr ConditionallyPresent(Args &&...args)
      // std::in_place is needed to construct a value, in case there are no
      // args
      : std::optional<T>(std::in_place, std::forward<Args>(args)...)
   {}
};

#endif
