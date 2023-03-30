/*!********************************************************************
 
 Audacity: A Digital Audio Editor
 
 @file Variant.h

 @brief emulates std::visit for one visitor
 
 Paul Licameli split from MemoryX.h
 
 **********************************************************************/
#ifndef __AUDACITY_VARIANT__
#define __AUDACITY_VARIANT__

#include <type_traits>
#include <variant>
#include <stdexcept>

//! Help to define Visit() below
template <typename Visitor, typename Variant>
struct VisitHelperReturn {
   using Var = std::remove_reference_t<Variant>;
   using Alt = std::variant_alternative_t<0, Var>;
   using QAlt = std::conditional_t<
      std::is_const_v<Var>, const Alt, Alt >;
   using Arg = std::conditional_t<std::is_lvalue_reference_v<Variant>,
      std::add_lvalue_reference_t<QAlt>, std::add_rvalue_reference_t<QAlt>
   >;
   // All this just so that the noreturn function below has an appropriate type
   using type = decltype( std::invoke(
     std::forward<Visitor>( std::declval<Visitor>() ),
     std::declval<Arg>() ) );
};

//! Help to define Visit() below
template <typename Visitor, typename Variant>
[[noreturn]] auto VisitHelper(Visitor &&, Variant &&)
   -> typename VisitHelperReturn<Visitor, Variant>::type
{
   // Fall through here when the variant holds no value
   // Should really throw std::bad_variant_access but that may not be available
   throw std::invalid_argument{"Bad variant"};
}

//! Help to define Visit() below
template <size_t Index, size_t... Indices, typename Visitor, typename Variant>
auto VisitHelper(Visitor &&vis, Variant &&var)
{
   // Invoke vis at most once after no-throw testing for presence of
   // alternatives in the variant
   if (const auto pValue = std::get_if<Index>(&var)) {
      if constexpr (std::is_lvalue_reference_v<Variant>)
         return std::invoke( std::forward<Visitor>(vis), (*pValue) );
      else
         return std::invoke( std::forward<Visitor>(vis), std::move(*pValue) );
   }
   // Recur down the index value pack
   return VisitHelper<Indices...>(
      std::forward<Visitor>(vis), std::forward<Variant>(var));
}

//! Help to define Visit() below
template <size_t... Indices, typename Visitor, typename Variant>
auto VisitHelper(std::index_sequence<Indices...>, Visitor &&vis, Variant &&var)
{
   // Non-template parameters were deduced and are passed on as non-deduced
   return VisitHelper<Indices...>(
      std::forward<Visitor>(vis), std::forward<Variant>(var) );
}

//! Mimic some of std::visit, for the case of one visitor only
/*! This is necessary because of limitations of the macOS implementation of
 some of the C++17 standard library without a minimum version of 10.13, and
 so let's use this even when not needed on the other platforms, instead of having
 too much conditional compilation
 */
template <typename Visitor, typename Variant>
auto Visit(Visitor &&vis, Variant &&var)
{
   constexpr auto size = std::variant_size_v<std::remove_reference_t<Variant>>;
   return VisitHelper( std::make_index_sequence<size>{},
      std::forward<Visitor>(vis), std::forward<Variant>(var) );
}
#endif
