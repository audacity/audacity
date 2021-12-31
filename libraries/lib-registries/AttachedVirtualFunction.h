/*!********************************************************************

Audacity: A Digital Audio Editor

@file AttachedVirtualFunction.h
@brief Utility for non-intrusive definition of a new method on a base class

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_ATTACHED_VIRTUAL_FUNCTION__
#define __AUDACITY_ATTACHED_VIRTUAL_FUNCTION__


#include <functional>
#include <mutex>
#include <type_traits>
#include <utility>
#include "InconsistencyException.h"

//! Class template generates single-dispatch, open method registry tables
/*!
Defines a "virtual" function with multiple bodies chosen by type-switch
on the runtime class of the first argument, leaving the set of overrides
open-ended for extension, but also requiring no modification of the definition
of the base class of the type hierarchy that is switched on.

The invocation of the function is not as efficient as for true virtual functions
but the advantage of this utility is greater compilation decoupling.  Client code
can attach its own virtual functions, non-intrusively, to the root of a class hierarchy
defined in the core.

There is no collection of overriding function pointers into virtual function tables by the
linker, but a registration of overrides into a table at static initialization time.  This allows
those implementations to be defined wherever is convenient, even in dynamically loaded
libraries.

Beware that invocation of the function should not be done during initialization
of file scope static objects.  Dispatch might not go to the correct subclass
case if initializations are not yet complete.

Example usage:

A core class:
```
// file Host.h
class AbstractHost
{
   // ...
};
```

Declare the attached function
(in a header that Host.cpp need not include at all)
as a specialization of the template:
```
// file Client.h
enum class ErrorCode { Ok, Bad, // ...
}; // a return type for our function

// First this empty structure serving just to distinguish among instantiations
// of AttachedVirtualFunction with otherwise identical parameters
// An incomplete type is enough
struct DoSomethingTag;

// Now declare the "virtual function"
using DoSomething =
AttachedVirtualFunction<
   DoSomethingTag,
   ErrorCode,
   AbstractHost, // class to be switched on by runtime type
   int, double // other arguments
>;
// Allow correct linkage for overrides defined in dynamically loaded modules:
DECLARE_EXPORTED_ATTACHED_VIRTUAL(AUDACITY_DLL_API, DoSomething);
```

Definitions needed:
```
//file Client.cpp
// Define the default function body here
DEFINE_ATTACHED_VIRTUAL(DoSomething) {
   return [](AbstractHost &host, int arg1, double arg2) {
      return ErrorCode::Ok;
   };
   // or you could return nullptr instead of a lambda to force InconsistencyException
   // at runtime if the virtual function is invoked for a host subclass for which no override
   // was defined.
}
```

Usage of the method somewhere else:
```
#include "Client.h"
void UseDoSomething( AbstractHost &host )
{
   // ...
   auto error = DoSomething::Call( host, 0, 1.0 );
   // ...
}
```

Derived classes from AbstractHost, not needing Client.h:
```
// file SpecialHost.h
#include "Host.h"
class SpecialHost : public AbstractHost
{
   // ...
};

class ExtraSpecialHost : public SpecialHost
{
   // ...
};
```

Overrides of the method, defined in any other .cpp file:
```
#include "SpecialHost.h"
#include "Client.h"
// An override of the function, building up a hierarchy of function bodies parallel
// to the host class hierarchy
using DoSomethingSpecial = DoSomething::Override< SpecialHost >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoSomethingSpecial) {
   // The function can be defined without casting the first argument
   return [](SpecialHost &host, int arg1, double arg2) {
      return arg1 == 0 ? ErrorCode::Ok : ErrorCode::Bad;
   };
}
static DoSomethingSpecial registerMe;

// A further override, demonstrating call-through too
using DoSomethingExtraSpecial =
   DoSomething::Override< ExtraSpecialHost, DoSomethingSpecial >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoSomethingExtraSpecial) {
   return [](ExtraSpecialHost &host, int arg1, double arg2){
      // Call the immediately overridden version of the function
      auto result = Callthrough( host, arg1, arg2 );
      if ( result == ErrorCode::OK ) {
         if ( arg2 != 1066.0 )
            result = ErrorCode::Bad;
      }
      return result;
   };
}
static DoSomethingExtraSpecial registerMe;
```

@tparam Tag an incomplete type, to distinguish methods with otherwise identical parameters
@tparam Return the value returned by each override
@tparam This type of the first argument, a class with at least one true virtual function, the root of the hierarchy for the run-time type-switch
@tparam Arguments any number of types for the second and later arguments
 
*/
template< typename Tag, typename Return, typename This, typename... Arguments >
class AttachedVirtualFunction
{
public:

   //! This member name is declared in this class template and redeclared in each override
   using Object = This;
   //! This member name is declared in this class template and redeclared in each override
   using Function = std::function< Return( Object&, Arguments... ) >;
   //! A function returning a std::function, which you must define so that the program links
   /*! It may return nullptr in case this must act somewhat as a "pure virtual",
      throwing InconsistencyException if the function is invoked on a subclass
      for which no override was defined */
   static Function Implementation();

#ifdef _WIN32
   __declspec(dllexport)
#endif
    //! At least one static instance must be created; more instances are harmless
    /*! (There will be others if there are any overrides.) */
   AttachedVirtualFunction()
   {
      static std::once_flag flag;
      std::call_once( flag, []{ Register<Object>( Implementation() ); } );
   }

   //! For defining overrides of the method
   /*!
    @tparam Subclass the more specific subclass of @b This
    @tparam Overridden The immediately overridden version, defaulting to the base version
    */
   template<
      typename Subclass, typename Overridden = AttachedVirtualFunction >
   struct Override : Overridden
   {
      //! Shadowing Overridden::Object
      using Object = Subclass;
      //! Shadowing Overridden::Function, giving the first argument a more specific type
      using Function = std::function< Return( Object&, Arguments... ) >;

      // Check that inheritance is correct
      static_assert(
         std::is_base_of_v< typename Overridden::Object, Object >,
         "overridden class must be a base of the overriding class"
      );

      //! A function returning a std::function that must be defined so that the program links
      static Function Implementation();
      //! May be used in the body of the overriding function, defining it in terms of the overridden one
      static Return Callthrough(
         typename Overridden::Object &object, Arguments &&...arguments )
      {
         return Overridden::Implementation()(
            object, std::forward< Arguments >( arguments )... );
      }
#ifdef _WIN32
   __declspec(dllexport)
#endif
      //! At least one static instance must be created; more instances are harmless
      /*! (There will be others if there are any further overrides.) */
      Override()
      {
         static std::once_flag flag;
         std::call_once( flag, []{
            // Register in the table an adaptor thunk that downcasts the object
            auto implementation = Implementation();
            Register< Subclass >( [=]( This &obj, Arguments &&...arguments ){
               return implementation(
                  static_cast< Subclass& >( obj ),
                  std::forward< Arguments >( arguments )... );
            });
         });
      }
   };

   //! Invoke the method -- but only after static initialization time
   static Return Call(
      This &obj, //!< Object on which to type-switch at run-time
      Arguments &&...arguments //!< other arguments
   )
   {
      try {
         // Note that the constructors of this class and overrides cause
         // the registry to be topologically sorted, with functions for
         // less-derived classes earlier in the table; so take the last
         // one that matches the object.  (The object might not be of the exact
         // class corresponding to any of the overrides, which is why this
         // solution involves calling the predicates generated in Register,
         // and wouldn't work just with hashing on std::type_index; but perhaps
         // such a cache could be memo-ized)
         auto &registry = GetRegistry();
         auto iter = registry.rbegin(), end = registry.rend();
         for ( ; iter != end; ++iter ) {
            auto &entry = *iter;
            if ( entry.predicate( &obj ) )
               // This might throw std::bad_function_call on a null function
               return entry.function(
                  obj, std::forward< Arguments >( arguments )... );
         }
         // If not found, also throw
         throw std::bad_function_call{};
      }
      catch ( const std::bad_function_call& ) {
         // No matching case found with a non-null function.
         // Translate the exception
         THROW_INCONSISTENCY_EXCEPTION;
      }
   }

private:
   template< typename Subclass >
   static void Register( const Function &function )
   {
      // Push back a dynamic type test and corresponding function body
      GetRegistry().push_back({
         []( This *b ){ return dynamic_cast< Subclass * >( b ) != nullptr; },
         function
      });
   }

   using Predicate = std::function< bool( This* ) >;

   //! Member of registry of implementations of the method
   struct Entry
   {
      Predicate predicate;
      Function function;
   };

   using Registry = std::vector< Entry >;
#ifdef _WIN32
   __declspec(dllexport)
#endif
   static Registry &GetRegistry()
   {
      static Registry registry;
      return registry;
   }
};

//! Typically follow the `using` declaration of a new AttachedVirtualFunction with this macro
#define DECLARE_EXPORTED_ATTACHED_VIRTUAL(DECLSPEC, Name)      \
   template<> auto DECLSPEC Name::Implementation() -> Function

//! Used in the companion .cpp file to the .h using the above macro; followed by a function body
#define DEFINE_ATTACHED_VIRTUAL(Name)                          \
   static Name register ## Name ;                              \
   template<> auto Name::Implementation() -> Function

//! Used to define overriding function; followed by a function body
#define DEFINE_ATTACHED_VIRTUAL_OVERRIDE(Name)                 \
   static Name register ## Name ;                              \
   template<> template<> auto Name::Implementation() -> Function \

#endif
