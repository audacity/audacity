/**********************************************************************

Audacity: A Digital Audio Editor

AttachedVirtualFunction.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_ATTACHED_VIRTUAL_FUNCTION__
#define __AUDACITY_ATTACHED_VIRTUAL_FUNCTION__

/* \brief Define a "virtual" function with multiple bodies chosen by type-switch
on the runtime class of the first argument, leaving the set of functions
open-ended for extension, but also requiring no modification of the definition
of the base class of the type hierarchy that is switched on.

The invocation of the function is not as efficient as for true virtual functions
but the advantage of this utility is greater compilation decoupling.  A client
can attach its own virtual functions to a class hierarchy in the core.

Beware that invocation of the function should not be done during initialization
of file scope static objects.  Dispatch might not go to the correct subclass
case if initializations are not yet complete.

Example usage:

////////
// Core classes in Host.h:
class AbstractHost
{
   // ...
};

class SpecialHost : public Abstract Host
{
   // ...
};

class ExtraSpecialHost : public SpecialHost
{
   // ...
};

////////
// Declare the root of the attached function hierarchy in Client.h,
// which Host.cpp need not include at all:

enum class ErrorCode { Ok, Bad, // ...
}; // a return type for our function

// First this empty structure serving just to distinguish among instantiations
// of AttachedVirtualFunction with otherwise identical arguments
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

////////
// Usage of the "virtual function"
#include "Client.h"
void UseDoSomething( AbstractHost &host )
{
   // ...
   auto error = DoSomething::Call( host, 0, 1.0 );
   // ...
}

////////
// Out-of-line registrations needed in Client.cpp:

// Define the default function body here (as a function returning a function!)
template<> auto DoSomething::Implementation() -> Function {
   return [](AbstractHost &host, int arg1, double arg2) {
      return ErrorCode::Ok;
   };
   // or you could return nullptr to force InconsistencyException at runtime if
   // the virtual function is invoked for a host subclass for which no override
   // was defined
}
// Must also guarantee construction of an instance of class DoSomething at least
// once before any use of DoSomething::Call()
static DoSomething registerMe;

////////
// An override of the function is defined in SpecialClient.cpp,
// building up a hierarchy of function bodies parallel to the host class
// hierarchy
using DoSomethingSpecial = DoSomething::Override< SpecialHost >;
template<> template<> auto DoSomethingSpecial::Implementation() -> Function {
   // The function can be defined assuming downcast of the first argument
   // has been done
   return [](SpecialHost &host, int arg1, double arg2) {
      return arg1 == 0 ? ErrorCode::Ok : ErrorCode::Bad;
   };
}
static DoSomethingSpecial registerMe;

////////
// A further override in ExtraSpecialClient.cpp, demonstrating call-through too
using DoSomethingExtraSpecial =
   DoSomething::Override< ExtraSpecialHost, DoSomethingSpecial >;
template<> template<>
auto DoSomethingExtraSpecial::Implementation() -> Function {
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
static DoSomethinExtraSpecial registerMe;

*/

#include <functional>
#include <mutex>
#include <type_traits>
#include <utility>
#include "InconsistencyException.h"

template< typename Tag, typename Return, typename This, typename... Arguments >
class AttachedVirtualFunction
{
public:

   // These member names are declared in this class template and redeclared
   // in each override
   using Object = This;
   using Function = std::function< Return( Object&, Arguments... ) >;
   // A function returning a std::function, which you define elsewhere;
   // it may return nullptr in case this must act somewhat as a "pure virtual",
   // throwing InconsistencyException if the function is invoked on a subclass
   // for which no override was defined
   static Function Implementation();

   // This class must be instantiated once at least to register the function in
   // a table, but may be instantiated multiply (and will be if there are any
   // overrides)
   AttachedVirtualFunction()
   {
      static std::once_flag flag;
      std::call_once( flag, []{ Register<This>( Implementation() ); } );
   }

   // For defining overrides of the virtual function; template arguments
   // are the more specific subclass and the immediately overridden version
   // of the function, defaulting to the base version
   template<
      typename Subclass, typename Overridden = AttachedVirtualFunction >
   struct Override : Overridden
   {
      using Object = Subclass;
      using Function = std::function< Return( Object&, Arguments... ) >;

      // Check that inheritance is correct
      static_assert(
         std::is_base_of< typename Overridden::Object, Object >::value,
         "overridden class must be a base of the overriding class"
      );

      // A function returning a std::function that must be defined out-of-line
      static Function Implementation();
      // May be used in the body of the overriding function, defining it in
      // terms of the overridden one:
      static Return Callthrough(
         typename Overridden::Object &object, Arguments &&...arguments )
      {
         return Overridden::Implementation()(
            object, std::forward< Arguments >( arguments )... );
      }
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

   static Return Call( This &obj, Arguments &&...arguments )
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
               return entry.function(
                  obj, std::forward< Arguments >( arguments )... );
         }
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

   struct Entry
   {
      Predicate predicate;
      Function function;
   };

   using Registry = std::vector< Entry >;
   static Registry &GetRegistry()
   {
      static Registry registry;
      return registry;
   }
};

#endif
