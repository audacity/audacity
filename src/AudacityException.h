#ifndef __AUDACITY_EXCEPTION__
#define __AUDACITY_EXCEPTION__

/**********************************************************************

 Audacity: A Digital Audio Editor

 AudacityException.h

 Paul Licameli
 
 Define the root of a hierarchy of classes that are thrown and caught
 by Audacity.
 
 Define some subclasses.  Not all subclasses need be defined here.

 **********************************************************************/

#include "MemoryX.h"
#include <wx/app.h>
#include <exception>

#include "Internat.h"

class wxString;

class AudacityException /* not final */
{
public:
   AudacityException() {}
   virtual ~AudacityException() = 0;

   // This is intended as a "polymorphic move copy constructor"
   // which leaves this "empty".
   // We would not need this if we had std::exception_ptr
   virtual std::unique_ptr< AudacityException > Move() = 0;

   // Action to do in the main thread at idle time of the event loop.
   virtual void DelayedHandlerAction() = 0;

protected:
   // Make this protected to prevent slicing copies
   AudacityException( AudacityException&& ) {}
   AudacityException( const AudacityException& ) = default;
   AudacityException &operator = (AudacityException &&) { return *this;}
   AudacityException &operator = ( const AudacityException & ) PROHIBITED;
};

// A subclass of AudacityException whose delayed handler action displays
// a message box.  The message is specified by further subclasses.
// Not more than one message box will be displayed for each pass through
// the main event idle loop.
class MessageBoxException /* not final */ : public AudacityException
{
   // Do not allow subclasses to change this behavior further, except
   // by overriding ErrorMessage()
   using AudacityException::DelayedHandlerAction;
   void DelayedHandlerAction() override;

protected:
   // If default-constructed with empty caption, it makes no message box.
   explicit MessageBoxException( const wxString &caption = wxString{} );
   ~MessageBoxException() override;

   MessageBoxException( const MessageBoxException& );
   MessageBoxException &operator = ( MessageBoxException && );

   // Format a default, internationalized error message for this exception.
   virtual wxString ErrorMessage() const = 0;

private:
   wxString caption;
   mutable bool moved { false };
};

// MessageBoxException that shows a given, unvarying string.
class SimpleMessageBoxException /* not final */ : public MessageBoxException
{
public:
   explicit SimpleMessageBoxException( const wxString &message_,
      const wxString &caption = _("Message") )
      : MessageBoxException{ caption }
      , message{ message_ }
   {}
   ~SimpleMessageBoxException() override;

   SimpleMessageBoxException( const SimpleMessageBoxException& ) = default;
   SimpleMessageBoxException &operator = (
      SimpleMessageBoxException && ) PROHIBITED;

   std::unique_ptr< AudacityException > Move() override;

   // Format a default, internationalized error message for this exception.
   virtual wxString ErrorMessage() const override;

private:
   wxString message;
};

struct DefaultDelayedHandlerAction
{
   void operator () (AudacityException *pException) const
   {
      if ( pException )
         pException->DelayedHandlerAction();
   }
};

// Classes that can supply the second argument of GuardedCall:
// Frequently useful converter of all exceptions to some failure constant
template <typename R> struct SimpleGuard
{
   explicit SimpleGuard( R value ) : m_value{ value } {}
   R operator () ( AudacityException * ) const { return m_value; }
   const R m_value;
};

// Simple guard specialization that returns bool, and defines Default
template<> struct SimpleGuard<bool>
{
   explicit SimpleGuard( bool value ) : m_value{ value } {}
   bool operator () ( AudacityException * ) const { return m_value; }
   static SimpleGuard Default()
      { return SimpleGuard{ false }; }
   const bool m_value;
};

// Simple guard specialization that returns nothing, and defines Default
template<> struct SimpleGuard<void>
{
   SimpleGuard() {}
   void operator () ( AudacityException * ) const {}
   static SimpleGuard Default() { return {}; }
};

template < typename R >
SimpleGuard< R > MakeSimpleGuard( R value )
{ return SimpleGuard< R >{ value }; }

inline SimpleGuard< void > MakeSimpleGuard() { return {}; }

/**
 * Call the body function (usually a lambda) inside a try block.
 *
 * The handler intercepts exceptions, and is passed nullptr if the
 * exception is of a type not defined by Audacity.  It may return a value
 * for the guarded call or throw the same or another exception.
 * It executes in the same thread as the body.
 *
 * If the handler is passed non-null and does not throw, then delayedHandler
 * executes later in the main thread, in idle time of the event loop.
 */
template <
   typename R = void, // return type

   typename F1, // function object with signature R()

   typename F2 = SimpleGuard< R >, // function object
      // with signature R( AudacityException * )

   typename F3 =
      DefaultDelayedHandlerAction // Any( AudacityException * ), ignore return
>
R GuardedCall
   ( const F1 &body,
     const F2 &handler = F2::Default(),
     const F3 &delayedHandler = {} )
{
   try { return body(); }
   catch ( AudacityException &e ) {

      auto end = finally([&]{
         if (!std::uncaught_exception()) {
            auto pException =
               std::shared_ptr< AudacityException > { e.Move().release() };
            wxTheApp->CallAfter( [=] { // capture pException by value
               delayedHandler( pException.get() );
            } );
         }
      });

      return handler( &e );
   }
   catch ( ... ) {
      return handler( nullptr );
   }
}

#endif
