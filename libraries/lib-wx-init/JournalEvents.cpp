/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file JournalEvents.cpp

  Paul Licameli

*********************************************************************/

#include "JournalEvents.h"
#include "Journal.h"
#include "JournalOutput.h"
#include "JournalRegistry.h"
#include "JournalWindowPaths.h"

#include <map>
#include <mutex>
#include <optional>
#include <unordered_map>

#include <wx/eventfilter.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>
#include <wx/tglbtn.h>
#include <wx/valgen.h>
#include <wx/window.h>

#include "AudacityException.h"
#include "BasicUI.h"
#include "Identifier.h"
#include "wxArrayStringEx.h"

namespace Journal {

namespace Events {

namespace {

struct Type;
using Types = std::vector< Type >;

// The list of event types to intercept and record.  Its construction must be
// delayed until wxWidgets has initialized and chosen the integer values of
// run-time event types.
static const Types &TypeCatalog();

// Events need to be recorded in the journal, but the numbers associated with
// event types by wxWidgets are chosen dynamically and may not be the same
// across runs or platforms.  We need an invariant name for each event type
// of interest, which also makes the journal more legible than if we wrote mere
// numbers.
using Code = Identifier;

// Lookup into the event type catalog during recording
using ByTypeMap = std::map< wxEventType, const Type& >;
static const ByTypeMap &ByType();

// Lookup into the event type catalog during playback
using ByCodeMap = std::unordered_map< Code, const Type & >;
static const ByCodeMap &ByCode();

// This sub-object populates the journal system's dictionary for playback
struct RegisteredEventType : RegisteredCommand {
   static bool DispatchEvent( wxArrayStringEx fields );
   explicit RegisteredEventType( Code code )
   : RegisteredCommand{ code.GET(), DispatchEvent }
   {}
};

//! Whether the event filter is still watching events
static bool sWatching{ true };

/*!
An entry in a catalog describing the types of events that are intercepted
and recorded, and simulated when playing back.
*/
struct Type : RegisteredEventType {

   // Function that returns a list of parameters that, with the event type,
   // are sufficient to record an event to the journal and recreate it on
   // playback; or a nullopt to skip the event
   using Serializer =
      std::function< std::optional<wxArrayStringEx>( const wxEvent& ) >;

   // Function that recreates an event at playback; or a null for failure
   using Deserializer = std::function< std::unique_ptr<wxEvent>(
      wxEventType, const wxArrayStringEx& ) >;

   // Helper to keep casts out of the supplied function's definition
   // Tag is the subtype of wxEvent expected by fn
   template< typename Tag, typename SerialFn >
   Serializer makeSerializer( SerialFn fn )
   {
      // Return an adaptor taking wxEvent
      return [fn = std::move( fn )]( const wxEvent &e ){
         return fn(dynamic_cast<const Tag&>(e));
      };
   }

   // Helper to check consistency
   // Tag is a subtype of wxEvent (such as wxCommandEvent), and fn defines
   // a deserializer dependent on a run-time wxEvent type (such as one of
   // the many event types implemented by wxCommandEvent)
   template< typename Tag, typename Fn >
   Deserializer checkDeserializer( wxEventTypeTag<Tag> type, Fn fn )
   {
      // Check consistency of the deduced template parameters
      // The deserializer should produce unique pointer to Tag
      using DeserializerResult =
         decltype( *fn( wxEVT_NULL, wxArrayStringEx{} ) );
      static_assert( std::is_same_v< Tag&, DeserializerResult >,
         "deserializer produces the wrong type" );

      return std::move( fn );
   }

   // Type-erasing constructor applies compile-time type consistency checks to
   // the functions, which can be supplied as lambdas using a subtype of wxEvent
   template<
      typename Tag, // such as wxCommandEvent
      typename SerialFn,
      typename DeserialFn
   >
   Type(
      wxEventTypeTag<Tag> type, /*!< such as wxEVT_BUTTON
         Tag is deduced at compile time, the int value is fixed at runtime
      */
      const Code &code, SerialFn serialFn, DeserialFn deserialFn )
      : RegisteredEventType{ code }
      , type{ type }
      , code{ code }
      , serializer{ makeSerializer<Tag>( std::move( serialFn ) ) }
      , deserializer{ checkDeserializer( type, std::move( deserialFn ) ) }
   {
   }

   // Data members
   wxEventType type; //!< Just an int!
   Code code; //!< Persistent cross-platform name corresponding to the int
   Serializer serializer;
   Deserializer deserializer;
};

bool RegisteredEventType::DispatchEvent( wxArrayStringEx fields )
{
   bool handled = false;
   if ( !fields.empty() ) {
      auto &catalog = ByCode();
      auto first = fields.begin();
      if (auto iter = catalog.find( *first ); iter != catalog.end()) {
         auto &type = iter->second;
         fields.erase( first );
         auto pEvent = type.deserializer( type.type, fields );
         if ( pEvent ) {
            // So far only dispatching command events.  Maybe other
            // methods of dispatch will be appropriate for other types.
            if ( auto pHandler = dynamic_cast<wxEvtHandler*>(
               pEvent->GetEventObject() )
            ) {
               if (auto pWindow = dynamic_cast<wxWindow *>(pHandler))
                  // Allow for the pushing and popping of event handlers
                  // on windows
                  pHandler = pWindow->GetEventHandler();
               pHandler->SafelyProcessEvent( *pEvent );
               handled = true;
            }
         }
      }
   }
   return handled;
}

wxString WindowEventName( const wxEvent &event )
{
   wxString windowName;
   if ( auto pWindow = dynamic_cast< wxWindow * >( event.GetEventObject() ) )
      windowName = WindowPaths::FindPath( *pWindow ).GET();
   return windowName;
}

std::optional<wxArrayStringEx> WindowEventSerialization( const wxEvent &event )
{
   std::optional< wxArrayStringEx > result;
   if ( auto windowName = WindowEventName( event ); !windowName.empty() )
      result.emplace( wxArrayStringEx{ windowName } );
   else
      FailedEventSerialization();
   return result;
}

template<typename Event = wxCommandEvent, typename EventType >
static Type NullaryCommandType( EventType type, const wxString &code ){
   const auto deserialize =
   []( wxEventType evtType, const wxArrayStringEx &components) {
      std::unique_ptr< wxCommandEvent > result;
      if ( components.size() == 1 ) {
         if ( auto pWindow = WindowPaths::FindByPath( components[0] ) ) {
            result = std::make_unique< wxCommandEvent >(
               evtType, pWindow->GetId() );
            result->SetEventObject( pWindow );
         }
      }
      return result;
   };

   return Type{ type, code, WindowEventSerialization, deserialize };
}

template<typename Event = wxCommandEvent, typename EventType >
static Type BooleanCommandType( EventType type, const wxString &code ){
   // Writes a window identifier to the journal and a bool
   const auto serialize =
   []( const Event &event ) {
      auto result = WindowEventSerialization( event );
      if ( result )
         result->push_back(
            wxString::Format( L"%d", event.GetInt() ? 1 : 0 ) );
      return result;
   };

   const auto deserialize =
   []( wxEventType type, const wxArrayStringEx &components ) {
      std::unique_ptr< Event > result;
      int value;
      if ( components.size() == 2 ) {
         if ( auto pWindow = WindowPaths::FindByPath( components[0] ) ) {
            if ( long longValue; components[1].ToLong( &longValue ) ) {
               bool value = (longValue != 0);
               result = std::make_unique< Event >(
                  type, pWindow->GetId() );
               result->SetEventObject( pWindow );
               result->SetInt( value );
               // Also change the state of the control before the event is
               // processed.  This class handles the most common control
               // types.
               wxGenericValidator validator( &value );
               validator.SetWindow( pWindow );
               validator.TransferToWindow();
            }
         }
      }
      return result;
   };

   return Type{ type, code, serialize, deserialize };
}

template<typename Event = wxCommandEvent, typename EventType >
static Type NumericalCommandType( EventType type, const wxString &code ){
   // Writes a window identifier to the journal and an int
   const auto serialize =
   []( const Event &event ) {
      auto result = WindowEventSerialization( event );
      if ( result )
         result->push_back( wxString::Format( L"%d", event.GetInt() ) );
      return result;
   };

   const auto deserialize =
   []( wxEventType type, const wxArrayStringEx &components ) {
      std::unique_ptr< Event > result;
      int value;
      if ( components.size() == 2 ) {
         if ( auto pWindow = WindowPaths::FindByPath( components[0] ) ) {
            if ( long longValue;
                components[1].ToLong( &longValue ) &&
                longValue == (value = static_cast<int>(longValue) )
            ) {
               result = std::make_unique< Event >(
                  type, pWindow->GetId() );
               result->SetEventObject( pWindow );
               result->SetInt( value );
               // Also change the state of the control before the event is
               // processed.  This class handles the most common control
               // types.
               wxGenericValidator validator( &value );
               validator.SetWindow( pWindow );
               validator.TransferToWindow();
            }
         }
      }
      return result;
   };

   return Type{ type, code, serialize, deserialize };
}

template<typename Event = wxCommandEvent, typename EventType >
static Type TextualCommandType( EventType type, const wxString &code ){
   // Writes a window identifier to the journal and a string
   const auto serialize =
   []( const Event &event ) {
      auto result = WindowEventSerialization( event );
      if ( result )
         result->push_back( event.GetString() );
      return result;
   };

   const auto deserialize =
   []( wxEventType type, const wxArrayStringEx &components ) {
      std::unique_ptr< Event > result;
      if ( components.size() == 2 ) {
         if ( auto pWindow = WindowPaths::FindByPath( components[0] ) ) {
            auto value = components[1];
            result = std::make_unique< Event >(
               type, pWindow->GetId() );
            result->SetEventObject( pWindow );
            result->SetString( value );
            // Also change the state of the control before the event is
            // processed.
            if ( auto pCtrl = dynamic_cast<wxTextEntry*>( pWindow ) ) {
               // Generic validator calls the SetValue() member function,
               // which generates an event for this type of control, but we
               // don't want that.  So use ChangeValue() instead
               pCtrl->ChangeValue( value );
            }
            else {
               // This class handles the most common control types.
               wxGenericValidator validator( &value );
               validator.SetWindow( pWindow );
               validator.TransferToWindow();
            }
         }
      }
      return result;
   };

   return Type{ type, code, serialize, deserialize };
}

const Types &TypeCatalog()
{
   static Types result {
      NullaryCommandType( wxEVT_BUTTON, "Press" ),
      NullaryCommandType( wxEVT_COMBOBOX_DROPDOWN, "DropDown" ),
      NullaryCommandType( wxEVT_COMBOBOX_CLOSEUP, "CloseUp" ),

      BooleanCommandType( wxEVT_CHECKBOX, "Check" ),
      BooleanCommandType( wxEVT_RADIOBUTTON, "Radio" ),
      BooleanCommandType( wxEVT_RADIOBOX, "RadioBox" ),
      BooleanCommandType( wxEVT_TOGGLEBUTTON, "Toggle" ),

      NumericalCommandType( wxEVT_CHOICE, "Choose" ),
      NumericalCommandType( wxEVT_SLIDER, "Slide" ),
      NumericalCommandType<wxSpinEvent>( wxEVT_SPINCTRL, "Spin" ),
      NumericalCommandType<wxSpinEvent>( wxEVT_SPIN_DOWN, "SpinUp" ),
      NumericalCommandType<wxSpinEvent>( wxEVT_SPIN_UP, "SpinDown" ),

      TextualCommandType( wxEVT_COMBOBOX, "Combo" ),
      TextualCommandType( wxEVT_TEXT, "Text" ),
      TextualCommandType( wxEVT_TEXT_ENTER, "FullText" ),
   };
   return result;
}

static const ByTypeMap &ByType()
{
   static std::once_flag flag;
   static ByTypeMap result;
   std::call_once( flag, []{
      for ( const auto &type : TypeCatalog() )
         result.emplace( type.type, type );
   } );
   return result;
}

static const ByCodeMap &ByCode()
{
   static std::once_flag flag;
   static ByCodeMap result;
   std::call_once( flag, []{
      for ( const auto &type : TypeCatalog() )
         result.emplace( type.code, type );
   } );
   return result;
}

//! Singleton object listens to global wxEvent stream
struct Watcher : wxEventFilter
{
   Watcher()
   {
      wxEvtHandler::AddFilter( this );
   }

   ~Watcher()
   {
      wxEvtHandler::RemoveFilter( this );
   }

   int FilterEvent( wxEvent &event ) override
   {
      if (!IsWatching())
         // Previously encountered error stopped recording of any more events
         return Event_Skip;

      static const auto &catalog = Events::ByType();
      const auto type = event.GetEventType();
      if (const auto iter = catalog.find(type); iter != catalog.end()) {
         // Try to write a representation to the journal
         const auto &info = iter->second;
         auto pStrings = info.serializer(event);
         if (!pStrings)
            return Event_Skip;
         else {
            pStrings->insert(pStrings->begin(), info.code.GET());
            Journal::Output(*pStrings);
         }
      }
      else
         // Just ignore this un-catalogued event type
         ;

      return Event_Skip;
   }
};

}

bool IsWatching()
{
   return sWatching;
}

void FailedEventSerialization()
{
   // After one event of one of the interesting types fails to record,
   // don't try again
   sWatching = false;
   BasicUI::CallAfter( []{
      BasicUI::ShowMessageBox(XO("Journal recording failed"));
   } );
}

namespace {

void Initialize()
{
   (void) TypeCatalog();
}

void Watch()
{
   static Watcher instance;
}

// Add a callback for startup of journalling
RegisteredInitializer initializer{ [](){
   // Register the event handler for recording
   // and dictionary items for replaying
   if ( !GetError() && IsRecording() )
      // one time installation
      Watch();

   if ( !GetError() && IsReplaying() )
      // Be sure event types are registered for dispatch
      Initialize();

   return true;
} };

}

}

}
