/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.h
@brief Toolkit-neutral facade for basic user interface services

Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_BASIC_UI__
#define __AUDACITY_BASIC_UI__

#include <functional>

namespace BasicUI {

//! @name Types used in the Services interface
//! @{

using Action = std::function<void()>;

//! Subclasses may hold information such as a parent window pointer for a dialog.
/*! The default-constructed empty value of this base class must be accepted by overrides of methods of
 Services */
class BASIC_UI_API WindowPlacement {
public:
   WindowPlacement() = default;

   //! Don't slice
   WindowPlacement( const WindowPlacement& ) PROHIBITED;
   //! Don't slice
   WindowPlacement &operator=( const WindowPlacement& ) PROHIBITED;
   virtual ~WindowPlacement();
};

//! @}

//! Abstract class defines a few user interface services, not mentioning particular toolkits
/*! The intention is that the application supplies a concrete implementation at
 startup.  Most code will not use this class directly, but call the inline
 functions that follow. */
class BASIC_UI_API Services {
public:
   virtual ~Services();
   virtual void DoCallAfter(const Action &action) = 0;
   virtual void DoYield() = 0;
};

//! Fetch the global instance, or nullptr if none is yet installed
BASIC_UI_API Services *Get();

//! Install an implementation; return the previously installed instance
BASIC_UI_API Services *Install(Services *pInstance);

/*! @name Functions that invoke global Services
   These dispatch to the global Services, if supplied.  If none was supplied,
   they are mostly no-ops, with exceptions as noted.  All should be called on
   the main thread only, except as noted.
 */
//! @{

//! Schedule an action to be done later, and in the main thread
/*! This function may be called in other threads than the main.  If no Services
 are yet installed, the action is not lost, but may be dispatched by Yield().
 The action may itself invoke CallAfter to enqueue other actions.
 */
void CallAfter(Action action);

//! Dispatch waiting events, including actions enqueued by CallAfter
/*! This function must be called by the main thread.  Actions enqueued by
 CallAfter before Services were installed will be dispatched in the sequence
 they were enqueued, unless an exception thrown by one of them stops the
 dispatching.
 */
void Yield();

//! @}
}


#endif
