/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.h
@brief Toolkit-neutral facade for basic user interface services

Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_BASIC_UI__
#define __AUDACITY_BASIC_UI__

#include <functional>
#include "Identifier.h"
#include "Internat.h"

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

enum class ErrorDialogType {
   ModelessError,
   ModalError,
   ModalErrorReport, /*!< If error reporting is enabled, may give option to
                      send; if not, then like ModalError
                      */
};

//! Options for variations of error dialogs; the default is for modal dialogs
struct ErrorDialogOptions {

   ErrorDialogOptions() = default;
   //! Non-explicit
   ErrorDialogOptions( ErrorDialogType type ) : type{ type } {}

   //! @name Chain-call style initializers
   //! @{

   ErrorDialogOptions &&ModalHelp( bool modalHelp_ ) &&
   { modalHelp = modalHelp_; return std::move(*this); }

   ErrorDialogOptions &&Log( std::wstring log_ ) &&
   { log = std::move(log_); return std::move(*this); }

   //! @}

   //! Type of help dialog
   ErrorDialogType type{ ErrorDialogType::ModalError };
   //! Whether the secondary help dialog with more information should be modal
   bool modalHelp{ true };
   //! Optional extra logging information to be shown
   std::wstring log;
};

//! "Message", suitably translated
BASIC_UI_API TranslatableString DefaultCaption();

enum class Icon {
   None,
   Warning,
   Error,
   Question,
   Information,
};

enum class Button {
   Default, //!< Like Ok, except maybe minor difference of dialog position
   Ok,      //!< One button
   YesNo    //!< Two buttons
};

struct MessageBoxOptions {
   //! @name Chain-call style initializers
   //! @{

   MessageBoxOptions &&Parent(WindowPlacement *parent_) &&
   { parent = parent_; return std::move(*this); }

   MessageBoxOptions &&Caption(TranslatableString caption_) &&
   { caption = std::move(caption_); return std::move(*this); }

   MessageBoxOptions &&IconStyle(Icon style) &&
   { iconStyle = style; return std::move(*this); }

   MessageBoxOptions &&ButtonStyle(Button style) &&
   { buttonStyle = style; return std::move(*this); }

   //! Override the usual defaulting to Yes; affects only the YesNo case
   MessageBoxOptions &&DefaultIsNo() &&
   { yesOrOkDefaultButton = false; return std::move(*this); }

   MessageBoxOptions &&CancelButton() &&
   { cancelButton = true; return std::move(*this); }

   //! Center the dialog on its parent window, if any
   MessageBoxOptions &&Centered() &&
   { centered = true; return std::move(*this); }

   //! @}

   WindowPlacement *parent{ nullptr };
   TranslatableString caption{ DefaultCaption() };
   Icon iconStyle{ Icon::None };
   Button buttonStyle{ Button::Default };
   bool yesOrOkDefaultButton{ true };
   bool cancelButton{ false };
   bool centered{ false };
};

enum class MessageBoxResult : int {
   None, //!< May be returned if no Services are installed
   Yes,
   No,
   Ok,
   Cancel,
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
   virtual void DoShowErrorDialog(const WindowPlacement &placement,
      const TranslatableString &dlogTitle,
      const TranslatableString &message,
      const ManualPageID &helpPage,
      const ErrorDialogOptions &options) = 0;
   virtual MessageBoxResult DoMessageBox(
      const TranslatableString& message,
      MessageBoxOptions options) = 0;
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

//! Show an error dialog with a link to the manual for further help
inline void ShowErrorDialog(
   const WindowPlacement &placement,    //!< how to parent the dialog
   const TranslatableString &dlogTitle, //!< Text for title bar
   const TranslatableString &message,   //!< The main message text
   const ManualPageID &helpPage,          //!< Identifies manual page (and maybe an anchor)
   const ErrorDialogOptions &options = {})
{
   if (auto p = Get())
      p->DoShowErrorDialog(placement, dlogTitle, message, helpPage, options);
}

//! Show a modal message box with either Ok or Yes and No, and optionally Cancel
/*!
 @return indicates which button was pressed
 */
inline MessageBoxResult ShowMessageBox( const TranslatableString &message,
   MessageBoxOptions options = {})
{
   if (auto p = Get())
      return p->DoMessageBox(message, std::move(options));
   else
      return MessageBoxResult::None;
}

//! @}
}


#endif
