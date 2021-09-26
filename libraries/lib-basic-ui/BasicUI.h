/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.h
@brief Toolkit-neutral facade for basic user interface services

Paul Licameli

**********************************************************************/
#ifndef __AUDACITY_BASIC_UI__
#define __AUDACITY_BASIC_UI__

#include <functional>
#include <memory>
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

enum ProgressDialogOptions : unsigned {
   ProgressShowStop            = (1 << 0),
   ProgressShowCancel          = (1 << 1),
   ProgressHideTime            = (1 << 2),
   ProgressConfirmStopOrCancel = (1 << 3),
};

enum class ProgressResult : unsigned
{
   Cancelled = 0, //<! User says that whatever is happening is undesirable and shouldn't have happened at all
   Success,       //<! User says nothing, everything works fine, continue doing whatever we're doing
   Failed,        //<! Something has gone wrong, we should stop and cancel everything we did
   Stopped        //<! Nothing is wrong, but user says we should stop now and leave things as they are now
};

//! Abstraction of a progress dialog with well defined time-to-completion estimate
class BASIC_UI_API ProgressDialog
{
public:
   virtual ~ProgressDialog();

   //! Update the bar and poll for clicks.  Call only on the main thread.
   virtual ProgressResult Poll(
      unsigned long long numerator,
      unsigned long long denominator,
      const TranslatableString &message = {}) = 0;
};

//! Abstraction of a progress dialog with undefined time-to-completion estimate
class BASIC_UI_API GenericProgressDialog
{
public:
   virtual ~GenericProgressDialog();
   //! Give some visual indication of progress.  Call only on the main thread.
   virtual void Pulse() = 0;
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
   virtual std::unique_ptr<ProgressDialog>
   DoMakeProgress(const TranslatableString &title,
      const TranslatableString &message,
      unsigned flag,
      const TranslatableString &remainingLabelText) = 0;
   virtual std::unique_ptr<GenericProgressDialog>
   DoMakeGenericProgress(const WindowPlacement &placement,
      const TranslatableString &title,
      const TranslatableString &message) = 0;
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
BASIC_UI_API void CallAfter(Action action);

//! Dispatch waiting events, including actions enqueued by CallAfter
/*! This function must be called by the main thread.  Actions enqueued by
 CallAfter before Services were installed will be dispatched in the sequence
 they were enqueued, unless an exception thrown by one of them stops the
 dispatching.
 */
BASIC_UI_API void Yield();

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

//! Create and display a progress dialog
/*!
 @param flags bitwise OR of values in ProgressDialogOptions
 @param remainingLabelText if not empty substitutes for "Remaining Time:"
 @return nullptr if Services not installed
 */
inline std::unique_ptr<ProgressDialog> MakeProgress(
   const TranslatableString & title,
   const TranslatableString & message,
   unsigned flags = (ProgressShowStop | ProgressShowCancel),
   const TranslatableString & remainingLabelText = {})
{
   if (auto p = Get())
      return p->DoMakeProgress(title, message, flags, remainingLabelText);
   else
      return nullptr;
}

//! Create and display a progress dialog (return nullptr if Services not installed)
/*!
 This function makes a "generic" progress dialog, for the case when time
 to completion cannot be estimated, but some indication of progress is still
 given
 */
inline std::unique_ptr<GenericProgressDialog> MakeGenericProgress(
   const WindowPlacement &placement,
   const TranslatableString &title, const TranslatableString &message)
{
   if (auto p = Get())
      return p->DoMakeGenericProgress(placement, title, message);
   else
      return nullptr;
}

//! @}
}

#endif
