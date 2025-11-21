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

using Action = std::function<void ()>;
using ProgressReporter = std::function<void (double)>;

//! Subclasses may hold information such as a parent window pointer for a dialog.
/*! The default-constructed empty value of this base class must be accepted by overrides of methods of
 Services */
class BASIC_UI_API WindowPlacement
{
public:
    WindowPlacement() = default;

    //! Don't slice
    WindowPlacement(const WindowPlacement&) = delete;
    //! Don't slice
    WindowPlacement& operator=(const WindowPlacement&) = delete;
    //! Whether null; default in the base class returns false
    virtual explicit operator bool() const;
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
    ErrorDialogOptions(ErrorDialogType type)
        : type{type} {}

    //! @name Chain-call style initializers
    //! @{

    ErrorDialogOptions&& ModalHelp(bool modalHelp_)
    && { modalHelp = modalHelp_; return std::move(*this); }

    ErrorDialogOptions&& Log(std::wstring log_)
    && { log = std::move(log_); return std::move(*this); }

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
    Ok,     //!< One button
    YesNo   //!< Two buttons
};

struct MessageBoxOptions {
    //! @name Chain-call style initializers
    //! @{

    MessageBoxOptions&& Parent(WindowPlacement* parent_)
    && { parent = parent_; return std::move(*this); }

    MessageBoxOptions&& Caption(TranslatableString caption_)
    && { caption = std::move(caption_); return std::move(*this); }

    MessageBoxOptions&& IconStyle(Icon style)
    && { iconStyle = style; return std::move(*this); }

    MessageBoxOptions&& ButtonStyle(Button style)
    && { buttonStyle = style; return std::move(*this); }

    //! Override the usual defaulting to Yes; affects only the YesNo case
    MessageBoxOptions&& DefaultIsNo()
    && { yesOrOkDefaultButton = false; return std::move(*this); }

    MessageBoxOptions&& CancelButton()
    && { cancelButton = true; return std::move(*this); }

    //! Center the dialog on its parent window, if any
    MessageBoxOptions&& Centered()
    && { centered = true; return std::move(*this); }

    //! @}

    WindowPlacement* parent{ nullptr };
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

enum GenericProgressDialogStyle : int {
    ProgressCanAbort            = (1 << 0),
    ProgressAppModal            = (1 << 1),
    ProgressShowElapsedTime     = (1 << 2),
    ProgressSmooth              = (1 << 3),
};

enum class ProgressResult : unsigned
{
    Cancelled = 0, //<! User says that whatever is happening is undesirable and shouldn't have happened at all
    Success,      //<! User says nothing, everything works fine, continue doing whatever we're doing
    Failed,       //<! Something has gone wrong, we should stop and cancel everything we did
    Stopped       //<! Nothing is wrong, but user says we should stop now and leave things as they are now
};

//! Abstraction of a progress dialog with well defined time-to-completion estimate
class BASIC_UI_API ProgressDialog
{
public:
    virtual ~ProgressDialog();

    //! Update the bar and poll for clicks.  Call only on the main thread.
    virtual ProgressResult Poll(
        unsigned long long numerator, unsigned long long denominator, const TranslatableString& message = {}) = 0;

    //! Change an existing dialog's message
    virtual void SetMessage(const TranslatableString& message) = 0;

    //! Change the dialog's title
    virtual void SetDialogTitle(const TranslatableString& title) = 0;

    //! Reset the dialog state
    virtual void Reinit() = 0;
};

//! Abstraction of a progress dialog with undefined time-to-completion estimate
class BASIC_UI_API GenericProgressDialog
{
public:
    virtual ~GenericProgressDialog();
    //! Give some visual indication of progress.  Call only on the main thread.
    virtual ProgressResult Pulse() = 0;
};

//! @}

//! Abstract class defines a few user interface services, not mentioning particular toolkits
/*! The intention is that the application supplies a concrete implementation at
 startup.  Most code will not use this class directly, but call the inline
 functions that follow. */
class BASIC_UI_API Services
{
public:
    virtual ~Services();
    virtual void DoCallAfter(const Action& action) = 0;
    virtual void DoYield() = 0;
    virtual void DoShowErrorDialog(const WindowPlacement& placement, const TranslatableString& dlogTitle, const TranslatableString& message,
                                   const ManualPageID& helpPage, const ErrorDialogOptions& options) = 0;
    virtual MessageBoxResult DoMessageBox(
        const TranslatableString& message, MessageBoxOptions options) = 0;
    virtual std::unique_ptr<ProgressDialog>
    DoMakeProgress(const TranslatableString& title, const TranslatableString& message, unsigned flag,
                   const TranslatableString& remainingLabelText) = 0;
    virtual std::unique_ptr<GenericProgressDialog>
    DoMakeGenericProgress(const WindowPlacement& placement, const TranslatableString& title, const TranslatableString& message,
                          int style) = 0;
    virtual int DoMultiDialog(const TranslatableString& message, const TranslatableString& title, const TranslatableStrings& buttons,
                              const ManualPageID& helpPage, const TranslatableString& boxMsg, bool log) = 0;

    virtual bool DoOpenInDefaultBrowser(const wxString& url) = 0;

    virtual std::unique_ptr<WindowPlacement> DoFindFocus() = 0;
    virtual void DoSetFocus(const WindowPlacement& focus) = 0;

    virtual bool IsUsingRtlLayout() const = 0;

    virtual bool IsUiThread() const = 0;
};

//! Fetch the global instance, or nullptr if none is yet installed
BASIC_UI_API Services* Get();

//! Install an implementation; return the previously installed instance
BASIC_UI_API Services* Install(Services* pInstance);

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

//! Open an URL in default browser
/*! This function may be called in other threads than the main.
 */
BASIC_UI_API bool OpenInDefaultBrowser(const wxString& url);

//! Show an error dialog with a link to the manual for further help
inline void ShowErrorDialog(
    const WindowPlacement& placement,   //!< how to parent the dialog
    const TranslatableString& dlogTitle, //!< Text for title bar
    const TranslatableString& message,  //!< The main message text
    const ManualPageID& helpPage,         //!< Identifies manual page (and maybe an anchor)
    const ErrorDialogOptions& options = {})
{
    if (auto p = Get()) {
        p->DoShowErrorDialog(placement, dlogTitle, message, helpPage, options);
    }
}

//! Show a modal message box with either Ok or Yes and No, and optionally Cancel
/*!
 @return indicates which button was pressed
 */
inline MessageBoxResult ShowMessageBox(const TranslatableString& message,
                                       MessageBoxOptions options = {})
{
    if (auto p = Get()) {
        return p->DoMessageBox(message, std::move(options));
    } else {
        return MessageBoxResult::None;
    }
}

//! Create and display a progress dialog
/*!
 @param flags bitwise OR of values in ProgressDialogOptions
 @param remainingLabelText if not empty substitutes for "Remaining Time:"
 @return nullptr if Services not installed
 */
inline std::unique_ptr<ProgressDialog> MakeProgress(
    const TranslatableString& title,
    const TranslatableString& message,
    unsigned flags = (ProgressShowStop | ProgressShowCancel),
    const TranslatableString& remainingLabelText = {})
{
    if (auto p = Get()) {
        return p->DoMakeProgress(title, message, flags, remainingLabelText);
    } else {
        return nullptr;
    }
}

//! Create and display a progress dialog (return nullptr if Services not installed)
/*!
 This function makes a "generic" progress dialog, for the case when time
 to completion cannot be estimated, but some indication of progress is still
 given
 */
inline std::unique_ptr<GenericProgressDialog> MakeGenericProgress(
    const WindowPlacement& placement,
    const TranslatableString& title, const TranslatableString& message,
    int style = (ProgressAppModal | ProgressShowElapsedTime | ProgressSmooth))
{
    if (auto p = Get()) {
        return p->DoMakeGenericProgress(placement, title, message, style);
    } else {
        return nullptr;
    }
}

/*!
 * @brief Helper for the update of a task's progress bar when this task is made
 * of a range's subtasks.
 * @details For each item from `first` till `last`, forwards the item as
 * argument to `action`, as well as a progress reporter that will contribute by
 * a fraction to the parent progress reporter. This fraction is the inverse of
 * the number of elements in the range.
 */
template<typename ItType, typename FnType>
void SplitProgress(
    ItType first, ItType last, FnType action, ProgressReporter parent)
{
    auto count = 0;
    const auto numIters = std::distance(first, last);
    if (numIters == 0) {
        return;
    }
    const ProgressReporter child
        =parent ? [&](double progress) {
        parent((count + progress) / numIters);
        }
    : ProgressReporter {};

    for (; first != last; ++first) {
        action(*first, child);
        ++count;
    }
}

//! Display a dialog with radio buttons.
/*!
 @return zero-based index of the chosen button, or -1 if Services not installed.
 @param message main message in the dialog
 @param title dialog title
 @param buttons labels for individual radio buttons
 @param boxMsg label for the group of buttons
 @param helpPage identifies a manual page
 @param log whether to add a "Show Log for Details" push button
 */
inline int ShowMultiDialog(const TranslatableString& message,
                           const TranslatableString& title,
                           const TranslatableStrings& buttons,
                           const ManualPageID& helpPage,
                           const TranslatableString& boxMsg, bool log)
{
    if (auto p = Get()) {
        return p->DoMultiDialog(message, title, buttons, helpPage, boxMsg, log);
    } else {
        return -1;
    }
}

//! Find the window that is accepting keyboard input, if any
/*!
 @post `result: result != nullptr` (but may point to an empty WindowPlacement)
 */
inline std::unique_ptr<WindowPlacement> FindFocus()
{
    if (auto p = Get()) {
        if (auto result = p->DoFindFocus()) {
            return result;
        }
    }
    return std::make_unique<WindowPlacement>();
}

//! Set the window that accepts keyboard input
inline void SetFocus(const WindowPlacement& focus)
{
    if (auto p = Get()) {
        p->DoSetFocus(focus);
    }
}

//! Whether using a right-to-left language layout
inline bool IsUsingRtlLayout()
{
    if (auto p = Get()) {
        return p->IsUsingRtlLayout();
    }
    return false;
}

//! Whether the current thread is the UI thread
inline bool IsUiThread()
{
    if (auto p = Get()) {
        return p->IsUiThread();
    }
    return true;
}

#define ASSERT_MAIN_THREAD() \
    assert(                     \
        BasicUI::IsUiThread() && \
        "This function should only be called on the main thread")

//! @}
}

#endif
