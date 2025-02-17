/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file AudacityException.cpp
  @brief Implements AudacityException and related

  Paul Licameli

***********************************************************************/

#include "AudacityException.h"

#include <wx/atomic.h>

#include "BasicUI.h"

AudacityException::~AudacityException()
{
}

void AudacityException::EnqueueAction(
    std::exception_ptr pException,
    std::function<void(AudacityException*)> delayedHandler)
{
    BasicUI::CallAfter([
                           pException = std::move(pException), delayedHandler = std::move(delayedHandler)
                       ] {
        try {
            std::rethrow_exception(pException);
        }
        catch (AudacityException& e)
        { delayedHandler(&e);
        }
    });
}

wxAtomicInt sOutstandingMessages {};

MessageBoxException::MessageBoxException(
    ExceptionType exceptionType_, const TranslatableString& caption_)
    : caption{caption_}
    , exceptionType{exceptionType_}
{
    if (!caption.empty()) {
        wxAtomicInc(sOutstandingMessages);
    } else {
        // invalidate me
        moved = true;
    }
}

// The class needs a copy constructor to be throwable
// (or will need it, by C++14 rules).  But the copy
// needs to act like a move constructor.  There must be unique responsibility
// for each exception thrown to decrease the global count when it is handled.
MessageBoxException::MessageBoxException(const MessageBoxException& that)
{
    caption = that.caption;
    moved = that.moved;
    helpUrl = that.helpUrl;
    exceptionType = that.exceptionType;
    that.moved = true;
}

MessageBoxException::~MessageBoxException()
{
    if (!moved) {
        // If exceptions are used properly, you should never reach this,
        // because moved should become true earlier in the object's lifetime.
        wxAtomicDec(sOutstandingMessages);
    }
}

SimpleMessageBoxException::~SimpleMessageBoxException()
{
}

TranslatableString SimpleMessageBoxException::ErrorMessage() const
{
    return message;
}

// This is meant to be invoked via wxEvtHandler::CallAfter
void MessageBoxException::DelayedHandlerAction()
{
    if (!moved) {
        // This test prevents accumulation of multiple messages between idle
        // times of the main even loop.  Only the last queued exception
        // displays its message.  We assume that multiple messages have a
        // common cause such as exhaustion of disk space so that the others
        // give the user no useful added information.

        using namespace BasicUI;
        if (wxAtomicDec(sOutstandingMessages) == 0) {
            if (exceptionType != ExceptionType::Internal
                && ErrorHelpUrl().IsEmpty()) {
                // We show BadEnvironment and BadUserAction in a similar way
                ShowMessageBox(
                    ErrorMessage(),
                    MessageBoxOptions {}
                    .Caption(caption.empty() ? DefaultCaption() : caption)
                    .IconStyle(Icon::Error));
            } else {
                using namespace BasicUI;
                auto type = exceptionType == ExceptionType::Internal
                            ? ErrorDialogType::ModalErrorReport : ErrorDialogType::ModalError;
                ShowErrorDialog({},
                                (caption.empty() ? DefaultCaption() : caption),
                                ErrorMessage(),
                                ErrorHelpUrl(),
                                ErrorDialogOptions { type });
            }
        }

        moved = true;
    }
}
