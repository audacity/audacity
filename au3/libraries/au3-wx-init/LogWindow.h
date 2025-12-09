/**********************************************************************

Audacity: A Digital Audio Editor

LogWindow.h

Paul Licameli split from AudacityLogger.h

**********************************************************************/

#ifndef __AUDACITY_LOG_WINDOW__
#define __AUDACITY_LOG_WINDOW__

//! Maintains the unique logging window which displays debug information
class WX_INIT_API LogWindow
{
public:
    //! Show or hide the unique logging window; create it on demand the first time it is shown
    static void Show(bool show = true);
    //! Destroys the log window (if any)
    static void Destroy();
};

#endif
