/**********************************************************************

  Audacity: A Digital Audio Editor

  CommandManagerWindowClasses.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_COMMAND_MANAGER_WINDOW_CLASSES__
#define __AUDACITY_COMMAND_MANAGER_WINDOW_CLASSES__

/*
 \brief By default, windows when focused override the association of the digits
 and certain navigation keys with commands, but certain windows do not, and
 those inherit this class.
 */
struct AUDACITY_DLL_API NonKeystrokeInterceptingWindow
{
    virtual ~NonKeystrokeInterceptingWindow();
};

/*
 \brief Top-level windows that do redirect keystrokes to the associated
 project's CommandManager inherit this class.
*/
struct AUDACITY_DLL_API TopLevelKeystrokeHandlingWindow
{
    virtual ~TopLevelKeystrokeHandlingWindow();

    //!By default returns true, meaning that keystrokes
    //!should be handled by associated CommandManager.
    virtual bool HandleCommandKeystrokes();
};

#endif
