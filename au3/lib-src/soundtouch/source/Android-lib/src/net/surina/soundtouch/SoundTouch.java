////////////////////////////////////////////////////////////////////////////////
///
/// Example class that invokes native SoundTouch routines through the JNI
/// interface.
///
/// Author        : Copyright (c) Olli Parviainen
/// Author e-mail : oparviai 'at' iki.fi
/// WWW           : http://www.surina.net
///
////////////////////////////////////////////////////////////////////////////////
//
// $Id: SoundTouch.java 165 2012-12-28 19:55:23Z oparviai $
//
////////////////////////////////////////////////////////////////////////////////

package net.surina.soundtouch;

public final class SoundTouch
{
    // Native interface function that returns SoundTouch version string.
    // This invokes the native c++ routine defined in "soundtouch-jni.cpp".
    public native final String getVersionString();

    // Load the native library upon startup
    static
    {
        System.loadLibrary("soundtouch");
    }
}
