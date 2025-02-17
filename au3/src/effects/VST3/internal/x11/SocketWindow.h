/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SocketWindow.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <glib.h>

#include <wx/nativewin.h>

#include <pluginterfaces/base/smartpointer.h>

namespace Steinberg {
class IPlugView;
}

namespace internal {
namespace x11 {
//!Wrapper for GtkSocket object, which provides X window mapping via XEmbed protocol
class SocketWindow : public wxNativeWindow
{
    Steinberg::IPtr<Steinberg::IPlugView> mPlugView;

    static void OnMap(GtkWidget* widget, gpointer data);
public:
    SocketWindow(wxWindow* parent, wxWindowID winid, Steinberg::IPlugView* plugView);
};
}
}
