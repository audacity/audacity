/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SocketWindow.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "SocketWindow.h"
#include "RunLoop.h"
#include "PlugFrame.h"

using namespace internal::x11;

void SocketWindow::OnMap(GtkWidget* widget, gpointer data)
{
   using namespace Steinberg;

   auto self = reinterpret_cast<SocketWindow*>(data);

   static auto runLoop = [&]() {
      auto display = GDK_WINDOW_XDISPLAY(gtk_widget_get_window(widget));
      return owned(safenew RunLoop(display));
   }();

   auto frame = owned(safenew PlugFrame(runLoop.get(), self->GetParent()));
   if(self->mPlugView->setFrame(frame) == Steinberg::kResultOk)
   {
      self->mPlugView->attached(
         (void*)gtk_socket_get_id(GTK_SOCKET(widget)), 
         Steinberg::kPlatformTypeX11EmbedWindowID
      );
   }
}

SocketWindow::SocketWindow(wxWindow* parent, wxWindowID winid, Steinberg::IPlugView* plugView)
   : wxNativeWindow(parent, winid, gtk_socket_new()), mPlugView(plugView)
{
   gtk_signal_connect(GTK_OBJECT(GetHandle()), "map", G_CALLBACK(&SocketWindow::OnMap), this);
}
