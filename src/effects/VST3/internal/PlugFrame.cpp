/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PlugFrame.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "PlugFrame.h"

internal::PlugFrame::PlugFrame(wxWindow* window)
   : mWindow(window)
{
   FUNKNOWN_CTOR
}

internal::PlugFrame::~PlugFrame()
{
   FUNKNOWN_DTOR;
}

static void UpdateWindowSize(wxWindow* window, const wxSize& newSize, bool fixed)
{
   if(fixed)
   {
      //Update min/max if plugin window has fixed size
      //but for some reason resize was requested
      window->SetMinSize(newSize);
      window->SetMaxSize(newSize);
   }
   window->SetSize(newSize);
}

Steinberg::tresult internal::PlugFrame::resizeView(Steinberg::IPlugView* view, Steinberg::ViewRect* newSize)
{
   if(auto window = mWindow.get())
   {
      auto fixed = view->canResize() != Steinberg::kResultTrue;
      UpdateWindowSize(window, {newSize->getWidth(), newSize->getHeight()}, fixed);

      return Steinberg::kResultTrue;
   }
   return Steinberg::kResultFalse;
}

IMPLEMENT_FUNKNOWN_METHODS(internal::PlugFrame, Steinberg::IPlugFrame, Steinberg::IPlugFrame::iid)
