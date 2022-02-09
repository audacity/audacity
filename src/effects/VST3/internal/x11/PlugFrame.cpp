/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PlugFrame.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "PlugFrame.h"

using namespace internal::x11;

IMPLEMENT_REFCOUNT(PlugFrame)

Steinberg::tresult PlugFrame::queryInterface (const ::Steinberg::TUID _iid, void** obj)
{
   QUERY_INTERFACE (_iid, obj, Steinberg::FUnknown::iid, Steinberg::IPlugFrame);
   QUERY_INTERFACE (_iid, obj, Steinberg::IPlugFrame::iid, Steinberg::IPlugFrame);
   //As VST3 documentation states, IPlugFrame also has to provide
   //reference to the Steinberg::Linux::IRunLoop implementation.
   if (mRunLoop && Steinberg::FUnknownPrivate::iidEqual (_iid, Steinberg::Linux::IRunLoop::iid))
   {
      mRunLoop->addRef();
      *obj = static_cast<Steinberg::Linux::IRunLoop*>(mRunLoop.get());
      return ::Steinberg::kResultOk;
   }
   *obj = nullptr;
   return ::Steinberg::kNoInterface;   
}

PlugFrame::PlugFrame(Steinberg::Linux::IRunLoop* runLoop, wxWindow* window) 
   : mWindow(window), mRunLoop(runLoop)
{
   FUNKNOWN_CTOR;
}
PlugFrame::~PlugFrame()
{
   FUNKNOWN_DTOR;
}

Steinberg::tresult PlugFrame::resizeView(Steinberg::IPlugView* view, Steinberg::ViewRect* newSize)
{
   const auto fixedSize = view->canResize() == Steinberg::kResultFalse;
   if(UpdateSize({newSize->getWidth(), newSize->getHeight()}, fixedSize))
      return Steinberg::kResultTrue;
   return Steinberg::kResultFalse;
}

bool PlugFrame::UpdateSize(const wxSize& newSize, bool fixed)
{
   if(auto window = mWindow.get())
   {
      //Wrapper (x11::SocketWindow) geometry needs to be updated too
      auto wrapper = window->GetChildren()[0];
      if(fixed)
      {
         //Update min/max if plugin window has fixed size
         //but for some reason resize was requested
         window->SetMinSize(newSize);
         window->SetMaxSize(newSize);
         wrapper->SetMinSize(newSize);
         wrapper->SetMaxSize(newSize);
      }
      window->SetSize(newSize);
      wrapper->SetSize(newSize);
      return true;
   }
   return false;
}
