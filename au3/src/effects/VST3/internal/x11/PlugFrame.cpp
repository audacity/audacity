/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PlugFrame.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "PlugFrame.h"
#include <wx/wupdlock.h>

using namespace internal::x11;

IMPLEMENT_REFCOUNT(PlugFrame)

Steinberg::tresult PlugFrame::queryInterface(const ::Steinberg::TUID _iid, void** obj)
{
    QUERY_INTERFACE(_iid, obj, Steinberg::FUnknown::iid, Steinberg::IPlugFrame);
    QUERY_INTERFACE(_iid, obj, Steinberg::IPlugFrame::iid, Steinberg::IPlugFrame);
    //As VST3 documentation states, IPlugFrame also has to provide
    //reference to the Steinberg::Linux::IRunLoop implementation.
    if (mRunLoop && Steinberg::FUnknownPrivate::iidEqual(_iid, Steinberg::Linux::IRunLoop::iid)) {
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

void PlugFrame::init(Steinberg::IPlugView* view, Steinberg::ViewRect* size)
{
    if (!mInitialized) {
        resizeView(view, size);
    }
}

Steinberg::tresult PlugFrame::resizeView(Steinberg::IPlugView* view, Steinberg::ViewRect* viewRect)
{
    if (auto window = mWindow.get()) {
        auto size = wxSize(viewRect->getWidth(), viewRect->getHeight());

        auto topWindow = wxGetTopLevelParent(window);
        wxWindowUpdateLocker windowUpdateLocker(topWindow);

        window->SetInitialSize(size);
        //Wrapper (x11::SocketWindow) geometry needs to be updated too
        window->GetChildren()[0]->SetInitialSize(size);

        topWindow->SetMinSize(wxDefaultSize);
        topWindow->Fit();
        topWindow->SetMinSize(topWindow->GetSize());

        if (!mInitialized) {
            topWindow->Center();
            mInitialized = true;
        }

        return view->onSize(viewRect);
    }
    return Steinberg::kResultFalse;
}
