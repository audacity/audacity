/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PlugFrame.cpp

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#include "PlugFrame.h"
#include <wx/wupdlock.h>

internal::PlugFrame::PlugFrame(wxWindow* window)
    : mWindow(window)
{
    FUNKNOWN_CTOR
}

internal::PlugFrame::~PlugFrame()
{
    FUNKNOWN_DTOR;
}

void internal::PlugFrame::init(Steinberg::IPlugView* view, Steinberg::ViewRect* size)
{
    if (!mInitialized) {
        resizeView(view, size);
    }
}

Steinberg::tresult internal::PlugFrame::resizeView(Steinberg::IPlugView* view, Steinberg::ViewRect* newSize)
{
    if (auto window = mWindow.get()) {
        auto topWindow = wxGetTopLevelParent(window);

        wxWindowUpdateLocker windowUpdateLocker(topWindow);

        window->SetInitialSize({ newSize->getWidth(), newSize->getHeight() });

        topWindow->SetMinSize(wxDefaultSize);
        topWindow->Fit();
        topWindow->SetMinSize(topWindow->GetSize());

        if (!mInitialized) {
            mInitialized = true;
            topWindow->Center();
        }
        return view->onSize(newSize);
    }
    return Steinberg::kResultFalse;
}

IMPLEMENT_FUNKNOWN_METHODS(internal::PlugFrame, Steinberg::IPlugFrame, Steinberg::IPlugFrame::iid)
