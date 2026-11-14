/**********************************************************************

  Audacity: A Digital Audio Editor

  @file PlugFrame.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <wx/wx.h>
#include <pluginterfaces/gui/iplugview.h>

namespace internal {
namespace x11 {
class PlugFrame final : public Steinberg::IPlugFrame
{
    wxWeakRef<wxWindow> mWindow;
    bool mInitialized{ false };

    Steinberg::IPtr<Steinberg::Linux::IRunLoop> mRunLoop;
public:

    PlugFrame(Steinberg::Linux::IRunLoop* runLoop, wxWindow* window);
    virtual ~PlugFrame();

    void init(Steinberg::IPlugView* view, Steinberg::ViewRect* size);

    Steinberg::tresult PLUGIN_API resizeView(Steinberg::IPlugView* view, Steinberg::ViewRect* newSize) override;

    DECLARE_FUNKNOWN_METHODS
};
}
}
