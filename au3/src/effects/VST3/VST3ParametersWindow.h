/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3ParametersWindow.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <unordered_map>
#include <wx/scrolwin.h>
#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/vsttypes.h>

namespace Steinberg {
namespace Vst {
class IComponentHandler;
class IEditController;
}
}

class wxStaticText;
class VST3ParameterControl;

/**
 * \brief "Plain" plugin UI, contains a list of parameter controls and values.
 */
class VST3ParametersWindow : public wxScrolledWindow
{
    const Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
    const Steinberg::IPtr<Steinberg::Vst::IComponentHandler> mComponentHandler;
    std::unordered_map<Steinberg::Vst::ParamID, VST3ParameterControl*> mControls;
    std::unordered_map<Steinberg::Vst::ParamID, VST3ParameterControl*> mLabels;
public:

    /*!
    *  \brief Creates VST3ParametersWindow inside parent.
     * \param parent Where all parameter controls will be placed
     * \param editController Used to read current values
     * \param handler Used to report parameter changes
     */
    static VST3ParametersWindow* Setup(wxWindow& parent, Steinberg::Vst::IEditController& editController,
                                       Steinberg::Vst::IComponentHandler& componentHandler);

    VST3ParametersWindow(wxWindow* parent, Steinberg::Vst::IEditController& editController, Steinberg::Vst::IComponentHandler& handler,
                         wxWindowID id = wxID_ANY, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize,
                         long style = wxScrolledWindowStyle, const wxString& name = wxPanelNameStr);

    //Updates all controls to match current state of the IEditController
    void ReloadParameters();

    //Updates individual parameter, useful during playback
    //(to update automated parameters or meters)
    void UpdateParameter(Steinberg::Vst::ParamID paramId);

private:

    void RegisterParameterControl(VST3ParameterControl* control);
    void RegisterParameterLabel(VST3ParameterControl* label);

    void OnParameterValueChanged(const wxCommandEvent& evt);
};
