/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3ParametersWindow.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <wx/scrolwin.h>
#include <pluginterfaces/base/smartpointer.h>
#include <pluginterfaces/vst/vsttypes.h>

namespace Steinberg
{
   namespace Vst
   {
      class IComponentHandler;
      class IEditController;
   }
}

class wxStaticText;

/**
 * \brief "Plain" plugin UI, contains a list of parameter controls and values.
 */
class VST3ParametersWindow : public wxScrolledWindow
{
   const Steinberg::IPtr<Steinberg::Vst::IEditController> mEditController;
   const Steinberg::IPtr<Steinberg::Vst::IComponentHandler> mComponentHandler;
public:

   /*!
   *  \brief Creates VST3ParametersWindow inside parent.
    * \param parent Where all parameter controls will be placed
    * \param editController Used to read current values
    * \param handler Used to report parameter changes 
    */
   static VST3ParametersWindow* Setup(wxWindow& parent,
      Steinberg::Vst::IEditController& editController,
      Steinberg::Vst::IComponentHandler& componentHandler);

   VST3ParametersWindow(wxWindow *parent,
                      Steinberg::Vst::IEditController& editController,
                      Steinberg::Vst::IComponentHandler& handler,
                      wxWindowID id = wxID_ANY,
                      const wxPoint& pos = wxDefaultPosition,
                      const wxSize& size = wxDefaultSize,
                      long style = wxScrolledWindowStyle,
                      const wxString& name = wxPanelNameStr);

private:

   void UpdateParameterValueText(wxStaticText* text, Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue normalizedValue, const wxString& units);

   void UpdateParameter(Steinberg::Vst::ParamID id, Steinberg::Vst::ParamValue normalizedValue);
   
};
