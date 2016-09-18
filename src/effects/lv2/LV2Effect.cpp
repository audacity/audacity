/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_LV2)

#include <cmath>

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/dcbuffer.h>
#include <wx/dialog.h>
#include <wx/dynarray.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "LoadLV2.h"
#include "LV2Effect.h"
#include "../../Internat.h"
#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/wxPanelWrapper.h"

#include "lilv/lilv.h"
#include "suil/suil.h"
#include "lv2/lv2plug.in/ns/ext/instance-access/instance-access.h"
#include "lv2/lv2plug.in/ns/ext/port-groups/port-groups.h"
#include "lv2/lv2plug.in/ns/ext/buf-size/buf-size.h"
#include "lv2/lv2plug.in/ns/ext/parameters/parameters.h"
#include "lv2/lv2plug.in/ns/extensions/ui/ui.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#include "win_gtk.h"
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

#if defined(__WXMAC__)
#include <AppKit/AppKit.h>
#endif

// Define the static URI nodes
#undef URI
#define URI(n, u) LilvNode *LV2Effect::n = NULL;
URILIST

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectMeter
//
///////////////////////////////////////////////////////////////////////////////

class LV2EffectMeter final : public wxWindow
{
public:
   LV2EffectMeter(wxWindow *parent, const LV2Port & ctrl);
   virtual ~LV2EffectMeter();

private:
   void OnErase(wxEraseEvent & evt);
   void OnPaint(wxPaintEvent & evt);
   void OnIdle(wxIdleEvent & evt);
   void OnSize(wxSizeEvent & evt);

private:
   const LV2Port & mCtrl;
   float mLastValue;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LV2EffectMeter, wxWindow)
   EVT_IDLE(LV2EffectMeter::OnIdle)
   EVT_ERASE_BACKGROUND(LV2EffectMeter::OnErase)
   EVT_PAINT(LV2EffectMeter::OnPaint)
   EVT_SIZE(LV2EffectMeter::OnSize)
END_EVENT_TABLE()

LV2EffectMeter::LV2EffectMeter(wxWindow *parent, const LV2Port & ctrl)
:  wxWindow(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxDEFAULT_CONTROL_BORDER),
   mCtrl(ctrl)
{
   mLastValue = -mCtrl.mVal;

   SetBackgroundColour(*wxWHITE);
}

LV2EffectMeter::~LV2EffectMeter()
{
}

void LV2EffectMeter::OnIdle(wxIdleEvent & WXUNUSED(evt))
{
   if (mLastValue != mCtrl.mVal)
   {
      Refresh(false);
   }
}

void LV2EffectMeter::OnErase(wxEraseEvent & WXUNUSED(evt))
{
   // Just ignore it to prevent flashing
}

void LV2EffectMeter::OnPaint(wxPaintEvent & WXUNUSED(evt))
{
   std::unique_ptr<wxDC> dc{ wxAutoBufferedPaintDCFactory(this) };

   // Cache some metrics
   wxRect r = GetClientRect();
   wxCoord x = r.GetLeft();
   wxCoord y = r.GetTop();
   wxCoord w = r.GetWidth();
   wxCoord h = r.GetHeight();

   // These use unscaled value, min, and max
   float val = mCtrl.mVal;
   if (val > mCtrl.mMax)
   {
      val = mCtrl.mMax;
   }
   if (val < mCtrl.mMin)
   {
      val = mCtrl.mMin;
   }
   val -= mCtrl.mMin;

   // Setup for erasing the background
   dc->SetPen(*wxTRANSPARENT_PEN);
   dc->SetBrush(wxColour(100, 100, 220));

   dc->Clear();
   dc->DrawRectangle(x, y, (w * (val / fabs(mCtrl.mMax - mCtrl.mMin))), h);

   mLastValue = mCtrl.mVal;
}

void LV2EffectMeter::OnSize(wxSizeEvent & WXUNUSED(evt))
{
   Refresh(false);
}

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectSettingsDialog
//
///////////////////////////////////////////////////////////////////////////////

class LV2EffectSettingsDialog final : public wxDialogWrapper
{
public:
   LV2EffectSettingsDialog(wxWindow *parent, LV2Effect *effect);
   virtual ~LV2EffectSettingsDialog();

   void PopulateOrExchange(ShuttleGui & S);

   void OnOk(wxCommandEvent & evt);

private:
   LV2Effect *mEffect;
   bool mUseLatency;
   bool mUseGUI;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LV2EffectSettingsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LV2EffectSettingsDialog::OnOk)
END_EVENT_TABLE()

LV2EffectSettingsDialog::LV2EffectSettingsDialog(wxWindow *parent, LV2Effect *effect)
:  wxDialogWrapper(parent, wxID_ANY, wxString(_("LV2 Effect Settings")))
{
   mEffect = effect;

   mEffect->mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
   mEffect->mHost->GetSharedConfig(wxT("Settings"), wxT("UseGUI"), mUseGUI, true);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LV2EffectSettingsDialog::~LV2EffectSettingsDialog()
{
}

void LV2EffectSettingsDialog::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         S.StartStatic(_("Latency Compensation"));
         {
            S.AddVariableText(wxString() +
               _("As part of their processing, some LV2 effects must delay returning ") +
               _("audio to Audacity. When not compensating for this delay, you will ") +
               _("notice that small silences have been inserted into the audio. ") +
               _("Enabling this setting will provide that compensation, but it may ") +
               _("not work for all LV2 effects."))->Wrap(650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(_("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(_("Graphical Mode"));
         {
            S.AddVariableText(wxString() +
               _("LV2 effects can have a graphical interface for setting parameter values.") +
               _(" A basic text-only method is also available. ") +
               _(" Reopen the effect for this to take effect."))->Wrap(650);
            S.TieCheckBox(_("Enable &graphical interface"),
                          mUseGUI);
         }
         S.EndStatic();
      }
      S.EndVerticalLay();
   }
   S.EndHorizontalLay();

   S.AddStandardButtons();

   Layout();
   Fit();
   Center();
}

void LV2EffectSettingsDialog::OnOk(wxCommandEvent & WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   mEffect->mHost->SetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency);
   mEffect->mHost->SetSharedConfig(wxT("Settings"), wxT("UseGUI"), mUseGUI);

   EndModal(wxID_OK);
}

///////////////////////////////////////////////////////////////////////////////
//
// LV2Effect
//
///////////////////////////////////////////////////////////////////////////////

enum
{
   ID_Duration = 10000,
   ID_Triggers = 11000,
   ID_Toggles  = 12000,
   ID_Sliders  = 13000,
   ID_Choices  = 14000,
   ID_Texts    = 15000,
};

BEGIN_EVENT_TABLE(LV2Effect, wxEvtHandler)
   EVT_COMMAND_RANGE(ID_Triggers, ID_Triggers + 999, wxEVT_COMMAND_BUTTON_CLICKED,   LV2Effect::OnTrigger)
   EVT_COMMAND_RANGE(ID_Toggles,  ID_Toggles + 999,  wxEVT_COMMAND_CHECKBOX_CLICKED, LV2Effect::OnToggle)
   EVT_COMMAND_RANGE(ID_Sliders,  ID_Sliders + 999,  wxEVT_COMMAND_SLIDER_UPDATED,   LV2Effect::OnSlider)
   EVT_COMMAND_RANGE(ID_Choices,  ID_Choices + 999,  wxEVT_COMMAND_CHOICE_SELECTED,  LV2Effect::OnChoice)
   EVT_COMMAND_RANGE(ID_Texts,    ID_Texts + 999,    wxEVT_COMMAND_TEXT_UPDATED,     LV2Effect::OnText)

   EVT_IDLE(LV2Effect::OnIdle)
END_EVENT_TABLE()

#include <wx/arrimpl.cpp>

WX_DEFINE_OBJARRAY(LV2PortArray);

LV2Effect::LV2Effect(const LilvPlugin *plug)
{
   mPlug = plug;

   mHost = NULL;
   mMaster = NULL;
   mProcess = NULL;
   mSuilInstance = NULL;

   mSampleRate = 44100;
   mBlockSize = 512;

   mLatencyPort = -1;
   mLatencyDone = false;
   mLatency = 0.0;

   mDialog = NULL;
   mSliders = NULL;
   mFields = NULL;

   mURIMap = NULL;
   mNumURIMap = 0;
   mOptions = NULL;
   mNumOptions = 0;
   mFeatures = NULL;
   mNumFeatures = 0;

   mIdleFeature = NULL;
   mOptionsInterface = NULL;

   mFactoryPresetsLoaded = false;
}

LV2Effect::~LV2Effect()
{
   if (mURIMap)
   {
      for (int i = 0; i < mNumURIMap; i++)
      {
         free(mURIMap[i]);
      }
      free(mURIMap);
   }

   if (mFeatures)
   {
      for (int i = 0; mFeatures[i] != NULL; i++)
      {
         delete mFeatures[i];
      }
      free(mFeatures);
   }

   if (mOptions)
   {
      free(mOptions);
   }
}

// ============================================================================
// IdentInterface Implementation
// ============================================================================

wxString LV2Effect::GetPath()
{
   return LilvString(lilv_plugin_get_uri(mPlug));
}

wxString LV2Effect::GetSymbol()
{
   return LilvString(lilv_plugin_get_name(mPlug), true);
}

wxString LV2Effect::GetName()
{
   return GetSymbol();
}

wxString LV2Effect::GetVendor()
{
   wxString vendor = LilvString(lilv_plugin_get_author_name(mPlug), true);

   if (vendor.IsEmpty())
   {
      vendor = XO("n/a");
   }

   return vendor;
}

wxString LV2Effect::GetVersion()
{
   return wxT("1.0");
}

wxString LV2Effect::GetDescription()
{
   return XO("n/a");
}

// ============================================================================
// EffectIdentInterface Implementation
// ============================================================================

EffectType LV2Effect::GetType()
{
   if (GetAudioInCount() == 0 && GetAudioOutCount() == 0)
   {
      return EffectTypeNone;
   }

   if (GetAudioInCount() == 0)
   {
      return EffectTypeGenerate;
   }

   if (GetAudioOutCount() == 0)
   {
      return EffectTypeAnalyze;
   }

   return EffectTypeProcess;
}

wxString LV2Effect::GetFamily()
{
   return LV2EFFECTS_FAMILY;
}

bool LV2Effect::IsInteractive()
{
   return mControls.GetCount() != 0;
}

bool LV2Effect::IsDefault()
{
   return false;
}

bool LV2Effect::IsLegacy()
{
   return false;
}

bool LV2Effect::SupportsRealtime()
{
   return GetType() == EffectTypeProcess;
}

bool LV2Effect::SupportsAutomation()
{
   return true;
}

// ============================================================================
// EffectClientInterface Implementation
// ============================================================================

bool LV2Effect::SetHost(EffectHostInterface *host)
{
   mHost = host;

   int numPorts = lilv_plugin_get_num_ports(mPlug);

   // Fail if we don't grok the port types
   for (int i = 0; i < numPorts; i++)
   {
      const LilvPort *port = lilv_plugin_get_port_by_index(mPlug, i);

      if (!lilv_port_is_a(mPlug, port, gAudio) &&
          !lilv_port_is_a(mPlug, port, gControl))
      {
         return false;
      }
   }

   // Allocate buffers for the port indices and the default control values
   float *minimumVals = new float [numPorts];
   float *maximumVals = new float [numPorts];
   float *defaultValues = new float [numPorts];

   // Retrieve the port ranges for all ports (some values may be NaN)
   lilv_plugin_get_port_ranges_float(mPlug,
                                     minimumVals,
                                     maximumVals,
                                     defaultValues);

   // Get info about all ports
   for (int i = 0; i < numPorts; i++)
   {
      const LilvPort *port = lilv_plugin_get_port_by_index(mPlug, i);
      int index = lilv_port_get_index(mPlug, port);

      // Quick check for audio ports
      if (lilv_port_is_a(mPlug, port, gAudio))
      {
         if (lilv_port_is_a(mPlug, port, gInput))
         {
            mAudioInputs.Add(index);
         }
         else if (lilv_port_is_a(mPlug, port, gOutput))
         {
            mAudioOutputs.Add(index);
         }
         continue;
      }

      // Only control ports from this point
      if (!lilv_port_is_a(mPlug, port, gControl))
      {
         continue;
      }

      LV2Port ctrl;
      ctrl.mIndex = index;

      // Get the port name
      ctrl.mSymbol = LilvString(lilv_port_get_symbol(mPlug, port));
      LilvNode *tmpName = lilv_port_get_name(mPlug, port);
      ctrl.mName = LilvString(tmpName);
      lilv_node_free(tmpName);

      // Get any unit descriptor
      LilvNode *unit = lilv_port_get(mPlug, port, gUnit);
      if (unit)
      {
         ctrl.mUnits = LilvString(lilv_world_get(gWorld, unit, gUnitSymbol, NULL));
      }

      // Get the group to which this port belongs or default to the main group
      ctrl.mGroup = wxEmptyString;
      LilvNode *group = lilv_port_get(mPlug, port, gGroup);
      if (group)
      {
         ctrl.mGroup = LilvString(lilv_world_get(gWorld, group, gLabel, NULL));
         if (ctrl.mGroup.IsEmpty())
         {
            ctrl.mGroup = LilvString(lilv_world_get(gWorld, group, gName, NULL));
         }

         if (ctrl.mGroup.IsEmpty())
         {
            ctrl.mGroup = LilvString(group);
         }
      }

      // Add it if not previously done
      if (mGroups.Index(ctrl.mGroup) == wxNOT_FOUND)
      {
         mGroups.Add(ctrl.mGroup);
      }

      // Get the scale points
      LilvScalePoints *points = lilv_port_get_scale_points(mPlug, port);
      LILV_FOREACH(scale_points, j, points)
      {
         const LilvScalePoint *point = lilv_scale_points_get(points, j);

         ctrl.mScaleValues.Add(lilv_node_as_float(lilv_scale_point_get_value(point)));
         ctrl.mScaleLabels.Add(LilvString(lilv_scale_point_get_label(point)));
      }
      lilv_scale_points_free(points);

      // Collect the value and range info
      ctrl.mHasLo = !std::isnan(minimumVals[i]);
      ctrl.mHasHi = !std::isnan(maximumVals[i]);
      ctrl.mMin = ctrl.mHasLo ? minimumVals[i] : 0.0;
      ctrl.mMax = ctrl.mHasHi ? maximumVals[i] : 1.0;
      ctrl.mLo = ctrl.mMin;
      ctrl.mHi = ctrl.mMax;
      ctrl.mDef = !std::isnan(defaultValues[i]) ?
                  defaultValues[i] :
                     ctrl.mHasLo ?
                     ctrl.mLo :
                        ctrl.mHasHi ?
                        ctrl.mHi :
                           0.0;
      ctrl.mVal = ctrl.mDef;

      // Figure out the type of port we have
      if (lilv_port_is_a(mPlug, port, gInput))
      {
         ctrl.mInput = true;
         if (lilv_port_has_property(mPlug, port, gToggled))
         {
            ctrl.mToggle = true;
         }
         else if (lilv_port_has_property(mPlug, port, gEnumeration))
         {
            ctrl.mEnumeration = true;
         }
         else if (lilv_port_has_property(mPlug, port, gInteger))
         {
            ctrl.mInteger = true;
         }
         else if (lilv_port_has_property(mPlug, port, gSampleRate))
         {
            ctrl.mSampleRate = true;
         }

         // Trigger properties can be combined with other types, but it
         // seems mostly to be combined with toggle.  So, we turn the
         // checkbox into a button.
         if (lilv_port_has_property(mPlug, port, gTrigger))
         {
            ctrl.mTrigger = true;
         }

         // We'll make the slider logarithmic
         if (lilv_port_has_property(mPlug, port, gLogarithmic))
         {
            ctrl.mLogarithmic = true;
         }

         if (lilv_port_has_property(mPlug, port, gEnumeration))
         {
            ctrl.mEnumeration = true;
         }

         mControlsMap[ctrl.mIndex] = mControls.GetCount();
         mGroupMap[ctrl.mGroup].Add(mControls.GetCount());
         mControls.Add(ctrl);
      }
      else if (lilv_port_is_a(mPlug, port, gOutput))
      {
         ctrl.mInput = false;
         if (lilv_port_has_property(mPlug, port, gLatency))
         {
            mLatencyPort = i;
         }
         else
         {
            mGroupMap[ctrl.mGroup].Add(mControls.GetCount());
            mControls.Add(ctrl);
         }
      }
      else
      {
         // Just ignore it for now
      }
   }

   delete [] minimumVals;
   delete [] maximumVals;
   delete [] defaultValues;

   // mHost will be null during registration
   if (mHost)
   {
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseGUI"), mUseGUI, true);

      bool haveDefaults;
      mHost->GetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), haveDefaults, false);
      if (!haveDefaults)
      {
         SaveParameters(mHost->GetFactoryDefaultsGroup());
         mHost->SetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), true);
      }

      LoadParameters(mHost->GetCurrentSettingsGroup());
   }

   AddOption(LV2_BUF_SIZE__minBlockLength,
             sizeof(mBlockSize),
             LV2_ATOM__Int,
             &mBlockSize);
   mBlockSizeOption = AddOption(LV2_BUF_SIZE__maxBlockLength,
                                sizeof(mBlockSize),
                                LV2_ATOM__Int,
                                &mBlockSize);
   mSampleRateOption = AddOption(LV2_CORE__sampleRate,
                                 sizeof(mSampleRate),
                                 LV2_ATOM__Double,
                                 &mSampleRate);
   AddOption(NULL, 0, NULL, NULL);

   mUriMapFeature.callback_data = this;
   mUriMapFeature.uri_to_id = LV2Effect::uri_to_id;

   mURIDMapFeature.handle = this;
   mURIDMapFeature.map = LV2Effect::urid_map;

   mURIDUnmapFeature.handle = this;
   mURIDUnmapFeature.unmap = LV2Effect::urid_unmap;

   mUIResizeFeature.handle = this;
   mUIResizeFeature.ui_resize = LV2Effect::ui_resize;

   AddFeature(LV2_UI_PREFIX "makeResident", NULL);
   AddFeature(LV2_UI__noUserResize, NULL);
   AddFeature(LV2_BUF_SIZE__boundedBlockLength, NULL);
   AddFeature(LV2_OPTIONS__options, mOptions);
   AddFeature(LV2_URI_MAP_URI, &mUriMapFeature);
   AddFeature(LV2_URID__map, &mURIDMapFeature);
   AddFeature(LV2_URID__unmap, &mURIDUnmapFeature);
   AddFeature(LV2_UI__resize, &mUIResizeFeature);
   AddFeature(LV2_DATA_ACCESS_URI, &mExtDataFeature);
   mInstanceAccessFeature = AddFeature(LV2_INSTANCE_ACCESS_URI, NULL);
   mParentFeature = AddFeature(LV2_UI__parent, NULL);
   AddFeature(NULL, NULL);
   
   return true;
}

unsigned LV2Effect::GetAudioInCount()
{
   return mAudioInputs.GetCount();
}

unsigned LV2Effect::GetAudioOutCount()
{
   return mAudioOutputs.GetCount();
}

int LV2Effect::GetMidiInCount()
{
   return 0;
}

int LV2Effect::GetMidiOutCount()
{
   return 0;
}

void LV2Effect::SetSampleRate(double rate)
{
   mSampleRate = (double) rate;

   if (mOptionsInterface && mOptionsInterface->set)
   {
      LV2_Options_Option options[2];         // 2 for empty terminating option
      memset(&options, 0, sizeof(options));
      memcpy(&options, mSampleRateOption, sizeof(*mSampleRateOption));

      if (mMaster)
      {
         mOptionsInterface->set(lilv_instance_get_handle(mMaster), options);
      }

      for (size_t i = 0, cnt = mSlaves.GetCount(); i < cnt; i++)
      {
         mOptionsInterface->set(lilv_instance_get_handle(mSlaves[i]), options);
      }
   }
}

size_t LV2Effect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = (int) maxBlockSize;

   if (mOptionsInterface && mOptionsInterface->set)
   {
      LV2_Options_Option options[2];         // 2 for empty terminating option
      memset(&options, 0, sizeof(options));
      memcpy(&options, mBlockSizeOption, sizeof(*mBlockSizeOption));

      if (mMaster)
      {
         mOptionsInterface->set(lilv_instance_get_handle(mMaster), options);
      }

      for (size_t i = 0, cnt = mSlaves.GetCount(); i < cnt; i++)
      {
         mOptionsInterface->set(lilv_instance_get_handle(mSlaves[i]), options);
      }
   }

   return mBlockSize;
}

sampleCount LV2Effect::GetLatency()
{
   if (mUseLatency && mLatencyPort >= 0 && !mLatencyDone)
   {
      mLatencyDone = true;
      return sampleCount( mLatency );
   }

   return 0;
}

size_t LV2Effect::GetTailSize()
{
   return 0;
}

bool LV2Effect::IsReady()
{
   return mMaster != NULL;
}

bool LV2Effect::ProcessInitialize(sampleCount WXUNUSED(totalLen), ChannelNames WXUNUSED(chanMap))
{
   mProcess = InitInstance(mSampleRate);
   if (!mProcess)
   {
      return false;
   }

   lilv_instance_activate(mProcess);

   mLatencyDone = false;

   return true;
}

bool LV2Effect::ProcessFinalize()
{
   if (mProcess)
   {
      lilv_instance_deactivate(mProcess);
      
      FreeInstance(mProcess);
      mProcess = NULL;
   }

   return true;
}

size_t LV2Effect::ProcessBlock(float **inbuf, float **outbuf, size_t size)
{
   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      lilv_instance_connect_port(mProcess, mAudioInputs[p], inbuf[p]);
   }

   for (size_t p = 0, cnt = mAudioOutputs.GetCount(); p < cnt; p++)
   {
      lilv_instance_connect_port(mProcess, mAudioOutputs[p], outbuf[p]);
   }

   lilv_instance_run(mProcess, size);

   return size;
}

bool LV2Effect::RealtimeInitialize()
{
   mMasterIn = new float *[mAudioInputs.GetCount()];
   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      mMasterIn[p] = new float[mBlockSize];
      lilv_instance_connect_port(mMaster, mAudioInputs[p], mMasterIn[p]);
   }

   mMasterOut = new float *[mAudioOutputs.GetCount()];
   for (size_t p = 0, cnt = mAudioOutputs.GetCount(); p < cnt; p++)
   {
      mMasterOut[p] = new float[mBlockSize];
      lilv_instance_connect_port(mMaster, mAudioOutputs[p], mMasterOut[p]);
   }

   lilv_instance_activate(mMaster);

   return true;
}

bool LV2Effect::RealtimeFinalize()
{
   for (size_t i = 0, cnt = mSlaves.GetCount(); i < cnt; i++)
   {
      lilv_instance_deactivate(mSlaves[i]);

      FreeInstance(mSlaves[i]);
   }
   mSlaves.Clear();

   lilv_instance_deactivate(mMaster);

   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      delete [] mMasterIn[p];
   }
   delete [] mMasterIn;
   
   for (size_t p = 0, cnt = mAudioOutputs.GetCount(); p < cnt; p++)
   {
      delete [] mMasterOut[p];
   }
   delete [] mMasterOut;

   return true;
}

bool LV2Effect::RealtimeSuspend()
{
   return true;
}

bool LV2Effect::RealtimeResume()
{
   return true;
}

bool LV2Effect::RealtimeProcessStart()
{
   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      memset(mMasterIn[p], 0, mBlockSize * sizeof(float));
   }

   mNumSamples = 0;

   return true;
}

size_t LV2Effect::RealtimeProcess(int group,
                                       float **inbuf,
                                       float **outbuf,
                                       size_t numSamples)
{
   wxASSERT(group >= 0 && group < (int) mSlaves.GetCount());
   wxASSERT(numSamples <= mBlockSize);

   if (group < 0 || group >= (int) mSlaves.GetCount())
   {
      return 0;
   }

   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      for (decltype(numSamples) s = 0; s < numSamples; s++)
      {
         mMasterIn[p][s] += inbuf[p][s];
      }
   }
   mNumSamples = wxMax(numSamples, mNumSamples);

   LilvInstance *slave = mSlaves[group];

   for (size_t p = 0, cnt = mAudioInputs.GetCount(); p < cnt; p++)
   {
      lilv_instance_connect_port(slave, mAudioInputs[p], inbuf[p]);
   }

   for (size_t p = 0, cnt = mAudioOutputs.GetCount(); p < cnt; p++)
   {
      lilv_instance_connect_port(slave, mAudioOutputs[p], outbuf[p]);
   }

   lilv_instance_run(slave, numSamples);

   return numSamples;
}

bool LV2Effect::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   LilvInstance *slave = InitInstance(sampleRate);
   if (!slave)
   {
      return false;
   }

   lilv_instance_activate(slave);

   mSlaves.Add(slave);

   return true;
}

bool LV2Effect::RealtimeProcessEnd()
{
   lilv_instance_run(mMaster, mNumSamples);

   return true;
}

bool LV2Effect::ShowInterface(wxWindow *parent, bool forceModal)
{
   if (mDialog)
   {
      mDialog->Close(true);
      return false;
   }

   mDialog = mHost->CreateUI(parent, this);
   if (!mDialog)
   {
      return false;
   }

   // Try to give the window a sensible default/minimum size
   mDialog->Layout();
   mDialog->Fit();
   mDialog->SetMinSize(mDialog->GetSize());

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();

      return false;
   }

   bool res = mDialog->ShowModal() != 0;
   mDialog = NULL;

   return res;
}

bool LV2Effect::GetAutomationParameters(EffectAutomationParameters & parms)
{
   for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
   {
      if (mControls[p].mInput)
      {
         if (!parms.Write(mControls[p].mName, mControls[p].mVal))
         {
            return false;
         }
      }
   }

   return true;
}

bool LV2Effect::SetAutomationParameters(EffectAutomationParameters & parms)
{
   // First pass validates values
   for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
   {
      LV2Port & ctrl = mControls[p];
      
      if (ctrl.mInput)
      {
         double d = 0.0;
         if (!parms.Read(ctrl.mName, &d))
         {
            return false;
         }

         // Use unscaled range here
         if (d < ctrl.mMin || d > ctrl.mMax)
         {
            return false;
         }
      }
   }

   // Second pass actually sets the values
   for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
   {
      LV2Port & ctrl = mControls[p];
      
      if (ctrl.mInput)
      {
         double d = 0.0;
         if (!parms.Read(ctrl.mName, &d))
         {
            return false;
         }

         ctrl.mTmp = ctrl.mVal * (ctrl.mSampleRate ? mSampleRate : 1.0);
      }
   }

   return true;
}

// ============================================================================
// EffectUIClientInterface Implementation
// ============================================================================

void LV2Effect::SetHostUI(EffectUIHostInterface *host)
{
   mUIHost = host;
}

bool LV2Effect::PopulateUI(wxWindow *parent)
{
   mParent = parent;

   mParent->PushEventHandler(this);

   mSliders = NULL;
   mFields = NULL;
   mSuilHost = NULL;
   mSuilInstance = NULL;

   mMaster = InitInstance(mSampleRate);
   if (mMaster == NULL)
   {
      wxMessageBox(wxT("Couldn't instantiate effect"));
      return false;
   }
   
   // Determine if the GUI editor is supposed to be used or not
   mHost->GetSharedConfig(wxT("Settings"),
                          wxT("UseGUI"),
                          mUseGUI,
                          true);

   // Until I figure out where to put the "Duration" control in the
   // graphical editor, force usage of plain editor.
   if (GetType() == EffectTypeGenerate)
   {
      mUseGUI = false;
   }

   if (mUseGUI)
   {
      mUseGUI = BuildFancy();
   }

   if (!mUseGUI)
   {
      return BuildPlain();
   }

   return true;
}

bool LV2Effect::IsGraphicalUI()
{
   return mUseGUI;
}

bool LV2Effect::ValidateUI()
{
   if (!mParent->Validate() || !mParent->TransferDataFromWindow())
   {
      return false;
   }

   if (GetType() == EffectTypeGenerate)
   {
      mHost->SetDuration(mDuration->GetValue());
   }

   return true;
}

bool LV2Effect::HideUI()
{
   return true;
}

bool LV2Effect::CloseUI()
{
#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(false);
#endif
#endif

   mParent->RemoveEventHandler(this);

   if (mSliders)
   {
      delete [] mSliders;
      mSliders = NULL;
   }

   if (mFields)
   {
      delete [] mFields;
      mFields = NULL;
   }

   if (mSuilInstance)
   {
      mIdleFeature = NULL;
      suil_instance_free(mSuilInstance);
      mSuilInstance = NULL;
   }

   if (mSuilHost)
   {
      suil_host_free(mSuilHost);
      mSuilHost = NULL;
   }

   if (mMaster)
   {
      FreeInstance(mMaster);
      mMaster = NULL;
   }

   mUIHost = NULL;
   mParent = NULL;
   mDialog = NULL;

   return true;
}

bool LV2Effect::LoadUserPreset(const wxString & name)
{
   if (!LoadParameters(name))
   {
      return false;
   }

   return TransferDataToWindow();
}

bool LV2Effect::SaveUserPreset(const wxString & name)
{
   return SaveParameters(name);
}

wxArrayString LV2Effect::GetFactoryPresets()
{
   if (mFactoryPresetsLoaded)
   {
      return mFactoryPresetNames;
   }

   LilvNodes* presets = lilv_plugin_get_related(mPlug, gPreset);
   if (presets)
   {
      LILV_FOREACH(nodes, i, presets)
      {
         const LilvNode *preset = lilv_nodes_get(presets, i);

         mFactoryPresetUris.Add(LilvString(preset));

         lilv_world_load_resource(gWorld, preset);
   
         LilvNodes *labels = lilv_world_find_nodes(gWorld, preset, gLabel, NULL);
         if (labels)
         {
            const LilvNode *label = lilv_nodes_get_first(labels);

            mFactoryPresetNames.Add(LilvString(label));

            lilv_nodes_free(labels);
         }
         else
         {
            mFactoryPresetNames.Add(LilvString(preset).AfterLast(wxT('#')));
         }
      }
   
      lilv_nodes_free(presets);
   }

   mFactoryPresetsLoaded = true;

   return mFactoryPresetNames;
}

bool LV2Effect::LoadFactoryPreset(int id)
{
   if (id < 0 || id >= (int) mFactoryPresetUris.GetCount())
   {
      return false;
   }

   LilvNode *preset = lilv_new_uri(gWorld, mFactoryPresetUris[id].ToUTF8());
   if (!preset)
   {
      return false;
   }

   LilvState *state = lilv_state_new_from_world(gWorld, &mURIDMapFeature, preset);
   if (state)
   {
      lilv_state_restore(state, mMaster, set_value_func, this, 0, NULL);
   
      lilv_state_free(state);
   
      TransferDataToWindow();
   }

   lilv_node_free(preset);

   return state != NULL;
}

bool LV2Effect::LoadFactoryDefaults()
{
   if (!LoadParameters(mHost->GetFactoryDefaultsGroup()))
   {
      return false;
   }

   return TransferDataToWindow();
}

bool LV2Effect::CanExportPresets()
{
   return false;
}

void LV2Effect::ExportPresets()
{
}

void LV2Effect::ImportPresets()
{
}

bool LV2Effect::HasOptions()
{
   return true;
}

void LV2Effect::ShowOptions()
{
   LV2EffectSettingsDialog dlg(mParent, this);
   if (dlg.ShowModal() == wxID_OK)
   {
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
   }
}

// ============================================================================
// LV2Effect Implementation
// ============================================================================

bool LV2Effect::LoadParameters(const wxString & group)
{
   wxString parms;
   if (!mHost->GetPrivateConfig(group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   EffectAutomationParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return SetAutomationParameters(eap);
}

bool LV2Effect::SaveParameters(const wxString & group)
{
   EffectAutomationParameters eap;
   if (!GetAutomationParameters(eap))
   {
      return false;
   }

   wxString parms;
   if (!eap.GetParameters(parms))
   {
      return false;
   }

   return mHost->SetPrivateConfig(group, wxT("Parameters"), parms);
}

LV2_Options_Option *LV2Effect::AddOption(const char *key, uint32_t size, const char *type, void *value)
{
   int ndx = mNumOptions;

   mNumOptions += 1;
   mOptions = (LV2_Options_Option *) realloc(mOptions, mNumOptions * sizeof(LV2_Options_Option));
   memset(&mOptions[ndx], 0, sizeof(mOptions[ndx]));

   if (key != NULL)
   {
      mOptions[ndx].context = LV2_OPTIONS_INSTANCE;
      mOptions[ndx].subject = 0;
      mOptions[ndx].key = URID_Map(key);
      mOptions[ndx].size = size;
      mOptions[ndx].type = URID_Map(type);
      mOptions[ndx].value = value;
   }

   return &mOptions[ndx];
}

LV2_Feature *LV2Effect::AddFeature(const char *uri, void *data)
{
   int ndx = mNumFeatures;

   mNumFeatures += 1;
   mFeatures = (LV2_Feature **) realloc(mFeatures, mNumFeatures * sizeof(LV2_Feature *));
   mFeatures[ndx] = NULL;

   if (uri != NULL)
   {
      mFeatures[ndx] = new LV2_Feature;
      mFeatures[ndx]->URI = uri;
      mFeatures[ndx]->data = data;
   }

   return mFeatures[ndx];
}

LilvInstance *LV2Effect::InitInstance(float sampleRate)
{
   LilvInstance *handle = lilv_plugin_instantiate(mPlug,
                                                  sampleRate,
                                                  mFeatures);
   if (!handle)
   {
      return NULL;
   }

   mOptionsInterface = (LV2_Options_Interface *)
      lilv_instance_get_extension_data(handle, LV2_OPTIONS__interface);

   SetBlockSize(mBlockSize);
   SetSampleRate(sampleRate);

   for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
   {
      lilv_instance_connect_port(handle,
                                 mControls[p].mIndex,
                                 &mControls[p].mVal);
   }

   if (mLatencyPort >= 0)
   {
      lilv_instance_connect_port(handle, mLatencyPort, &mLatency);
   }

   return handle;
}

void LV2Effect::FreeInstance(LilvInstance *handle)
{
   lilv_instance_free(handle);
}

bool LV2Effect::BuildFancy()
{
   // Set the native UI type
   const char *nativeType =
#if defined(__WXGTK__)
      LV2_UI__GtkUI;
#elif defined(__WXMSW__)
      LV2_UI__WindowsUI;
#elif defined(__WXMAC__)
      LV2_UI__CocoaUI;
#endif
   
   // Determine if the plugin has a supported UI
   const LilvUI *ui = NULL;
   const LilvNode *uiType = NULL;
   LilvUIs *uis = lilv_plugin_get_uis(mPlug);
   if (uis)
   {
      LilvNode *containerType = lilv_new_uri(gWorld, nativeType);
      if (containerType)
      {
         LILV_FOREACH(uis, iter, uis)
         {
            ui = lilv_uis_get(uis, iter);
            if (lilv_ui_is_supported(ui, suil_ui_supported, containerType, &uiType))
            {
               break;
            }
            ui = NULL;
         }

         lilv_node_free(containerType);
      }
   }

   // No usable UI found
   if (ui == NULL)
   {
      lilv_uis_free(uis);
      return false;
   }

   // Use a panel to host the plugins GUI
   // container is owned by mParent, but we may destroy it if there are
   // any errors before completing the build of UI.
   auto container = std::make_unique<wxPanelWrapper>(mParent, wxID_ANY);
   if (!container)
   {
      lilv_uis_free(uis);
      return false;
   }

   {
      auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      wxSizerItem *si = NULL;
      if (vs)
      {
         auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         if (hs)
         {
            si = hs->Add(container.get(), 1, wxCENTER | wxEXPAND);
            vs->Add(hs.release(), 0, wxCENTER);
         }
      }

      if (!si)
      {
         lilv_uis_free(uis);
         return false;
      }

#if defined(__WXGTK__)
      // Make sure the parent has a window
      if (!gtk_widget_get_window(GTK_WIDGET(container->m_wxwindow)))
      {
         gtk_widget_realize(GTK_WIDGET(container->m_wxwindow));
      }

      mParentFeature->data = GTK_WIDGET(container->GetHandle());
#elif defined(__WXMSW__)
      mParentFeature->data = container->GetHandle();
#elif defined(__WXMAC__)
      mParentFeature->data = container->GetHandle();
#endif

      mInstanceAccessFeature->data = lilv_instance_get_handle(mMaster);
      mExtDataFeature.data_access = lilv_instance_get_descriptor(mMaster)->extension_data;

      // Create the suil host
      mSuilHost = suil_host_new(LV2Effect::suil_write_func, NULL, NULL, NULL);
      if (!mSuilHost)
      {
         lilv_uis_free(uis);
         return false;
      }

      mSuilInstance = suil_instance_new(mSuilHost,
         this,
         nativeType,
         lilv_node_as_uri(lilv_plugin_get_uri(mPlug)),
         lilv_node_as_uri(lilv_ui_get_uri(ui)),
         lilv_node_as_uri(uiType),
         lilv_uri_to_path(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui))),
         lilv_uri_to_path(lilv_node_as_uri(lilv_ui_get_binary_uri(ui))),
         mFeatures);

      lilv_uis_free(uis);

      // Bail if the instance (no compatible UI) couldn't be created
      if (!mSuilInstance)
      {
         suil_host_free(mSuilHost);
         mSuilHost = NULL;

         return false;
      }

#if defined(__WXGTK__)
      GtkWidget* widget = GTK_WIDGET(suil_instance_get_widget(mSuilInstance));
      gtk_widget_show_all(widget);

      GtkRequisition sz;
      gtk_widget_size_request(widget, &sz);
      gtk_widget_set_size_request(widget, 1, 1);
      gtk_widget_set_size_request(widget, sz.width, sz.height);

      wxPizza *pizza = WX_PIZZA(container->m_wxwindow);
      pizza->put(widget,
         0, //gtk_pizza_get_xoffset(pizza),
         0, //gtk_pizza_get_yoffset(pizza),
         sz.width,
         sz.height);
      gtk_widget_show_all(GTK_WIDGET(pizza));
      si->SetMinSize(wxSize(sz.width, sz.height));
#elif defined(__WXMSW__)
      HWND widget = (HWND)suil_instance_get_widget(mSuilInstance);
      RECT rect;
      GetWindowRect(widget, &rect);
      si->SetMinSize(wxSize(rect.right - rect.left, rect.bottom - rect.top));
#elif defined(__WXMAC__)
      NSView *view = (NSView *) suil_instance_get_widget(mSuilInstance);
      NSSize sz = [view frame].size;
      si->SetMinSize(sz.width, sz.height);
#endif

      mParent->SetSizerAndFit(vs.release());
      // mParent will guarantee release of the container now.
      container.release();
   }

   mIdleFeature = (const LV2UI_Idle_Interface *)
      suil_instance_extension_data(mSuilInstance, LV2_UI__idleInterface);

   TransferDataToWindow();

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

   return true;
}

bool LV2Effect::BuildPlain()
{
   int numCols = 5;

   // Allocate memory for the user parameter controls
   int ctrlcnt = (int) mControls.GetCount();
   mSliders = new wxSlider *[ctrlcnt];
   mFields = new wxTextCtrl *[ctrlcnt];

   wxSizer *innerSizer;

   wxASSERT(mParent); // To justify safenew
   wxScrolledWindow *const w = safenew wxScrolledWindow(mParent,
      wxID_ANY,
      wxDefaultPosition,
      wxDefaultSize,
      wxVSCROLL | wxTAB_TRAVERSAL);

   {
      auto outerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
      w->SetScrollRate(0, 20);

      // This fools NVDA into not saying "Panel" when the dialog gets focus
      w->SetName(wxT("\a"));
      w->SetLabel(wxT("\a"));

      outerSizer->Add(w, 1, wxEXPAND);

      {
         auto uInnerSizer = std::make_unique<wxBoxSizer>(wxVERTICAL);
         innerSizer = uInnerSizer.get();

         if (GetType() == EffectTypeGenerate)
         {
            // Add the length control
            auto groupSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, _("Generator"));

            auto sizer = std::make_unique<wxBoxSizer>(wxHORIZONTAL);

            wxWindow *item = safenew wxStaticText(w, 0, _("&Duration:"));
            sizer->Add(item, 0, wxALIGN_CENTER | wxALL, 5);
            mDuration = safenew
               NumericTextCtrl(NumericConverter::TIME,
               w,
               ID_Duration,
               mHost->GetDurationFormat(),
               mHost->GetDuration(),
               mSampleRate,
               wxDefaultPosition,
               wxDefaultSize,
               true);
            mDuration->SetName(_("Duration"));
            mDuration->EnableMenu();
            sizer->Add(mDuration, 0, wxALIGN_CENTER | wxALL, 5);

            groupSizer->Add(sizer.release(), 0, wxALIGN_CENTER | wxALL, 5);
            innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
         }

         mGroups.Sort();

         for (size_t i = 0, cnt = mGroups.GetCount(); i < cnt; i++)
         {
            wxString label = mGroups[i];
            if (label.IsEmpty())
            {
               label = _("Effect Settings");
            }
            auto groupSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, label);

            auto gridSizer = std::make_unique<wxFlexGridSizer>(numCols, 5, 5);
            gridSizer->AddGrowableCol(3);

            const wxArrayInt & params = mGroupMap[mGroups[i]];
            for (size_t pi = 0, cnt = params.GetCount(); pi < cnt; pi++)
            {
               int p = params[pi];
               LV2Port & ctrl = mControls[p];
               wxString labelText = ctrl.mName;
               if (!ctrl.mUnits.IsEmpty())
               {
                  labelText += wxT(" (") + ctrl.mUnits + wxT(")");
               }

               if (ctrl.mTrigger)
               {
                  gridSizer->Add(1, 1, 0);

                  wxASSERT(w); // To justify safenew
                  wxButton *b = safenew wxButton(w, ID_Triggers + p, labelText);
                  gridSizer->Add(b, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  continue;
               }

               wxWindow *item = safenew wxStaticText(w, wxID_ANY, labelText + wxT(":"),
                  wxDefaultPosition, wxDefaultSize,
                  wxALIGN_RIGHT);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);

               if (ctrl.mToggle)
               {
                  wxCheckBox *c = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
                  c->SetName(labelText);
                  c->SetValue(ctrl.mVal > 0);
                  gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
               }
               else if (ctrl.mEnumeration)      // Check before integer
               {
                  int s;
                  for (s = (int)ctrl.mScaleValues.GetCount() - 1; s >= 0; s--)
                  {
                     if (ctrl.mVal >= ctrl.mScaleValues[s])
                     {
                        break;
                     }
                  }

                  if (s < 0)
                  {
                     s = 0;
                  }

                  wxChoice *c = safenew wxChoice(w, ID_Choices + p);
                  c->SetName(labelText);
                  c->Append(ctrl.mScaleLabels);
                  c->SetSelection(s);
                  gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
               }
               else if (!ctrl.mInput)
               {
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  LV2EffectMeter *m = safenew LV2EffectMeter(w, ctrl);
                  gridSizer->Add(m, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);
                  gridSizer->Add(1, 1, 0);
               }
               else
               {
                  mFields[p] = safenew wxTextCtrl(w, ID_Texts + p, wxT(""));
                  mFields[p]->SetName(labelText);
                  gridSizer->Add(mFields[p], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);

                  float rate = ctrl.mSampleRate ? mSampleRate : 1.0;

                  ctrl.mVal = ctrl.mDef;
                  ctrl.mLo = ctrl.mMin * rate;
                  ctrl.mHi = ctrl.mMax * rate;
                  ctrl.mTmp = ctrl.mDef * rate;

                  if (ctrl.mInteger)
                  {
                     IntegerValidator<float> vld(&ctrl.mTmp);
                     vld.SetRange(ctrl.mLo, ctrl.mHi);
                     mFields[p]->SetValidator(vld);
                  }
                  else
                  {
                     FloatingPointValidator<float> vld(6, &ctrl.mTmp);
                     vld.SetRange(ctrl.mLo, ctrl.mHi);

                     // Set number of decimal places
                     float range = ctrl.mHi - ctrl.mLo;
                     int style = range < 10 ? NUM_VAL_THREE_TRAILING_ZEROES :
                        range < 100 ? NUM_VAL_TWO_TRAILING_ZEROES :
                        NUM_VAL_ONE_TRAILING_ZERO;
                     vld.SetStyle(style);

                     mFields[p]->SetValidator(vld);
                  }

                  if (ctrl.mHasLo)
                  {
                     wxString str;
                     if (ctrl.mInteger || ctrl.mSampleRate)
                     {
                        str.Printf(wxT("%d"), lrintf(ctrl.mLo));
                     }
                     else
                     {
                        str = Internat::ToDisplayString(ctrl.mLo);
                     }
                     item = safenew wxStaticText(w, wxID_ANY, str);
                     gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);
                  }
                  else
                  {
                     gridSizer->Add(1, 1, 0);
                  }

                  mSliders[p] = safenew wxSlider(w, ID_Sliders + p,
                     0, 0, 1000,
                     wxDefaultPosition,
                     wxSize(150, -1));
                  mSliders[p]->SetName(labelText);
                  gridSizer->Add(mSliders[p], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);

                  if (ctrl.mHasHi)
                  {
                     wxString str;
                     if (ctrl.mInteger || ctrl.mSampleRate)
                     {
                        str.Printf(wxT("%d"), lrintf(ctrl.mHi));
                     }
                     else
                     {
                        str = Internat::ToDisplayString(ctrl.mHi);
                     }
                     item = safenew wxStaticText(w, wxID_ANY, str);
                     gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                  }
                  else
                  {
                     gridSizer->Add(1, 1, 0);
                  }
               }
            }

            groupSizer->Add(gridSizer.release(), 1, wxEXPAND | wxALL, 5);
            innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
         }

         innerSizer->Layout();

         // Calculate the maximum width of all columns (bypass Generator sizer)
         wxArrayInt widths;
         widths.Add(0, numCols);

         size_t cnt = innerSizer->GetChildren().GetCount();
         for (size_t i = (GetType() == EffectTypeGenerate); i < cnt; i++)
         {
            wxSizer *groupSizer = innerSizer->GetItem(i)->GetSizer();
            wxFlexGridSizer *gridSizer = (wxFlexGridSizer *)groupSizer->GetItem((size_t)0)->GetSizer();

            size_t items = gridSizer->GetChildren().GetCount();
            int cols = gridSizer->GetCols();

            for (size_t j = 0; j < items; j++)
            {
               wxSizerItem *item = gridSizer->GetItem(j);
               widths[j % cols] = wxMax(widths[j % cols], item->GetSize().GetWidth());
            }
         }

         // Set each column in all of the groups to the same width.
         for (size_t i = (GetType() == EffectTypeGenerate); i < cnt; i++)
         {
            wxSizer *groupSizer = innerSizer->GetItem(i)->GetSizer();
            wxFlexGridSizer *gridSizer = (wxFlexGridSizer *)groupSizer->GetItem((size_t)0)->GetSizer();

            size_t items = gridSizer->GetChildren().GetCount();
            int cols = gridSizer->GetCols();

            for (size_t j = 0; j < items; j++)
            {
               wxSizerItem *item = gridSizer->GetItem(j);

               int flags = item->GetFlag();
               if (flags & wxEXPAND)
               {
                  continue;
               }

               if (flags & wxALIGN_RIGHT)
               {
                  flags = (flags & ~wxALL) | wxLEFT;
               }
               else
               {
                  flags = (flags & ~wxALL) | wxRIGHT;
               }
               item->SetFlag(flags);

               item->SetBorder(widths[j % cols] - item->GetMinSize().GetWidth());
            }
         }

         w->SetSizer(uInnerSizer.release());
      }

      mParent->SetSizer(outerSizer.release());
   }

   // Try to give the window a sensible default/minimum size
   wxSize sz1 = innerSizer->GetMinSize();
   wxSize sz2 = mParent->GetMinSize();
   w->SetSizeHints(wxSize(-1, wxMin(sz1.y, sz2.y)));

   // And let the parent reduce to the NEW minimum if possible
   mParent->SetSizeHints(w->GetMinSize());

   TransferDataToWindow();

   return true;
}

bool LV2Effect::TransferDataToWindow()
{
   if (mUseGUI)
   {
      if (mSuilInstance)
      {
         for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
         {
            if (mControls[p].mInput)
            {
               suil_instance_port_event(mSuilInstance,
                                        mControls[p].mIndex,
                                        sizeof(float),
                                        0,
                                        &mControls[p].mVal);
            }
         }
      }

      return true;
   }

   for (size_t i = 0, cnt = mGroups.GetCount(); i < cnt; i++)
   {
      const wxArrayInt & params = mGroupMap[mGroups[i]];
      for (size_t pi = 0, cnt = params.GetCount(); pi < cnt; pi++)
      {
         int p = params[pi];
         LV2Port & ctrl = mControls[p];

         if (ctrl.mTrigger)
         {
            continue;
         }

         if (ctrl.mToggle)
         {
            wxCheckBox *c = wxDynamicCast(mParent->FindWindow(ID_Toggles + p), wxCheckBox);
            c->SetValue(ctrl.mVal > 0);
         }
         else if (ctrl.mEnumeration)      // Check before integer
         {
            int s;
            for (s = (int) ctrl.mScaleValues.GetCount() - 1; s >= 0; s--)
            {
               if (ctrl.mVal >= ctrl.mScaleValues[s])
               {
                  break;
               }
            }

            if (s < 0)
            {
               s = 0;
            }

            wxChoice *c = wxDynamicCast(mParent->FindWindow(ID_Choices + p), wxChoice);
            c->SetSelection(s);
         }
         else if (ctrl.mInput)
         {
            ctrl.mTmp = ctrl.mDef * (ctrl.mSampleRate ? mSampleRate : 1.0);
            SetSlider(mSliders[p], ctrl);
         }
      }
   }

   if (!mParent->TransferDataToWindow())
   {
      return false;
   }

   return true;
}

bool LV2Effect::TransferDataFromWindow()
{
   if (!mParent->Validate() || !mParent->TransferDataFromWindow())
   {
      return false;
   }

   return true;
}

void LV2Effect::SetSlider(wxSlider *slider, const LV2Port & ctrl)
{
   float lo = ctrl.mLo;
   float hi = ctrl.mHi;
   float val = ctrl.mTmp;

   if (ctrl.mLogarithmic)
   {
      lo = logf(lo);
      hi = logf(hi);
      val = logf(val);
   }

   slider->SetValue(lrintf((val - lo) / (hi - lo) * 1000.0));
}

void LV2Effect::OnTrigger(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Triggers;

   mControls[p].mVal = mControls[p].mDef;
}

void LV2Effect::OnToggle(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Toggles;

   mControls[p].mVal = evt.GetInt() ? 1.0 : 0.0;
}

void LV2Effect::OnChoice(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Choices;

   mControls[p].mVal = mControls[p].mScaleValues[evt.GetInt()];
}

void LV2Effect::OnText(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Texts;
   LV2Port & ctrl = mControls[p];

   if (mParent->FindWindow(ID_Texts + p)->GetValidator()->TransferFromWindow())
   {
      ctrl.mVal = ctrl.mSampleRate ? ctrl.mTmp / mSampleRate : ctrl.mTmp;

      SetSlider(mSliders[p], mControls[p]);
   }   
}

void LV2Effect::OnSlider(wxCommandEvent & evt)
{
   int p = evt.GetId() - ID_Sliders;
   LV2Port & ctrl = mControls[p];

   float lo = ctrl.mLo;
   float hi = ctrl.mHi;

   if (ctrl.mLogarithmic)
   {
      lo = logf(lo);
      hi = logf(hi);
   }

   ctrl.mTmp = ((float) evt.GetInt() / 1000.0) * (hi - lo) + lo;
   ctrl.mTmp = ctrl.mLogarithmic ? expf(ctrl.mTmp) : ctrl.mTmp;

   ctrl.mTmp = ctrl.mTmp < ctrl.mLo ? ctrl.mLo : ctrl.mTmp;
   ctrl.mTmp = ctrl.mTmp > ctrl.mHi ? ctrl.mHi : ctrl.mTmp;

   ctrl.mVal = ctrl.mSampleRate ? ctrl.mTmp / mSampleRate : ctrl.mTmp;

   mParent->FindWindow(ID_Texts + p)->GetValidator()->TransferToWindow();
}

void LV2Effect::OnIdle(wxIdleEvent & WXUNUSED(evt))
{
   if (mIdleFeature)
   {
      mIdleFeature->idle(suil_instance_get_handle(mSuilInstance));
   }
}

// ============================================================================
// Feature handlers
// ============================================================================

// static callback
uint32_t LV2Effect::uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                              const char *WXUNUSED(map),
                              const char *uri)
{
   return ((LV2Effect *)callback_data)->URID_Map(uri);
}

// static callback
LV2_URID LV2Effect::urid_map(LV2_URID_Map_Handle handle, const char *uri)
{
   return ((LV2Effect *)handle)->URID_Map(uri);
}

LV2_URID LV2Effect::URID_Map(const char *uri)
{
   for (int i = 0; i < mNumURIMap; i++)
   {
      if (strcmp(mURIMap[i], uri) == 0)
      {
         return i + 1;
      }
   }

   mNumURIMap += 1;
   mURIMap = (char **) realloc(mURIMap, mNumURIMap * sizeof(char*));
   mURIMap[mNumURIMap - 1] = strdup(uri);

   return mNumURIMap;
}

// static callback
const char *LV2Effect::urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid)
{
   return ((LV2Effect *)handle)->URID_Unmap(urid);
}

const char *LV2Effect::URID_Unmap(LV2_URID urid)
{
   if (urid > 0 && urid <= (LV2_URID) mNumURIMap)
   {
      return mURIMap[urid - 1];
   }

   return NULL;
}

// static callback
int LV2Effect::ui_resize(LV2UI_Feature_Handle handle, int width, int height)
{
   return ((LV2Effect *)handle)->UIResize(width, height);
}

int LV2Effect::UIResize(int WXUNUSED(width), int WXUNUSED(height))
{
#if 0
   // Nothing to do yet
#endif
   return 1;
}

// static callback
void LV2Effect::suil_write_func(SuilController controller,
                                uint32_t       port_index,
                                uint32_t       buffer_size,
                                uint32_t       protocol,
                                const void     *buffer)
{
   ((LV2Effect *)controller)->UIWrite(port_index, buffer_size, protocol, buffer);
}

void LV2Effect::UIWrite(uint32_t port_index,
                        uint32_t buffer_size,
                        uint32_t protocol,
                        const void *buffer)
{
   if (protocol != 0 || buffer_size != sizeof(float))
   {
      return;
   }

   wxLongToLongHashMap::iterator it = mControlsMap.find(port_index);
   if (it != mControlsMap.end())
   {
      mControls[(it->second)].mVal = *((const float *)buffer);
   }
}

// static callback
void LV2Effect::set_value_func(const char *port_symbol,
                               void       *user_data,
                               const void *value,
                               uint32_t   size,
                               uint32_t   type)
{
   ((LV2Effect *)user_data)->SetPortValue(port_symbol, value, size, type);
}

void LV2Effect::SetPortValue(const char *port_symbol,
                             const void *value,
                             uint32_t   size,
                             uint32_t   type)
{
   wxString symbol = wxString::FromUTF8(port_symbol);
   LV2_URID Bool = URID_Map(lilv_node_as_string(gBool));
   LV2_URID Double = URID_Map(lilv_node_as_string(gDouble));
   LV2_URID Float = URID_Map(lilv_node_as_string(gFloat));
   LV2_URID Int = URID_Map(lilv_node_as_string(gInt));
   LV2_URID Long = URID_Map(lilv_node_as_string(gLong));

   for (size_t p = 0, cnt = mControls.GetCount(); p < cnt; p++)
   {
      if (mControls[p].mSymbol.IsSameAs(symbol))
      {
         if (type == Bool && size == sizeof(bool))
         {
            mControls[p].mVal = (float) *((const bool *) value) ? 1.0f : 0.0f;
         }
         else if (type == Double && size == sizeof(double))
         {
            mControls[p].mVal = (float) *((const double *) value);
         }
         else if (type == Float && size == sizeof(float))
         {
            mControls[p].mVal = (float) *((const float *) value);
         }
         else if (type == Int && size == sizeof(int32_t))
         {
            mControls[p].mVal = (float) *((const int32_t *) value);
         }
         else if (type == Long && size == sizeof(int64_t))
         {
            mControls[p].mVal = (float) *((const int64_t *) value);
         }

         break;
      }
   }
}

#endif
