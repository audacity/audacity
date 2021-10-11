/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

**********************************************************************/



#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2Effect.h"
#include "SampleCount.h"

#include <cmath>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dcbuffer.h>
#include <wx/dialog.h>
#include <wx/crt.h>
#include <wx/log.h>
#include <wx/msgqueue.h>

#ifdef __WXMAC__
#include <wx/evtloop.h>
#endif

#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/tokenzr.h>
#include <wx/intl.h>
#include <wx/scrolwin.h>

#include "../../ShuttleGui.h"
#include "../../widgets/valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/wxPanelWrapper.h"
#include "../../widgets/NumericTextCtrl.h"

#include "lilv/lilv.h"
#include "suil/suil.h"
#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/instance-access/instance-access.h"
#include "lv2/port-groups/port-groups.h"
#include "lv2/parameters/parameters.h"
#include "lv2/state/state.h"
#include "lv2/ui/ui.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

// Define a maximum block size in number of samples (not bytes)
#define DEFAULT_BLOCKSIZE 1048576

// Define a reasonable default sequence size in bytes
#define DEFAULT_SEQSIZE 8192

// Define the static URI map
URIDMap LV2Effect::gURIDMap;

// Define the static LILV URI nodes
#undef NODE
#define NODE(n, u) LilvNode *LV2Effect::node_##n = NULL;
NODELIST

// Define the static URIDs
#undef URID
#define URID(n, u) LV2_URID LV2Effect::urid_##n = 0;
URIDLIST

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectMeter
//
///////////////////////////////////////////////////////////////////////////////

class LV2EffectMeter final : public wxWindow
{
public:
   LV2EffectMeter(wxWindow *parent, const LV2ControlPortPtr ctrl);
   virtual ~LV2EffectMeter();

private:
   void OnErase(wxEraseEvent &evt);
   void OnPaint(wxPaintEvent &evt);
   void OnIdle(wxIdleEvent &evt);
   void OnSize(wxSizeEvent &evt);

private:
   const LV2ControlPortPtr mControlPort;
   float mLastValue;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LV2EffectMeter, wxWindow)
   EVT_IDLE(LV2EffectMeter::OnIdle)
   EVT_ERASE_BACKGROUND(LV2EffectMeter::OnErase)
   EVT_PAINT(LV2EffectMeter::OnPaint)
   EVT_SIZE(LV2EffectMeter::OnSize)
END_EVENT_TABLE()

LV2EffectMeter::LV2EffectMeter(wxWindow *parent, const LV2ControlPortPtr port)
:  wxWindow(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxDEFAULT_CONTROL_BORDER),
   mControlPort(port)
{
   mLastValue = -mControlPort->mVal;

   SetBackgroundColour(*wxWHITE);
}

LV2EffectMeter::~LV2EffectMeter()
{
}

void LV2EffectMeter::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();
   if (mLastValue != mControlPort->mVal)
   {
      Refresh(false);
   }
}

void LV2EffectMeter::OnErase(wxEraseEvent &WXUNUSED(evt))
{
   // Just ignore it to prevent flashing
}

void LV2EffectMeter::OnPaint(wxPaintEvent &WXUNUSED(evt))
{
   std::unique_ptr<wxDC> dc {wxAutoBufferedPaintDCFactory(this)};

   // Cache some metrics
   wxRect r = GetClientRect();
   wxCoord x = r.GetLeft();
   wxCoord y = r.GetTop();
   wxCoord w = r.GetWidth();
   wxCoord h = r.GetHeight();

   // These use unscaled value, min, and max
   float val = mControlPort->mVal;
   if (val > mControlPort->mMax)
   {
      val = mControlPort->mMax;
   }
   if (val < mControlPort->mMin)
   {
      val = mControlPort->mMin;
   }
   val -= mControlPort->mMin;

   // Setup for erasing the background
   dc->SetPen(*wxTRANSPARENT_PEN);
   dc->SetBrush(wxColour(100, 100, 220));

   dc->Clear();
   dc->DrawRectangle(x, y, (w * (val / fabs(mControlPort->mMax - mControlPort->mMin))), h);

   mLastValue = mControlPort->mVal;
}

void LV2EffectMeter::OnSize(wxSizeEvent &WXUNUSED(evt))
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

   void PopulateOrExchange(ShuttleGui &S);

   void OnOk(wxCommandEvent &evt);

private:
   LV2Effect *mEffect;
   int mBufferSize;
   bool mUseLatency;
   bool mUseGUI;

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LV2EffectSettingsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LV2EffectSettingsDialog::OnOk)
END_EVENT_TABLE()

LV2EffectSettingsDialog::LV2EffectSettingsDialog(wxWindow *parent, LV2Effect *effect)
:  wxDialogWrapper(parent, wxID_ANY, XO("LV2 Effect Settings"))
{
   mEffect = effect;

   mEffect->mHost->GetSharedConfig(wxT("Settings"), wxT("BufferSize"), mBufferSize, 8192);
   mEffect->mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
   mEffect->mHost->GetSharedConfig(wxT("Settings"), wxT("UseGUI"), mUseGUI, true);

   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

LV2EffectSettingsDialog::~LV2EffectSettingsDialog()
{
}

void LV2EffectSettingsDialog::PopulateOrExchange(ShuttleGui &S)
{
   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, 1);
   {
      S.StartVerticalLay(false);
      {
         // This really shouldn't be required for LV2 plugins because they have the ability
         // to specify their exact requirements in the TTL file and/or to check the host
         // supplied min/max values.  However, I've run across one (Harrison Consoles XT-EQ)
         // that crashed on sizes greater than 8192.
         S.StartStatic(XO("Buffer Size"));
         {
            IntegerValidator<int> vld(&mBufferSize);
            vld.SetRange(8, DEFAULT_BLOCKSIZE);

            S.AddVariableText( XO(
"The buffer size controls the number of samples sent to the effect "
"on each iteration. Smaller values will cause slower processing and "
"some effects require 8192 samples or less to work properly. However "
"most effects can accept large buffers and using them will greatly "
"reduce processing time."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               wxTextCtrl *t;
               t = S.TieNumericTextBox(
                  XXO("&Buffer Size (8 to %d) samples:")
                     .Format( DEFAULT_BLOCKSIZE ),
                  mBufferSize,
                  12);
               t->SetMinSize(wxSize(100, -1));
               t->SetValidator(vld);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Latency Compensation"));
         {
            S.AddVariableText( XO(
"As part of their processing, some LV2 effects must delay returning "
"audio to Audacity. When not compensating for this delay, you will "
"notice that small silences have been inserted into the audio. "
"Enabling this setting will provide that compensation, but it may "
"not work for all LV2 effects."),
               false, 0, 650);

            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieCheckBox(XXO("Enable &compensation"),
                             mUseLatency);
            }
            S.EndHorizontalLay();
         }
         S.EndStatic();

         S.StartStatic(XO("Graphical Mode"));
         {
            S.AddVariableText( XO(
"LV2 effects can have a graphical interface for setting parameter values."
" A basic text-only method is also available. "
" Reopen the effect for this to take effect."),
               false, 0, 650);
            S.TieCheckBox(XXO("Enable &graphical interface"),
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

void LV2EffectSettingsDialog::OnOk(wxCommandEvent &WXUNUSED(evt))
{
   if (!Validate())
   {
      return;
   }

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   mEffect->mHost->SetSharedConfig(wxT("Settings"), wxT("BufferSize"), mBufferSize);
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
   ID_Toggles = 12000,
   ID_Sliders = 13000,
   ID_Choices = 14000,
   ID_Texts = 15000,
   ID_TIMER = 20000,
};

BEGIN_EVENT_TABLE(LV2Effect, wxEvtHandler)
   EVT_COMMAND_RANGE(ID_Triggers, ID_Triggers + 999, wxEVT_COMMAND_BUTTON_CLICKED, LV2Effect::OnTrigger)
   EVT_COMMAND_RANGE(ID_Toggles, ID_Toggles + 999, wxEVT_COMMAND_CHECKBOX_CLICKED, LV2Effect::OnToggle)
   EVT_COMMAND_RANGE(ID_Sliders, ID_Sliders + 999, wxEVT_COMMAND_SLIDER_UPDATED, LV2Effect::OnSlider)
   EVT_COMMAND_RANGE(ID_Choices, ID_Choices + 999, wxEVT_COMMAND_CHOICE_SELECTED, LV2Effect::OnChoice)
   EVT_COMMAND_RANGE(ID_Texts, ID_Texts + 999, wxEVT_COMMAND_TEXT_UPDATED, LV2Effect::OnText)

   EVT_TIMER(ID_TIMER, LV2Effect::OnTimer)
   EVT_IDLE(LV2Effect::OnIdle)
END_EVENT_TABLE()

LV2Effect::LV2Effect(const LilvPlugin *plug)
{
   mPlug = plug;

   mHost = NULL;
   mMaster = NULL;
   mProcess = NULL;
   mSuilInstance = NULL;

   mSampleRate = 44100;
   mBlockSize = DEFAULT_BLOCKSIZE;
   mSeqSize = DEFAULT_SEQSIZE;

   mMinBlockSize = 1;
   mMaxBlockSize = mBlockSize;
   mUserBlockSize = mBlockSize;

   mLatencyPort = -1;
   mLatencyDone = false;
   mRolling = false;
   mActivated = false;

   mDialog = NULL;

   mUIIdleInterface = NULL;
   mUIShowInterface = NULL;

   mAudioIn = 0;
   mAudioOut = 0;
   mMidiIn = 0;
   mMidiOut = 0;

   mControlIn.reset();
   mControlOut.reset();

   mPositionSpeed = 1.0;
   mPositionFrame = 0.0;
   
   mNativeWin = NULL;
   mNativeWinInitialSize = wxDefaultSize;
   mNativeWinLastSize = wxDefaultSize;
   mResizing = false;
#if defined(__WXGTK__)
   mResized = false;
#endif

   mExternalUIHost.plugin_human_id = NULL;
   mExternalWidget = NULL;
   mExternalUIClosed = false;

   mNoResize = false;

   mSupportsNominalBlockLength = false;
   mSupportsSampleRate = false;

   mFactoryPresetsLoaded = false;
}

LV2Effect::~LV2Effect()
{
}

// ============================================================================
// ComponentInterface Implementation
// ============================================================================

PluginPath LV2Effect::GetPath()
{
   return LilvString(lilv_plugin_get_uri(mPlug));
}

ComponentInterfaceSymbol LV2Effect::GetSymbol()
{
   return LilvString(lilv_plugin_get_name(mPlug), true);
}

VendorSymbol LV2Effect::GetVendor()
{
   wxString vendor = LilvString(lilv_plugin_get_author_name(mPlug), true);

   if (vendor.empty())
   {
      return XO("n/a");
   }

   return {vendor};
}

wxString LV2Effect::GetVersion()
{
   return wxT("1.0");
}

TranslatableString LV2Effect::GetDescription()
{
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType LV2Effect::GetType()
{
   if (GetAudioInCount() == 0 && GetAudioOutCount() == 0)
   {
      return EffectTypeTool;
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

EffectFamilySymbol LV2Effect::GetFamily()
{
   return LV2EFFECTS_FAMILY;
}

bool LV2Effect::IsInteractive()
{
   return mControlPorts.size() != 0;
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

   AddOption(urid_SequenceSize, sizeof(mSeqSize), urid_Int, &mSeqSize);
   AddOption(urid_MinBlockLength, sizeof(mMinBlockSize), urid_Int, &mMinBlockSize);
   AddOption(urid_MaxBlockLength, sizeof(mMaxBlockSize), urid_Int, &mMaxBlockSize);

   mBlockSizeOption = AddOption(urid_NominalBlockLength,
                                sizeof(mBlockSize),
                                urid_Int,
                                &mBlockSize);
   mSampleRateOption = AddOption(urid_SampleRate,
                                 sizeof(mSampleRate),
                                 urid_Float,
                                 &mSampleRate);
   AddOption(0, 0, 0, NULL);

   if (!ValidateOptions(lilv_plugin_get_uri(mPlug)))
   {
      return false;
   }

   mUriMapFeature.callback_data = this;
   mUriMapFeature.uri_to_id = LV2Effect::uri_to_id;

   mURIDMapFeature.handle = this;
   mURIDMapFeature.map = LV2Effect::urid_map;

   mURIDUnmapFeature.handle = this;
   mURIDUnmapFeature.unmap = LV2Effect::urid_unmap;

   mUIResizeFeature.handle = this;
   mUIResizeFeature.ui_resize = LV2Effect::ui_resize;

   mLogFeature.handle = this;
   mLogFeature.printf = LV2Effect::log_printf;
   mLogFeature.vprintf = LV2Effect::log_vprintf;

   mExternalUIHost.ui_closed = LV2Effect::ui_closed;

   LilvNode *pluginName = lilv_plugin_get_name(mPlug);
   mExternalUIHost.plugin_human_id = lilv_node_as_string(pluginName);
   lilv_node_free(pluginName);

   AddFeature(LV2_UI__noUserResize, NULL);
   AddFeature(LV2_UI__fixedSize, NULL);
   AddFeature(LV2_UI__idleInterface, NULL);
   AddFeature(LV2_UI__makeResident, NULL);
   AddFeature(LV2_BUF_SIZE__boundedBlockLength, NULL);
   AddFeature(LV2_BUF_SIZE__fixedBlockLength, NULL);
   AddFeature(LV2_OPTIONS__options, mOptions.data());
   AddFeature(LV2_URI_MAP_URI, &mUriMapFeature);
   AddFeature(LV2_URID__map, &mURIDMapFeature);
   AddFeature(LV2_URID__unmap, &mURIDUnmapFeature);
   AddFeature(LV2_UI__resize, &mUIResizeFeature);
   AddFeature(LV2_DATA_ACCESS_URI, &mExtensionDataFeature);
   AddFeature(LV2_LOG__log, &mLogFeature);
   AddFeature(LV2_EXTERNAL_UI__Host, &mExternalUIHost);
   AddFeature(LV2_EXTERNAL_UI_DEPRECATED_URI, &mExternalUIHost);
   // Some plugins specify this as a feature
   AddFeature(LV2_EXTERNAL_UI__Widget, NULL);

   mInstanceAccessFeature = AddFeature(LV2_INSTANCE_ACCESS_URI, NULL);
   mParentFeature = AddFeature(LV2_UI__parent, NULL);

   AddFeature(NULL, NULL);

   if (!ValidateFeatures(lilv_plugin_get_uri(mPlug)))
   {
      return false;
   }

   auto minLength = lilv_world_get(gWorld, lilv_plugin_get_uri(mPlug), node_MinBlockLength, NULL);
   if (minLength)
   {
      if (lilv_node_is_int(minLength))
      {
         int val = lilv_node_as_int(minLength);
         if (mMinBlockSize < val)
         {
            mMinBlockSize = val;
         }
      }
      lilv_node_free(minLength);
   }

   auto maxLength = lilv_world_get(gWorld, lilv_plugin_get_uri(mPlug), node_MaxBlockLength, NULL);
   if (maxLength)
   {
      if (lilv_node_is_int(maxLength))
      {
         int val = lilv_node_as_int(maxLength);
         if (mMaxBlockSize > val)
         {
            mMaxBlockSize = val;
         }
      }
      lilv_node_free(maxLength);
   }

   if (mMinBlockSize > mMaxBlockSize)
   {
      mMaxBlockSize = mMinBlockSize;
   }

   auto numPorts = lilv_plugin_get_num_ports(mPlug);

   // Allocate buffers for the port indices and the default control values
   Floats minimumVals {numPorts};
   Floats maximumVals {numPorts};
   Floats defaultVals {numPorts};

   // Retrieve the port ranges for all ports (some values may be NaN)
   lilv_plugin_get_port_ranges_float(mPlug,
                                     minimumVals.get(),
                                     maximumVals.get(),
                                     defaultVals.get());

   // Get info about all ports
   for (size_t i = 0; i < numPorts; i++)
   {
      const LilvPort *port = lilv_plugin_get_port_by_index(mPlug, i);
      int index = lilv_port_get_index(mPlug, port);

      // It must be input or output, anything else is bogus
      bool isInput;
      if (lilv_port_is_a(mPlug, port, node_InputPort))
      {
         isInput = true;
      }
      else if (lilv_port_is_a(mPlug, port, node_OutputPort))
      {
         isInput = false;
      }
      else
      {
         assert(false);
         return false;
      }

      // Get the port name and symbol
      wxString symbol = LilvString(lilv_port_get_symbol(mPlug, port));
      wxString name = LilvString(lilv_port_get_name(mPlug, port), true);

      // Get the group to which this port belongs or default to the main group
      wxString groupName = wxEmptyString;
      LilvNode *group = lilv_port_get(mPlug, port, node_Group);
      if (group)
      {
         groupName = LilvString(lilv_world_get(gWorld, group, node_Label, NULL), true);
         if (groupName.empty())
         {
            groupName = LilvString(lilv_world_get(gWorld, group, node_Name, NULL), true);
         }

         if (groupName.empty())
         {
            groupName = LilvString(group);
         }

         lilv_node_free(group);
      }
      else
      {
         groupName = _("Effect Settings");
      }

      // Get the latency port
      uint32_t latencyIndex = lilv_plugin_get_latency_port_index(mPlug);

      // Get the ports designation (must be freed)
      LilvNode *designation = lilv_port_get(mPlug, port, node_Designation);

      // Check for audio ports
      if (lilv_port_is_a(mPlug, port, node_AudioPort))
      {
         mAudioPorts.push_back(std::make_shared<LV2AudioPort>(port, index, isInput, symbol, name, groupName));

         isInput ? mAudioIn++ : mAudioOut++;
      }
      // Check for Control ports
      else if (lilv_port_is_a(mPlug, port, node_ControlPort))
      {
         // Add group if not previously done
         if (mGroupMap.find(groupName) == mGroupMap.end())
         {
            mGroups.push_back(groupName);
         }
         mGroupMap[groupName].push_back(mControlPorts.size());

         mControlPorts.push_back(std::make_shared<LV2ControlPort>(port, index, isInput, symbol, name, groupName));
         LV2ControlPortPtr controlPort = mControlPorts.back();

         // Get any unit descriptor
         LilvNode *unit = lilv_port_get(mPlug, port, node_Unit);
         if (unit)
         {
            // Really should use lilv_world_get_symbol()
            LilvNode *symbol = lilv_world_get_symbol(gWorld, unit);
            if (symbol)
            {
               controlPort->mUnits = LilvString(symbol);
               lilv_node_free(symbol);
            }
            lilv_node_free(unit);
         }

         // Get the scale points
         LilvScalePoints *points = lilv_port_get_scale_points(mPlug, port);
         LILV_FOREACH(scale_points, j, points)
         {
            const LilvScalePoint *point = lilv_scale_points_get(points, j);

            controlPort->mScaleValues.push_back(lilv_node_as_float(lilv_scale_point_get_value(point)));
            controlPort->mScaleLabels.push_back(LilvString(lilv_scale_point_get_label(point)));
         }
         lilv_scale_points_free(points);

         // Collect the value and range info
         controlPort->mHasLo = !std::isnan(minimumVals[i]);
         controlPort->mHasHi = !std::isnan(maximumVals[i]);
         controlPort->mMin = controlPort->mHasLo ? minimumVals[i] : 0.0;
         controlPort->mMax = controlPort->mHasHi ? maximumVals[i] : 1.0;
         controlPort->mLo = controlPort->mMin;
         controlPort->mHi = controlPort->mMax;
         controlPort->mDef = !std::isnan(defaultVals[i])
                     ? defaultVals[i]
                     : controlPort->mHasLo
                       ? controlPort->mLo
                       : controlPort->mHasHi
                         ? controlPort->mHi
                         : 0.0;
         controlPort->mVal = controlPort->mDef;
         controlPort->mLst = controlPort->mVal;

         // Figure out the type of port we have
         if (isInput)
         {
            if (lilv_port_has_property(mPlug, port, node_Toggled))
            {
               controlPort->mToggle = true;
            }
            else if (lilv_port_has_property(mPlug, port, node_Enumeration))
            {
               controlPort->mEnumeration = true;
            }
            else if (lilv_port_has_property(mPlug, port, node_Integer))
            {
               controlPort->mInteger = true;
            }
            else if (lilv_port_has_property(mPlug, port, node_SampleRate))
            {
               controlPort->mSampleRate = true;
            }

            // Trigger properties can be combined with other types, but it
            // seems mostly to be combined with toggle.  So, we turn the
            // checkbox into a button.
            if (lilv_port_has_property(mPlug, port, node_Trigger))
            {
               controlPort->mTrigger = true;
            }

            // We'll make the slider logarithmic
            if (lilv_port_has_property(mPlug, port, node_Logarithmic))
            {
               controlPort->mLogarithmic = true;
            }

            if (lilv_port_has_property(mPlug, port, node_Enumeration))
            {
               controlPort->mEnumeration = true;
            }

            mControlPortMap[controlPort->mIndex] = controlPort;
         }
         else
         {
            if (controlPort->mIndex == latencyIndex)
            {
               mLatencyPort = i;
            }
         }
      }
      // Check for atom ports
      else if (lilv_port_is_a(mPlug, port, node_AtomPort))
      {
         mAtomPorts.push_back(std::make_shared<LV2AtomPort>(port, index, isInput, symbol, name, groupName));
         std::shared_ptr<LV2AtomPort> atomPort = mAtomPorts.back();

         atomPort->mMinimumSize = 8192;
         LilvNode *min = lilv_port_get(mPlug, port, node_MinimumSize);
         if (min)
         {
            if (lilv_node_is_int(min))
            {
               uint32_t val = lilv_node_as_int(min);
               if (atomPort->mMinimumSize < val)
               {
                  atomPort->mMinimumSize = val;
               }
            }
            lilv_node_free(min);
         }

         atomPort->mBuffer.resize(atomPort->mMinimumSize);
         atomPort->mRing = zix_ring_new(atomPort->mMinimumSize);
         zix_ring_mlock(atomPort->mRing);

         if (lilv_port_supports_event(mPlug, port, node_Position))
         {
            atomPort->mWantsPosition = true;
         }

         if (lilv_port_supports_event(mPlug, port, node_MidiEvent))
         {
            atomPort->mIsMidi = true;
            (isInput ? mMidiIn : mMidiOut) += 1;
         }

         bool isControl = lilv_node_equals(designation, node_Control);
         if (isInput)
         {
            if (!mControlIn || isControl)
            {
               mControlIn = atomPort;
            }
         }
         else
         {
            if (!mControlOut || isControl)
            {
               mControlOut = atomPort;
            }
         }
      }
      // Check for CV ports
      else if (lilv_port_is_a(mPlug, port, node_CVPort))
      {
         mCVPorts.push_back(std::make_shared<LV2CVPort>(port, index, isInput, symbol, name, groupName));
         std::shared_ptr<LV2CVPort> cvPort = mCVPorts.back();
      
         // Collect the value and range info
         if (!std::isnan(minimumVals[i]))
         {
            cvPort->mHasLo = true;
            cvPort->mMin = minimumVals[i];
         }

         if (!std::isnan(maximumVals[i]))
         {
            cvPort->mHasHi = true;
            cvPort->mMax = maximumVals[i];
         }

         if (!std::isnan(defaultVals[i]))
         {
            cvPort->mDef = defaultVals[i];
         }
         else if (cvPort->mHasLo)
         {
            cvPort->mDef = cvPort->mMin;
         }
         else if (cvPort->mHasHi)
         {
            cvPort->mDef = cvPort->mMax;
         }
      }

      // Free the designation node
      if (designation)
      {
         lilv_node_free(designation);
      }
   }

   // Ignore control designation if one of them is missing
   if ((mControlIn && !mControlOut) || (!mControlIn && mControlOut))
   {
      mControlIn.reset();
      mControlOut.reset();
   }

   // Determine available extensions
   mWantsOptionsInterface = false;
   mWantsWorkerInterface = false;
   mWantsStateInterface = false;

   LilvNodes *extdata = lilv_plugin_get_extension_data(mPlug);
   if (extdata)
   {
      LILV_FOREACH(nodes, i, extdata)
      {
         const LilvNode *node = lilv_nodes_get(extdata, i);
         const char *uri = lilv_node_as_string(node);

         if (strcmp(uri, LV2_OPTIONS__interface) == 0)
         {
            mWantsOptionsInterface = true;
         }
         else if (strcmp(uri, LV2_WORKER__interface) == 0)
         {
            mWantsWorkerInterface = true;
         }
         else if (strcmp(uri, LV2_STATE__interface) == 0)
         {
            mWantsStateInterface = true;
         }
      }
      lilv_nodes_free(extdata);
   }

   // mHost will be null during registration
   if (mHost)
   {
      int userBlockSize;
      mHost->GetSharedConfig(wxT("Settings"), wxT("BufferSize"), userBlockSize, 8192);
      mUserBlockSize = std::max(1, userBlockSize);
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseGUI"), mUseGUI, true);

      mBlockSize = mUserBlockSize;

      bool haveDefaults;
      mHost->GetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), haveDefaults, false);
      if (!haveDefaults)
      {
         SaveParameters(mHost->GetFactoryDefaultsGroup());
         mHost->SetPrivateConfig(mHost->GetFactoryDefaultsGroup(), wxT("Initialized"), true);
      }

      LoadParameters(mHost->GetCurrentSettingsGroup());
   }

   lv2_atom_forge_init(&mForge, &mURIDMapFeature);

   return true;
}

unsigned LV2Effect::GetAudioInCount()
{
   return mAudioIn;
}

unsigned LV2Effect::GetAudioOutCount()
{
   return mAudioOut;
}

int LV2Effect::GetMidiInCount()
{
   return mMidiIn;
}

int LV2Effect::GetMidiOutCount()
{
   return mMidiOut;
}

void LV2Effect::SetSampleRate(double rate)
{
   mSampleRate = (float) rate;

   if (mMaster)
   {
      mMaster->SetSampleRate();
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      mSlaves[i]->SetSampleRate();
   }
}

size_t LV2Effect::SetBlockSize(size_t maxBlockSize)
{
   mBlockSize = std::min(std::min((int)maxBlockSize, mUserBlockSize), mMaxBlockSize);

   if (mBlockSize < mMinBlockSize)
   {
      mBlockSize = mMinBlockSize;
   }
   if (mBlockSize > mMaxBlockSize)
   {
      mBlockSize = mMaxBlockSize;
   }

   if (mMaster)
   {
      mMaster->SetBlockSize();
   }

   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; i++)
   {
      mSlaves[i]->SetBlockSize();
   }

   return mBlockSize;
}

size_t LV2Effect::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount LV2Effect::GetLatency()
{
   if (mUseLatency && mLatencyPort >= 0 && !mLatencyDone)
   {
      mLatencyDone = true;
      return sampleCount(mMaster->GetLatency());
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

   for (auto & port : mCVPorts)
   {
      port->mBuffer.reinit((unsigned) mBlockSize, port->mIsInput);
   }

   lilv_instance_activate(mProcess->GetInstance());
   mActivated = true;

   mLatencyDone = false;

   return true;
}

bool LV2Effect::ProcessFinalize()
{
   if (mProcess)
   {
      FreeInstance(mProcess);
      mProcess = NULL;
   }

   return true;
}

size_t LV2Effect::ProcessBlock(float **inbuf, float **outbuf, size_t size)
{
   wxASSERT(size <= ( size_t) mBlockSize);

   LilvInstance *instance = mProcess->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mAudioPorts)
   {
      lilv_instance_connect_port(instance,
                                 port->mIndex,
                                 (port->mIsInput ? inbuf[i++] : outbuf[o++]));
   }

   // Transfer incoming events from the ring buffer to the event buffer for each
   // atom input port.  These will be made available to each slave in the chain and
   // to the master once all slaves have run.
   //
   // In addition, reset the output Atom ports.
   for (auto & port : mAtomPorts)
   {
      uint8_t *buf = port->mBuffer.data();

      if (port->mIsInput)
      {
         lv2_atom_forge_set_buffer(&mForge,
                                   buf,
                                   port->mBuffer.size());

         LV2_Atom_Forge_Frame seqFrame;
         LV2_Atom_Sequence *seq = ( LV2_Atom_Sequence *)
            lv2_atom_forge_sequence_head(&mForge, &seqFrame, 0);

         if (port->mWantsPosition)
         {
            lv2_atom_forge_frame_time(&mForge, mPositionFrame);

            LV2_Atom_Forge_Frame posFrame;
            lv2_atom_forge_object(&mForge, &posFrame, 0, urid_Position);
            lv2_atom_forge_key(&mForge, urid_Speed);
            lv2_atom_forge_float(&mForge, mPositionSpeed);
            lv2_atom_forge_key(&mForge, urid_Frame);
            lv2_atom_forge_long(&mForge, mPositionFrame);
            lv2_atom_forge_pop(&mForge, &posFrame);
         }

         ZixRing *ring = port->mRing;
         LV2_Atom atom;
         while (zix_ring_read(ring, &atom, sizeof(atom)))
         {
            if (mForge.offset + sizeof(LV2_Atom_Event) + atom.size < mForge.size)
            {
               lv2_atom_forge_frame_time(&mForge, mPositionFrame);

               lv2_atom_forge_write(&mForge, &atom, sizeof(atom));
               zix_ring_read(ring, &mForge.buf[mForge.offset], atom.size);
               mForge.offset += atom.size;
               seq->atom.size += atom.size;
            }
            else
            {
               zix_ring_skip(ring, atom.size);
               wxLogError(wxT("LV2 sequence buffer overflow"));
            }
         }

         lv2_atom_forge_pop(&mForge, &seqFrame);
      }
      else
      {
         port->mBuffer.resize(port->mMinimumSize);
         *(( LV2_Atom *) buf) =
         {
            port->mMinimumSize,
            urid_Chunk
         };
      }
   }

   lilv_instance_run(instance, size);

   mProcess->SendResponses();

   for (auto & port : mAtomPorts)
   {
      if (!port->mIsInput)
      {
         port->mBuffer.resize(port->mMinimumSize);

         LV2_Atom *chunk = ( LV2_Atom *) port->mBuffer.data();
         chunk->size = port->mMinimumSize;
         chunk->type = urid_Chunk;
      }
   }

   return size;
}

bool LV2Effect::RealtimeInitialize()
{
   mMasterIn.reinit(mAudioIn, (unsigned int) mBlockSize);
   mMasterOut.reinit(mAudioOut, (unsigned int) mBlockSize);

   for (auto & port : mCVPorts)
   {
      port->mBuffer.reinit((unsigned) mBlockSize, port->mIsInput);
   }

   lilv_instance_activate(mMaster->GetInstance());
   mActivated = true;

   return true;
}

bool LV2Effect::RealtimeFinalize()
{
   for (auto & slave : mSlaves)
   {
      FreeInstance(slave);
   }
   mSlaves.clear();

   if (mActivated)
   {
      lilv_instance_deactivate(mMaster->GetInstance());
      mActivated = false;
   }

   for (auto & port : mCVPorts)
   {
      port->mBuffer.reset();
   }

   mMasterIn.reset();
   mMasterOut.reset();

   return true;
}

bool LV2Effect::RealtimeAddProcessor(unsigned WXUNUSED(numChannels), float sampleRate)
{
   LV2Wrapper *slave = InitInstance(sampleRate);
   if (!slave)
   {
      return false;
   }

   mSlaves.push_back(slave);

   lilv_instance_activate(slave->GetInstance());
   mActivated = true;

   return true;
}

bool LV2Effect::RealtimeSuspend()
{
   mPositionSpeed = 0.0;
   mPositionFrame = 0.0;
   mRolling = false;

   return true;
}

bool LV2Effect::RealtimeResume()
{
   mPositionSpeed = 1.0;
   mPositionFrame = 0.0;
   mRolling = true;

   return true;
}

bool LV2Effect::RealtimeProcessStart()
{
   int i = 0;
   for (auto & port : mAudioPorts)
   {
      if (port->mIsInput)
      {
         memset(mMasterIn[i++].get(), 0, mBlockSize * sizeof(float));
      }
   }

   mNumSamples = 0;

   // Transfer incoming events from the ring buffer to the event buffer for each
   // atom input port.  These will be made available to each slave in the chain and
   // to the master once all slaves have run.
   //
   // In addition, reset the output Atom ports.
   for (auto & port : mAtomPorts)
   {
      uint8_t *buf = port->mBuffer.data();

      if (port->mIsInput)
      {
         lv2_atom_forge_set_buffer(&mForge,
                                   buf,
                                   port->mBuffer.size());

         LV2_Atom_Forge_Frame seqFrame;
         LV2_Atom_Sequence *seq = (LV2_Atom_Sequence *)
            lv2_atom_forge_sequence_head(&mForge, &seqFrame, 0);

         if (port->mWantsPosition)
         {
            lv2_atom_forge_frame_time(&mForge, mPositionFrame);

            LV2_Atom_Forge_Frame posFrame;
            lv2_atom_forge_object(&mForge, &posFrame, 0, urid_Position);
            lv2_atom_forge_key(&mForge, urid_Speed);
            lv2_atom_forge_float(&mForge, mPositionSpeed);
            lv2_atom_forge_key(&mForge, urid_Frame);
            lv2_atom_forge_long(&mForge, mPositionFrame);
            lv2_atom_forge_pop(&mForge, &posFrame);
         }

         ZixRing *ring = port->mRing;
         LV2_Atom atom;
         while (zix_ring_read(ring, &atom, sizeof(atom)))
         {
            if (mForge.offset + sizeof(LV2_Atom_Event) + atom.size < mForge.size)
            {
               lv2_atom_forge_frame_time(&mForge, mPositionFrame);

               lv2_atom_forge_write(&mForge, &atom, sizeof(atom));
               zix_ring_read(ring, &mForge.buf[mForge.offset], atom.size);
               mForge.offset += atom.size;
               seq->atom.size += atom.size;
            }
            else
            {
               zix_ring_skip(ring, atom.size);
               wxLogError(wxT("LV2 sequence buffer overflow"));
            }
         }
         lv2_atom_forge_pop(&mForge, &seqFrame);
#if 0
         LV2_ATOM_SEQUENCE_FOREACH(seq, ev)
         {
            LV2_Atom_Object *o = (LV2_Atom_Object *) &ev->body;
            wxLogDebug(wxT("ev = %lld ev.size %d ev.type %d"), ev->time.frames, ev->body.size, ev->body.type);
         }
#endif
      }
      else
      {
         port->mBuffer.resize(port->mMinimumSize);
         *((LV2_Atom *) buf) =
         {
            port->mMinimumSize,
            urid_Chunk
         };
      }
   }

   return true;
}

size_t LV2Effect::RealtimeProcess(int group, float **inbuf, float **outbuf, size_t numSamples)
{
   wxASSERT(group >= 0 && group < (int) mSlaves.size());
   wxASSERT(numSamples <= (size_t) mBlockSize);

   if (group < 0 || group >= (int) mSlaves.size())
   {
      return 0;
   }

   LV2Wrapper *slave = mSlaves[group];
   LilvInstance *instance = slave->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mAudioPorts)
   {
      if (port->mIsInput)
      {
         for (decltype(numSamples) s = 0; s < numSamples; s++)
         {
            mMasterIn[i][s] += inbuf[i][s];
         }
      }

      lilv_instance_connect_port(instance,
                                 port->mIndex,
                                 (port->mIsInput ? inbuf[i++] : outbuf[o++]));
   }

   mNumSamples = wxMax(numSamples, mNumSamples);

   if (mRolling)
   {
      lilv_instance_run(instance, numSamples);
   }
   else
   {
      while (--i >= 0)
      {
         for (decltype(numSamples) s = 0; s < numSamples; s++)
         {
            outbuf[i][s] = inbuf[i][s];
         }
      }
   }

   slave->SendResponses();

   for (auto & port : mAtomPorts)
   {
      uint8_t *buf = port->mBuffer.data();

      if (!port->mIsInput)
      {
         port->mBuffer.resize(port->mMinimumSize);

         LV2_Atom *chunk = ( LV2_Atom *) buf;
         chunk->size = port->mMinimumSize;
         chunk->type = urid_Chunk;
      }
   }

   if (group == 0)
   {
      mPositionFrame += numSamples;
   }

   return numSamples;
}

bool LV2Effect::RealtimeProcessEnd()
{
   // Nothing to do if we did process any samples
   if (mNumSamples == 0)
   {
      return true;
   }

   int i = 0;
   int o = 0;
   for (auto & port : mAudioPorts)
   {
      lilv_instance_connect_port(mMaster->GetInstance(),
                                 port->mIndex,
                                 (port->mIsInput ? mMasterIn[i++].get() : mMasterOut[o++].get()));
   }

   if (mRolling)
   {
      lilv_instance_run(mMaster->GetInstance(), mNumSamples);
   }

   for (auto & port : mAtomPorts)
   {
      if (!port->mIsInput)
      {
         ZixRing *ring = port->mRing;

         LV2_ATOM_SEQUENCE_FOREACH((LV2_Atom_Sequence *) port->mBuffer.data(), ev)
         {
            zix_ring_write(ring, &ev->body, ev->body.size + sizeof(LV2_Atom));
         }
      }
   }

   mNumSamples = 0;

   return true;
}

bool LV2Effect::ShowInterface(
   wxWindow &parent, const EffectDialogFactory &factory, bool forceModal)
{
   if (mDialog)
   {
      if (mDialog->Close(true))
      {
         mDialog = nullptr;
      }
      return false;
   }

   // mDialog is null
   auto cleanup = valueRestorer(mDialog);

   if ( factory )
      mDialog = factory(parent, mHost, this);
   if (!mDialog)
   {
      return false;
   }

   // Try to give the window a sensible default/minimum size
   mDialog->Layout();
   mDialog->Fit();
   mDialog->SetMinSize(mDialog->GetSize());
   if (mNoResize)
   {
      mDialog->SetMaxSize(mDialog->GetSize());
   }

   if ((SupportsRealtime() || GetType() == EffectTypeAnalyze) && !forceModal)
   {
      mDialog->Show();
      cleanup.release();

      return false;
   }

   bool res = mDialog->ShowModal() != 0;

   return res;
}

bool LV2Effect::GetAutomationParameters(CommandParameters &parms)
{
   for (auto & port : mControlPorts)
   {
      if (port->mIsInput)
      {
         if (!parms.Write(port->mName, port->mVal))
         {
            return false;
         }
      }
   }

   return true;
}

bool LV2Effect::SetAutomationParameters(CommandParameters &parms)
{
   // First pass validates values
   for (auto & port : mControlPorts)
   {
      if (port->mIsInput)
      {
         double d = 0.0;
         if (!parms.Read(port->mName, &d))
         {
            return false;
         }

         // Use unscaled range here
         if (d < port->mMin || d > port->mMax)
         {
            return false;
         }
      }
   }

   // Second pass actually sets the values
   for (auto & port : mControlPorts)
   {
      if (port->mIsInput)
      {
         double d = 0.0;
         if (!parms.Read(port->mName, &d))
         {
            return false;
         }

         port->mVal = d;
         port->mTmp = port->mVal * (port->mSampleRate ? mSampleRate : 1.0);
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

bool LV2Effect::PopulateUI(ShuttleGui &S)
{
   auto parent = S.GetParent();
   mParent = parent;

   mParent->PushEventHandler(this);

   mSuilHost = NULL;
   mSuilInstance = NULL;

   mMaster = InitInstance(mSampleRate);
   if (mMaster == NULL)
   {
      AudacityMessageBox( XO("Couldn't instantiate effect") );
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
#if 0
   // Nothing to do yet
#endif
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

   if (mSuilInstance)
   {
      if (mNativeWin)
      {
         mNativeWin->Destroy();
         mNativeWin = NULL;
      }

      mUIIdleInterface = NULL;
      mUIShowInterface = NULL;
      mExternalWidget = NULL;

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

bool LV2Effect::LoadUserPreset(const RegistryPath &name)
{
   if (!LoadParameters(name))
   {
      return false;
   }

   return TransferDataToWindow();
}

bool LV2Effect::SaveUserPreset(const RegistryPath &name)
{
   return SaveParameters(name);
}

RegistryPaths LV2Effect::GetFactoryPresets()
{
   if (mFactoryPresetsLoaded)
   {
      return mFactoryPresetNames;
   }

   LilvNodes *presets = lilv_plugin_get_related(mPlug, node_Preset);
   if (presets)
   {
      LILV_FOREACH(nodes, i, presets)
      {
         const LilvNode *preset = lilv_nodes_get(presets, i);

         mFactoryPresetUris.push_back(LilvString(preset));

         lilv_world_load_resource(gWorld, preset);

         LilvNodes *labels = lilv_world_find_nodes(gWorld, preset, node_Label, NULL);
         if (labels)
         {
            const LilvNode *label = lilv_nodes_get_first(labels);

            mFactoryPresetNames.push_back(LilvString(label));

            lilv_nodes_free(labels);
         }
         else
         {
            mFactoryPresetNames.push_back(LilvString(preset).AfterLast(wxT('#')));
         }
      }

      lilv_nodes_free(presets);
   }

   mFactoryPresetsLoaded = true;

   return mFactoryPresetNames;
}

bool LV2Effect::LoadFactoryPreset(int id)
{
   if (id < 0 || id >= (int) mFactoryPresetUris.size())
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
      lilv_state_restore(state, mMaster->GetInstance(), set_value_func, this, 0, NULL);

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
      // Reinitialize configuration settings
      int userBlockSize;
      mHost->GetSharedConfig(wxT("Settings"), wxT("BufferSize"), userBlockSize, DEFAULT_BLOCKSIZE);
      mUserBlockSize = std::max(1, userBlockSize);
      mHost->GetSharedConfig(wxT("Settings"), wxT("UseLatency"), mUseLatency, true);
   }
}

// ============================================================================
// LV2Effect Implementation
// ============================================================================

bool LV2Effect::LoadParameters(const RegistryPath &group)
{
   wxString parms;
   if (!mHost->GetPrivateConfig(group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return SetAutomationParameters(eap);
}

bool LV2Effect::SaveParameters(const RegistryPath &group)
{
   CommandParameters eap;
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

size_t LV2Effect::AddOption(LV2_URID key, uint32_t size, LV2_URID type, const void *value)
{
   int ndx = mOptions.size();
   mOptions.resize(1 + ndx);

   memset(&mOptions[ndx], 0, sizeof(mOptions[ndx]));

   if (key != 0)
   {
      mOptions[ndx].context = LV2_OPTIONS_INSTANCE;
      mOptions[ndx].subject = 0;
      mOptions[ndx].key = key;
      mOptions[ndx].size = size;
      mOptions[ndx].type = type;
      mOptions[ndx].value = value;
   }

   return ndx;
}

LV2_Feature *LV2Effect::AddFeature(const char *uri, void *data)
{
   size_t ndx = mFeatures.size();
   mFeatures.resize(1 + ndx);

   if (uri)
   {
      mFeatures[ndx].reset(safenew LV2_Feature);
      mFeatures[ndx]->URI = uri;
      mFeatures[ndx]->data = data;
   }

   return mFeatures[ndx].get();
}

bool LV2Effect::ValidateFeatures(const LilvNode *subject)
{
   if (CheckFeatures(subject, node_RequiredFeature, true))
   {
      return CheckFeatures(subject, node_OptionalFeature, false);
   }

   return false;
}

bool LV2Effect::CheckFeatures(const LilvNode *subject, const LilvNode *predicate, bool required)
{
   bool supported = true;

   LilvNodes *nodes = lilv_world_find_nodes(gWorld, subject, predicate, NULL);
   if (nodes)
   {
      LILV_FOREACH(nodes, i, nodes)
      {
         const LilvNode *node = lilv_nodes_get(nodes, i);
         const char *uri = lilv_node_as_string(node);

         if ((strcmp(uri, LV2_UI__noUserResize) == 0) ||
             (strcmp(uri, LV2_UI__fixedSize) == 0))
         {
            mNoResize = true;
         }
         else if (strcmp(uri, LV2_WORKER__schedule) == 0)
         {
            /* Supported but handled in LV2Wrapper */
         }
         else
         {
            supported = false;

            for (auto & feature : mFeatures)
            {
               if (feature && strcmp(feature->URI, uri) == 0)
               {
                  supported = true;
                  break;
               }
            }

            if (!supported)
            {
               if (required)
               {
                  wxLogError(wxT("%s requires unsupported feature %s"), lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
                  printf(_("%s requires unsupported feature %s\n"), lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
                  break;
               }
               supported = true;
            }
         }
      }

      lilv_nodes_free(nodes);
   }


   return supported;
}

bool LV2Effect::ValidateOptions(const LilvNode *subject)
{
   if (CheckOptions(subject, node_RequiredOption, true))
   {
      return CheckOptions(subject, node_SupportedOption, false);
   }

   return false;
}

bool LV2Effect::CheckOptions(const LilvNode *subject, const LilvNode *predicate, bool required)
{
   bool supported = true;

   LilvNodes *nodes = lilv_world_find_nodes(gWorld, subject, predicate, NULL);
   if (nodes)
   {
      LILV_FOREACH(nodes, i, nodes)
      {
         const LilvNode *node = lilv_nodes_get(nodes, i);
         const char *uri = lilv_node_as_string(node);
         LV2_URID urid = URID_Map(uri);

         if (urid == urid_NominalBlockLength)
         {
            mSupportsNominalBlockLength = true;
         }
         else if (urid == urid_SampleRate)
         {
            mSupportsSampleRate = true;
         }
         else
         {
            supported = false;

            for (auto & option : mOptions)
            {
               if (option.key == urid)
               {
                  supported = true;
                  break;
               }
            }

            if (!supported)
            {
               if (required)
               {
                  wxLogError(wxT("%s requires unsupported option %s"), lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
                  printf(_("%s requires unsupported option %s\n"), lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
                  break;
               }
               supported = true;
            }
         }
      }

      lilv_nodes_free(nodes);
   }

   return supported;
}

LV2Wrapper *LV2Effect::InitInstance(float sampleRate)
{
   LV2Wrapper *wrapper = new LV2Wrapper(this);
   if (wrapper == NULL)
   {
      return NULL;
   }
   
   LilvInstance *instance = wrapper->Instantiate(mPlug, sampleRate, mFeatures);
   if (!instance)
   {
      delete wrapper;
      return NULL;
   }

   wrapper->SetBlockSize();
   wrapper->SetSampleRate();

   // Connect all control ports
   for (auto & port : mControlPorts)
   {
      // If it's not an input port and master has already been created
      // then connect the port to a dummy field since slave output port
      // values are unwanted as the master values will be used.
      //
      // Otherwise, connect it to the real value field.
      lilv_instance_connect_port(instance,
                                 port->mIndex,
                                 !port->mIsInput && mMaster
                                 ? &port->mDmy
                                 : &port->mVal);
   }

   // Connect all atom ports
   for (auto & port : mAtomPorts)
   {
      lilv_instance_connect_port(instance, port->mIndex, port->mBuffer.data());
   }

   // We don't fully support CV ports, so connect them to dummy buffers for now.
   for (auto & port : mCVPorts)
   {
      lilv_instance_connect_port(instance, port->mIndex, port->mBuffer.get());
   }

   // Give plugin a chance to initialize.  The SWH plugins (like AllPass) need
   // this before it can be safely deleted.
   lilv_instance_activate(instance);
   lilv_instance_deactivate(instance);

   for (auto & port : mAtomPorts)
   {
      if (!port->mIsInput)
      {
         ZixRing *ring = port->mRing;

         LV2_ATOM_SEQUENCE_FOREACH(( LV2_Atom_Sequence *) port->mBuffer.data(), ev)
         {
            zix_ring_write(ring, &ev->body, ev->body.size + sizeof(LV2_Atom));
         }
      }
   }

   return wrapper;
}

void LV2Effect::FreeInstance(LV2Wrapper *wrapper)
{
   delete wrapper;
}

bool LV2Effect::BuildFancy()
{
   // Set the native UI type
   const char *nativeType =
#if defined(__WXGTK3__)
      LV2_UI__Gtk3UI;
#elif defined(__WXGTK__)
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
            if (lilv_ui_is_a(ui, node_Gtk) || lilv_ui_is_a(ui, node_Gtk3))
            {
               uiType = node_Gtk;
               break;
            }

            ui = NULL;
         }

         lilv_node_free(containerType);
      }
   }

   // Check for other supported UIs
   if (ui == NULL)
   {
      LILV_FOREACH(uis, iter, uis)
      {
         ui = lilv_uis_get(uis, iter);
         if (lilv_ui_is_a(ui, node_ExternalUI) || lilv_ui_is_a(ui, node_ExternalUIOld))
         {
            uiType = node_ExternalUI;
            break;
         }
         ui = NULL;
      }
   }

   // No usable UI found
   if (ui == NULL)
   {
      lilv_uis_free(uis);
      return false;
   }

   const LilvNode *uinode = lilv_ui_get_uri(ui);
   lilv_world_load_resource(gWorld, uinode);
   if (!ValidateFeatures(uinode))
   {
      lilv_uis_free(uis);
      return false;
   }

   const char *containerType;

   if (uiType == node_ExternalUI)
   {
      containerType = LV2_EXTERNAL_UI__Widget;
   }
   else
   {
      containerType = nativeType;
      mParentFeature->data = mParent->GetHandle();

#if defined(__WXGTK__)
      // Make sure the parent has a window
      if (!gtk_widget_get_window(GTK_WIDGET(mParent->m_wxwindow)))
      {
         gtk_widget_realize(GTK_WIDGET(mParent->m_wxwindow));
      }
#endif
   }

   LilvInstance *instance = mMaster->GetInstance();
   mInstanceAccessFeature->data = lilv_instance_get_handle(instance);
   mExtensionDataFeature.data_access = lilv_instance_get_descriptor(instance)->extension_data;

   // Set before creating the UI instance so the initial size (if any) can be captured
   mNativeWinInitialSize = wxDefaultSize;
   mNativeWinLastSize = wxDefaultSize;

   // Create the suil host
   mSuilHost = suil_host_new(LV2Effect::suil_port_write_func,
                             LV2Effect::suil_port_index_func,
                             NULL,
                             NULL);
   if (!mSuilHost)
   {
      lilv_uis_free(uis);
      return false;
   }

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   char *libPath = lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), NULL);
   wxString path = wxPathOnly(libPath);
   SetDllDirectory(path.c_str());
   lilv_free(libPath);
#endif

   char *bundlePath = lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)), NULL);
   char *binaryPath = lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), NULL);

   mSuilInstance = suil_instance_new(mSuilHost,
                                     this,
                                     containerType,
                                     lilv_node_as_uri(lilv_plugin_get_uri(mPlug)),
                                     lilv_node_as_uri(lilv_ui_get_uri(ui)),
                                     lilv_node_as_uri(uiType),
                                     bundlePath,
                                     binaryPath,
                                     reinterpret_cast<const LV2_Feature * const *>(mFeatures.data()));

   lilv_free(binaryPath);
   lilv_free(bundlePath);
   lilv_uis_free(uis);

   // Bail if the instance (no compatible UI) couldn't be created
   if (!mSuilInstance)
   {
#if defined(__WXMSW__)
      SetDllDirectory(NULL);
#endif

      suil_host_free(mSuilHost);
      mSuilHost = NULL;

      return false;
   }

   if (uiType == node_ExternalUI)
   {
      mParent->SetMinSize(wxDefaultSize);

      mExternalWidget = (LV2_External_UI_Widget *) suil_instance_get_widget(mSuilInstance);
      mTimer.SetOwner(this, ID_TIMER);
      mTimer.Start(20);

      LV2_EXTERNAL_UI_SHOW(mExternalWidget);
   }
   else
   {
      WXWidget widget = (WXWidget) suil_instance_get_widget(mSuilInstance);

#if defined(__WXGTK__)
      // Needed by some plugins (e.g., Invada) to ensure the display is fully
      // populated.
      gtk_widget_show_all(widget);

      // See note at size_request()
      g_signal_connect(widget, "size-request", G_CALLBACK(LV2Effect::size_request), this);
#endif

      Destroy_ptr< NativeWindow > uNativeWin{ safenew NativeWindow() };
      if ( !uNativeWin->Create(mParent, widget) )
         return false;
      mNativeWin = uNativeWin.release();

      mNativeWin->Bind(wxEVT_SIZE, &LV2Effect::OnSize, this);

      // The plugin called the LV2UI_Resize::ui_resize function to set the size before
      // the native window was created, so set the size now.
      if (mNativeWinInitialSize != wxDefaultSize)
      {
         mNativeWin->SetMinSize(mNativeWinInitialSize);
      }

      wxSizerItem *si = NULL;
      auto vs = std::make_unique<wxBoxSizer>(wxVERTICAL);
      if (vs)
      {
         auto hs = std::make_unique<wxBoxSizer>(wxHORIZONTAL);
         if (hs)
         {
            if (mNoResize)
            {
               si = hs->Add(mNativeWin, 0, wxCENTER);
               vs->Add(hs.release(), 1, wxCENTER);
            }
            else
            {
               si = hs->Add(mNativeWin, 1, wxEXPAND);
               vs->Add(hs.release(), 1, wxEXPAND);
            }
         }
      }

      if (!si)
      {
         lilv_uis_free(uis);
         return false;
      }

      mParent->SetSizerAndFit(vs.release());
   }

   mUIIdleInterface = (LV2UI_Idle_Interface *)
      suil_instance_extension_data(mSuilInstance, LV2_UI__idleInterface);

   mUIShowInterface = (LV2UI_Show_Interface *)
      suil_instance_extension_data(mSuilInstance, LV2_UI__showInterface);

   if (mUIShowInterface)
   {
//      mUIShowInterface->show(suil_instance_get_handle(mSuilInstance));
   }

   TransferDataToWindow();

#ifdef __WXMAC__
#ifdef __WX_EVTLOOP_BUSY_WAITING__
   wxEventLoop::SetBusyWaiting(true);
#endif
#endif

#if defined(__WXMSW__)
   SetDllDirectory(NULL);
#endif

   return true;
}

bool LV2Effect::BuildPlain()
{
   int numCols = 5;
   wxSizer *innerSizer;

   wxASSERT(mParent); // To justify safenew
   wxScrolledWindow *const w = safenew 
      wxScrolledWindow(mParent,
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
               NumericTextCtrl(w, ID_Duration,
                               NumericConverter::TIME,
                               mHost->GetDurationFormat(),
                               mHost->GetDuration(),
                               mSampleRate,
                               NumericTextCtrl::Options {}
            .AutoPos(true));
            mDuration->SetName( XO("Duration") );
            sizer->Add(mDuration, 0, wxALIGN_CENTER | wxALL, 5);

            groupSizer->Add(sizer.release(), 0, wxALIGN_CENTER | wxALL, 5);
            innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
         }

         std::sort(mGroups.begin(), mGroups.end());

         for (size_t i = 0, groupCount = mGroups.size(); i < groupCount; i++)
         {
            wxString label = mGroups[i];
            auto groupSizer = std::make_unique<wxStaticBoxSizer>(wxVERTICAL, w, label);

            auto gridSizer = std::make_unique<wxFlexGridSizer>(numCols, 5, 5);
            gridSizer->AddGrowableCol(3);

            for (auto & p : mGroupMap[mGroups[i]])
            {
               auto & port = mControlPorts[p];

               if (port->mNotOnGui)
               {
                  continue;
               }

               wxString labelText = port->mName;
               if (!port->mUnits.empty())
               {
                  labelText += wxT(" (") + port->mUnits + wxT(")");
               }

               if (port->mTrigger)
               {
                  gridSizer->Add(1, 1, 0);

                  wxASSERT(w); // To justify safenew
                  wxButton *b = safenew wxButton(w, ID_Triggers + p, labelText);
                  gridSizer->Add(b, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                  port->mCtrl.button = b;

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  continue;
               }

               wxWindow *item = safenew wxStaticText(w, wxID_ANY, labelText + wxT(":"),
                                                     wxDefaultPosition, wxDefaultSize,
                                                     wxALIGN_RIGHT);
               gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);

               if (port->mToggle)
               {
                  wxCheckBox *c = safenew wxCheckBox(w, ID_Toggles + p, wxT(""));
                  c->SetName(labelText);
                  c->SetValue(port->mVal > 0);
                  gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                  port->mCtrl.checkbox = c;

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
               }
               else if (port->mEnumeration)      // Check before integer
               {
                  int s;
                  for (s = (int) port->mScaleValues.size() - 1; s >= 0; s--)
                  {
                     if (port->mVal >= port->mScaleValues[s])
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
                  c->Append(port->mScaleLabels);
                  c->SetSelection(s);
                  gridSizer->Add(c, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                  port->mCtrl.choice = c;

                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);
               }
               else if (!port->mIsInput)
               {
                  gridSizer->Add(1, 1, 0);
                  gridSizer->Add(1, 1, 0);

                  LV2EffectMeter *m = safenew LV2EffectMeter(w, port);
                  gridSizer->Add(m, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);
                  port->mCtrl.meter = m;

                  gridSizer->Add(1, 1, 0);
               }
               else
               {
                  wxTextCtrl *t = safenew wxTextCtrl(w, ID_Texts + p, wxT(""));
                  t->SetName(labelText);
                  gridSizer->Add(t, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT);
                  port->mText = t;

                  float rate = port->mSampleRate ? mSampleRate : 1.0;

                  port->mLo = port->mMin * rate;
                  port->mHi = port->mMax * rate;
                  port->mTmp = port->mVal * rate;

                  if (port->mInteger)
                  {
                     IntegerValidator<float> vld(&port->mTmp);
                     vld.SetRange(port->mLo, port->mHi);
                     t->SetValidator(vld);
                  }
                  else
                  {
                     FloatingPointValidator<float> vld(6, &port->mTmp);
                     vld.SetRange(port->mLo, port->mHi);

                     // Set number of decimal places
                     float range = port->mHi - port->mLo;
                     auto style = range < 10
                                  ? NumValidatorStyle::THREE_TRAILING_ZEROES
                                  : range < 100
                                    ? NumValidatorStyle::TWO_TRAILING_ZEROES
                                    : NumValidatorStyle::ONE_TRAILING_ZERO;
                     vld.SetStyle(style);

                     t->SetValidator(vld);
                  }

                  if (port->mHasLo)
                  {
                     wxString str;
                     if (port->mInteger || port->mSampleRate)
                     {
                        str.Printf(wxT("%d"), (int) lrintf(port->mLo));
                     }
                     else
                     {
                        str = Internat::ToDisplayString(port->mLo);
                     }
                     item = safenew wxStaticText(w, wxID_ANY, str);
                     gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT);
                  }
                  else
                  {
                     gridSizer->Add(1, 1, 0);
                  }

                  wxSlider *s = safenew wxSliderWrapper(w, ID_Sliders + p,
                                                        0, 0, 1000,
                                                        wxDefaultPosition,
                                                        wxSize(150, -1));
                  s->SetName(labelText);
                  gridSizer->Add(s, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND);
                  port->mCtrl.slider = s;

                  if (port->mHasHi)
                  {
                     wxString str;
                     if (port->mInteger || port->mSampleRate)
                     {
                        str.Printf(wxT("%d"), (int) lrintf(port->mHi));
                     }
                     else
                     {
                        str = Internat::ToDisplayString(port->mHi);
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
         std::vector<int> widths(numCols);

         size_t cnt = innerSizer->GetChildren().GetCount();
         for (size_t i = (GetType() == EffectTypeGenerate); i < cnt; i++)
         {
            wxSizer *groupSizer = innerSizer->GetItem(i)->GetSizer();
            wxFlexGridSizer *gridSizer = (wxFlexGridSizer *) groupSizer->GetItem((size_t) 0)->GetSizer();

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
            wxFlexGridSizer *gridSizer = (wxFlexGridSizer *) groupSizer->GetItem((size_t) 0)->GetSizer();

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
   w->SetMinSize( { -1, std::min(sz1.y, sz2.y) } );

   // And let the parent reduce to the NEW minimum if possible
   mParent->SetMinSize(w->GetMinSize());

   TransferDataToWindow();

   return true;
}

bool LV2Effect::TransferDataToWindow()
{
   if (mUseGUI)
   {
      if (mSuilInstance)
      {
         for (auto & port : mControlPorts)
         {
            if (port->mIsInput)
            {
               suil_instance_port_event(mSuilInstance,
                                        port->mIndex,
                                        sizeof(float),
                                        0,
                                        &port->mVal);
            }
         }
      }

      return true;
   }

   for (auto & group : mGroups)
   {
      const auto & params = mGroupMap[group];
      for (auto & param : params)
      {
         auto & port = mControlPorts[param];

         if (port->mTrigger)
         {
            continue;
         }

         if (port->mToggle)
         {
            port->mCtrl.checkbox->SetValue(port->mVal > 0);
         }
         else if (port->mEnumeration)      // Check before integer
         {
            int s;
            for (s = (int) port->mScaleValues.size() - 1; s >= 0; s--)
            {
               if (port->mVal >= port->mScaleValues[s])
               {
                  break;
               }
            }

            if (s < 0)
            {
               s = 0;
            }

            port->mCtrl.choice->SetSelection(s);
         }
         else if (port->mIsInput)
         {
            port->mTmp = port->mVal * (port->mSampleRate ? mSampleRate : 1.0);
            SetSlider(port);
         }
      }
   }

   if (mParent && !mParent->TransferDataToWindow())
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

void LV2Effect::SetSlider(const LV2ControlPortPtr & port)
{
   float lo = port->mLo;
   float hi = port->mHi;
   float val = port->mTmp;

   if (port->mLogarithmic)
   {
      lo = logf(lo);
      hi = logf(hi);
      val = logf(val);
   }

   port->mCtrl.slider->SetValue(lrintf((val - lo) / (hi - lo) * 1000.0));
}

void LV2Effect::OnTrigger(wxCommandEvent &evt)
{
   auto & port = mControlPorts[evt.GetId() - ID_Triggers];

   port->mVal = port->mDef;
}

void LV2Effect::OnToggle(wxCommandEvent &evt)
{
   auto & port = mControlPorts[evt.GetId() - ID_Toggles];

   port->mVal = evt.GetInt() ? 1.0 : 0.0;
}

void LV2Effect::OnChoice(wxCommandEvent &evt)
{
   auto & port = mControlPorts[evt.GetId() - ID_Choices];

   port->mVal = port->mScaleValues[evt.GetInt()];
}

void LV2Effect::OnText(wxCommandEvent &evt)
{
   auto & port = mControlPorts[evt.GetId() - ID_Texts];

   if (port->mText->GetValidator()->TransferFromWindow())
   {
      port->mVal = port->mSampleRate ? port->mTmp / mSampleRate : port->mTmp;

      SetSlider(port);
   }
}

void LV2Effect::OnSlider(wxCommandEvent &evt)
{
   auto & port = mControlPorts[evt.GetId() - ID_Sliders];

   float lo = port->mLo;
   float hi = port->mHi;

   if (port->mLogarithmic)
   {
      lo = logf(lo);
      hi = logf(hi);
   }

   port->mTmp = (((float) evt.GetInt()) / 1000.0) * (hi - lo) + lo;
   port->mTmp = port->mLogarithmic ? expf(port->mTmp) : port->mTmp;

   port->mTmp = port->mTmp < port->mLo ? port->mLo : port->mTmp;
   port->mTmp = port->mTmp > port->mHi ? port->mHi : port->mTmp;

   port->mVal = port->mSampleRate ? port->mTmp / mSampleRate : port->mTmp;

   port->mText->GetValidator()->TransferToWindow();
}

void LV2Effect::OnTimer(wxTimerEvent &evt)
{
   evt.Skip();

   if (mExternalWidget)
   {
      LV2_EXTERNAL_UI_RUN(mExternalWidget);
   }
}

void LV2Effect::OnIdle(wxIdleEvent &evt)
{
   evt.Skip();

   if (!mSuilInstance)
   {
      return;
   }

   if (mExternalUIClosed)
   {
      mExternalUIClosed = false;
      mDialog->Close();
      return;
   }

   if (mUIIdleInterface)
   {
      SuilHandle handle = suil_instance_get_handle(mSuilInstance);
      if (mUIIdleInterface->idle(handle))
      {
         if (mUIShowInterface)
         {
            mUIShowInterface->hide(handle);
         }
         mDialog->Close();
         return;
      }
   }

   if (mControlOut)
   {
      ZixRing *ring = mControlOut->mRing;

      LV2_Atom *atom = (LV2_Atom *) malloc(mControlOut->mMinimumSize);
      if (atom)
      {
         while (zix_ring_read(ring, atom, sizeof(LV2_Atom)))
         {
            uint32_t size = lv2_atom_total_size(atom);

            if (size < mControlOut->mMinimumSize)
            {
               zix_ring_read(ring, LV2_ATOM_CONTENTS(LV2_Atom, atom), atom->size);
               suil_instance_port_event(mSuilInstance,
                                        mControlOut->mIndex,
                                        size,
                                        urid_EventTransfer,
                                        atom);
            }
            else
            {
               zix_ring_skip(ring, atom->size);
               wxLogError(wxT("LV2 sequence buffer overflow"));
            }
         }
         free(atom);
      }
   }

   for (auto & port : mControlPorts)
   {
      // Let UI know that a port's value has changed
      if (port->mVal != port->mLst)
      {
         suil_instance_port_event(mSuilInstance,
                                  port->mIndex,
                                  sizeof(port->mVal),
                                  0,
                                  &port->mVal);
         port->mLst = port->mVal;
      }
   }
}

void LV2Effect::OnSize(wxSizeEvent & evt)
{
   evt.Skip();

   // Don't do anything here if we're recursing
   if (mResizing)
   {
      return;
   }

   // Indicate resizing is occurring
   mResizing = true;

   // Can only resize AFTER the dialog has been completely created and
   // there's no need to resize if we're already at the desired size.
   if (mDialog && evt.GetSize() != mNativeWinLastSize)
   {
      // Save the desired size and set the native window to match
      mNativeWinLastSize = evt.GetSize();
      mNativeWin->SetMinSize(mNativeWinLastSize);

      // Clear the minimum size of the parent window to allow the following
      // Fit() to make proper adjustments
      mParent->SetMinSize(wxDefaultSize);

#if defined(__WXGTK__)
      // If the user resized the native window, then we need to also
      // clear the dialogs minimum size.  If this isn't done, the dialog
      // will not resize properly when going from a larger size to a smaller
      // size (due to the minimum size constraint).
      //
      // In this case, mResized has been set by the "size_request()" function
      // to indicate that this is a plugin generated resize request.
      if (mResized)
      {
        mDialog->SetMinSize(wxDefaultSize);
      }

      // Resize dialog
      mDialog->Fit();

      // Reestablish the minimum (and maximum) now that the dialog
      // has is desired size.
      if (mResized)
      {
         mDialog->SetMinSize(mDialog->GetSize());
         if (mNoResize)
         {
            mDialog->SetMaxSize(mDialog->GetSize());
         }
      }

      // Tell size_request() that the native window was just resized.
      mResized = true;
#else
      // Resize the dialog to fit its content.
      mDialog->Fit();
#endif
   }

   // No longer resizing
   mResizing = false;
}

// ============================================================================
// Feature handlers
// ============================================================================

// static callback
uint32_t LV2Effect::uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                              const char *WXUNUSED(map),
                              const char *uri)
{
   return ((LV2Effect *) callback_data)->URID_Map(uri);
}

// static callback
LV2_URID LV2Effect::urid_map(LV2_URID_Map_Handle handle, const char *uri)
{
   return ((LV2Effect *) handle)->URID_Map(uri);
}

LV2_URID LV2Effect::URID_Map(const char *uri)
{
   LV2_URID urid;
   
   urid = Lookup_URI(gURIDMap, uri, false);
   if (urid > 0)
   {
      return urid;
   }

   urid = Lookup_URI(mURIDMap, uri);
   if (urid > 0)
   {
      return urid + gURIDMap.size();
   }

   return 0;
}

LV2_URID LV2Effect::Lookup_URI(URIDMap & map, const char *uri, bool add)
{
   size_t ndx = map.size();
   for (size_t i = 0; i < ndx; i++)
   {
      if (strcmp(map[i].get(), uri) == 0)
      {
         return i + 1;
      }
   }

   if (add)
   {
      // Almost all compilers have strdup(), but VC++ and MinGW call it _strdup().
      map.push_back(MallocString<>(wxCRT_StrdupA(uri)));
      return ndx + 1;
   }

   return 0;
}

// static callback
const char *LV2Effect::urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid)
{
   return ((LV2Effect *) handle)->URID_Unmap(urid);
}

const char *LV2Effect::URID_Unmap(LV2_URID urid)
{
   if (urid > 0)
   {
      if (urid <= (LV2_URID) gURIDMap.size())
      {
         return mURIDMap[urid - 1].get();
      }

      urid -= gURIDMap.size();

      if (urid <= (LV2_URID) mURIDMap.size())
      {
         return mURIDMap[urid - 1].get();
      }
   }

   return NULL;
}

// static callback
int LV2Effect::log_printf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, ...)
{
   va_list ap;
   int len;

   va_start(ap, fmt);
   len = ((LV2Effect *) handle)->LogVPrintf(type, fmt, ap);
   va_end(ap);

   return len;
}

// static callback
int LV2Effect::log_vprintf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, va_list ap)
{
   return ((LV2Effect *) handle)->LogVPrintf(type, fmt, ap);
}

int LV2Effect::LogVPrintf(LV2_URID type, const char *fmt, va_list ap)
{
   long level = wxLOG_Error;

   if (type == urid_Error)
   {
      level = wxLOG_Error;
   }
   else if (type == urid_Note)
   {
      level = wxLOG_Info;
   }
   else if (type == urid_Trace)
   {
      level = wxLOG_Trace;
   }
   else if (type == urid_Warning)
   {
      level = wxLOG_Warning;
   }
   else
   {
      level = wxLOG_Message;
   }

   char *msg = NULL;
   int len = wxCRT_VsnprintfA(msg, 0, fmt, ap);

   msg = (char *) malloc(len + 1);
   if (msg)
   {
      wxCRT_VsnprintfA(msg, len, fmt, ap);

      wxString text(msg);

      wxLogGeneric(level, wxT("%s: %s"), GetSymbol().Msgid().Translation(), text);

      free(msg);
   }

   return len;
}

// static callback
int LV2Effect::ui_resize(LV2UI_Feature_Handle handle, int width, int height)
{
   return ((LV2Effect *) handle)->UIResize(width, height);
}

int LV2Effect::UIResize(int width, int height)
{
   // Queue a wxSizeEvent to resize the plugins UI
   if (mNativeWin)
   {
      wxSizeEvent sw(wxSize(width, height));
      sw.SetEventObject(mNativeWin);
      mNativeWin->GetEventHandler()->AddPendingEvent(sw);
   }
   // The window hasn't been created yet, so record the desired size
   else
   {
      mNativeWinInitialSize = wxSize(width, height);
   }

   return 0;
}

// static callback
void LV2Effect::ui_closed(LV2UI_Controller controller)
{
   return ((LV2Effect *) controller)->UIClosed();
}

void LV2Effect::UIClosed()
{
   mExternalUIClosed = true;

   return;
}

// static callback
void LV2Effect::suil_port_write_func(SuilController controller,
                                     uint32_t port_index,
                                     uint32_t buffer_size,
                                     uint32_t protocol,
                                     const void *buffer)
{
   ((LV2Effect *) controller)->SuilPortWrite(port_index, buffer_size, protocol, buffer);
}

void LV2Effect::SuilPortWrite(uint32_t port_index,
                              uint32_t buffer_size,
                              uint32_t protocol,
                              const void *buffer)
{
   // Handle implicit floats
   if (protocol == 0 && buffer_size == sizeof(float))
   {
      auto it = mControlPortMap.find(port_index);
      if (it != mControlPortMap.end())
      {
         it->second->mVal = *((const float *) buffer);
      }
   }
   // Handle event transfers
   else if (protocol == urid_EventTransfer)
   {
      if (mControlIn && port_index == mControlIn->mIndex)
      {
         zix_ring_write(mControlIn->mRing, buffer, buffer_size);
      }
   }

   return;
}

// static callback
uint32_t LV2Effect::suil_port_index_func(SuilController controller,
                                         const char *port_symbol)
{
   return ((LV2Effect *) controller)->SuilPortIndex(port_symbol);
}

uint32_t LV2Effect::SuilPortIndex(const char *port_symbol)
{
   for (size_t i = 0, cnt = lilv_plugin_get_num_ports(mPlug); i < cnt; i++)
   {
      const LilvPort *port = lilv_plugin_get_port_by_index(mPlug, i);
      if (strcmp(port_symbol, lilv_node_as_string(lilv_port_get_symbol(mPlug, port))) == 0)
      {
         return lilv_port_get_index(mPlug, port);
      }
   }

   return LV2UI_INVALID_PORT_INDEX;
}

// static callback
const void *LV2Effect::get_value_func(const char *port_symbol,
                                      void *user_data,
                                      uint32_t *size,
                                      uint32_t *type)
{
   return ((LV2Effect *) user_data)->GetPortValue(port_symbol, size, type);
}

const void *LV2Effect::GetPortValue(const char *port_symbol,
                                    uint32_t *size,
                                    uint32_t *type)
{
   wxString symbol = wxString::FromUTF8(port_symbol);

   for (auto & port : mControlPorts)
   {
      if (port->mSymbol == symbol)
      {
         *size = sizeof(float);
         *type = urid_Float;
         return (void *) &port->mVal;
      }
   }

   *size = 0;
   *type = 0;

   return NULL;
}

// static callback
void LV2Effect::set_value_func(const char *port_symbol,
                               void *user_data,
                               const void *value,
                               uint32_t size,
                               uint32_t type)
{
   ((LV2Effect *) user_data)->SetPortValue(port_symbol, value, size, type);
}

void LV2Effect::SetPortValue(const char *port_symbol,
                             const void *value,
                             uint32_t size,
                             uint32_t type)
{
   wxString symbol = wxString::FromUTF8(port_symbol);

   for (auto & port : mControlPorts)
   {
      if (port->mSymbol == symbol)
      {
         if (type == urid_Bool && size == sizeof(bool))
         {
            port->mVal = (float) (*((const bool *) value)) ? 1.0f : 0.0f;
         }
         else if (type == urid_Double && size == sizeof(double))
         {
            port->mVal = (float) (*((const double *) value));
         }
         else if (type == urid_Float && size == sizeof(float))
         {
            port->mVal = (float) (*((const float *) value));
         }
         else if (type == urid_Int && size == sizeof(int32_t))
         {
            port->mVal = (float) (*((const int32_t *) value));
         }
         else if (type == urid_Long && size == sizeof(int64_t))
         {
            port->mVal = (float) (*((const int64_t *) value));
         }

         break;
      }
   }
}

#if defined(__WXGTK__)
// static callback
//
// Need to queue a wxSizeEvent when the native window gets resized outside of
// WX control.  Many of the x42 LV2 plugins can resize themselves when changing
// the scale factor. (e.g., open "x42-dpl" effect and right click to change scaling)
void LV2Effect::size_request(GtkWidget *widget, GtkRequisition *requisition, LV2Effect *effect)
{
   effect->SizeRequest(widget, requisition);
}

void LV2Effect::SizeRequest(GtkWidget *widget, GtkRequisition *requisition)
{
   // Don't do anything if the OnSize() method is active
   if (!mResizing)
   {
      // If the OnSize() routine has processed an event, mResized will be true,
      // so just set the widgets size.
      if (mResized)
      {
         gtk_widget_set_size_request(widget, mNativeWinLastSize.x, mNativeWinLastSize.y);
         mResized = false;
      }
      // Otherwise, the plugin has resized the widget and we need to let WX know
      // about it.
      else if (mNativeWin)
      {
         mResized = true;
         wxSizeEvent se(wxSize(requisition->width, requisition->height));
         se.SetEventObject(mNativeWin);
         mNativeWin->GetEventHandler()->AddPendingEvent(se);
      }
   }
}
#endif

LV2Wrapper::LV2Wrapper(LV2Effect *effect)
:  mEffect(effect)
{
   mInstance = NULL;
   mHandle = NULL;
   mOptionsInterface = NULL;
   mStateInterface = NULL;
   mWorkerInterface = NULL;
   mWorkerSchedule = {};
   mFreeWheeling = false;
   mLatency = 0.0;
   mStopWorker = false;
}

LV2Wrapper::~LV2Wrapper()
{
   if (mInstance)
   {
      wxThread *thread = GetThread();
      if (thread && thread->IsAlive())
      {
         mStopWorker = true;

         LV2Work work = {0, NULL};
         mRequests.Post(work);

         thread->Wait();
      }

      if (mEffect->mActivated)
      {
         lilv_instance_deactivate(mInstance);
         mEffect->mActivated = false;
      }

      lilv_instance_free(mInstance);
      mInstance = NULL;
   }
}

LilvInstance *LV2Wrapper::Instantiate(const LilvPlugin *plugin,
                                      double sampleRate,
                                      std::vector<std::unique_ptr<LV2_Feature>> & features)
{
   if (mEffect->mWantsWorkerInterface)
   {
      // Remove terminator
      features.pop_back();

      mWorkerSchedule.handle = this;
      mWorkerSchedule.schedule_work = LV2Wrapper::schedule_work;
      mEffect->AddFeature(LV2_WORKER__schedule, &mWorkerSchedule);

      mEffect->AddFeature(NULL, NULL);
   }

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   const LilvNode *const libNode = lilv_plugin_get_library_uri(plugin);
   const char *const libUri = lilv_node_as_uri(libNode);
   char *libPath = lilv_file_uri_parse(libUri, NULL);
   wxString path = wxPathOnly(libPath);
   SetDllDirectory(path.c_str());
   lilv_free(libPath);
#endif

   mInstance = lilv_plugin_instantiate(plugin,
                                       sampleRate,
                                       reinterpret_cast<LV2_Feature **>(features.data()));

#if defined(__WXMSW__)
   SetDllDirectory(NULL);
#endif

   if (mEffect->mWantsWorkerInterface)
   {
      // Remove terminator
      features.pop_back();

      // Remove the worker interface feature
      features.pop_back();

      // Re-add terminator
      mEffect->AddFeature(NULL, NULL);
   }

   if (!mInstance)
   {
      return NULL;
   }

   mHandle = lilv_instance_get_handle(mInstance);

   mOptionsInterface = (LV2_Options_Interface *)
      lilv_instance_get_extension_data(mInstance, LV2_OPTIONS__interface);

   mStateInterface = (LV2_State_Interface *)
      lilv_instance_get_extension_data(mInstance, LV2_STATE__interface);

   mWorkerInterface = (LV2_Worker_Interface *)
      lilv_instance_get_extension_data(mInstance, LV2_WORKER__interface);

   if (mEffect->mLatencyPort >= 0)
   {
      lilv_instance_connect_port(mInstance, mEffect->mLatencyPort, &mLatency);
   }

   if (mWorkerInterface)
   {
      if (CreateThread() == wxTHREAD_NO_ERROR)
      {
         GetThread()->Run();
      }

   }

   return mInstance;
}

LilvInstance *LV2Wrapper::GetInstance()
{
   return mInstance;
}

LV2_Handle LV2Wrapper::GetHandle()
{
   return mHandle;
}

float LV2Wrapper::GetLatency()
{
   return mLatency;
}

void LV2Wrapper::SetFreeWheeling(bool enable)
{
   mFreeWheeling = enable;
}

void LV2Wrapper::SetSampleRate()
{
   if (mEffect->mSupportsSampleRate && mOptionsInterface && mOptionsInterface->set)
   {
      LV2_Options_Option options[2] = {};

      memcpy(&options,
             &mEffect->mOptions[mEffect->mSampleRateOption],
             sizeof(mEffect->mOptions[0]));

      mOptionsInterface->set(mHandle, options);
   }
}

void LV2Wrapper::SetBlockSize()
{
   if (mEffect->mSupportsNominalBlockLength && mOptionsInterface && mOptionsInterface->set)
   {
      LV2_Options_Option options[2] = {};
      memcpy(&options,
               &mEffect->mOptions[mEffect->mBlockSizeOption],
               sizeof(mEffect->mOptions[0]));

      mOptionsInterface->set(mHandle, options);
   }
}

void LV2Wrapper::ConnectPorts(float **inbuf, float **outbuf)
{
}

void *LV2Wrapper::Entry()
{
   LV2Work work;

   while (mRequests.Receive(work) == wxMSGQUEUE_NO_ERROR)
   {
      if (mStopWorker)
      {
         break;
      }

      mWorkerInterface->work(mHandle,
                             respond,
                             this,
                             work.size,
                             work.data);
   }

   return (void *) 0;
}

void LV2Wrapper::SendResponses()
{
   if (mWorkerInterface)
   {
      LV2Work work;

      while (mResponses.ReceiveTimeout(0, work) == wxMSGQUEUE_NO_ERROR)
      {
         mWorkerInterface->work_response(mHandle, work.size, work.data);
      }

      if (mWorkerInterface->end_run)
      {
         mWorkerInterface->end_run(mHandle);
      }
   }
}

// static callback
LV2_Worker_Status LV2Wrapper::schedule_work(LV2_Worker_Schedule_Handle handle,
                                           uint32_t size,
                                           const void *data)
{
   return ((LV2Wrapper *) handle)->ScheduleWork(size, data);
}

LV2_Worker_Status LV2Wrapper::ScheduleWork(uint32_t size, const void *data)
{
   if (mFreeWheeling)
   {
      return mWorkerInterface->work(mHandle,
                                    respond,
                                    this,
                                    size,
                                    data);
   }

   LV2Work work = {size, data};

   mRequests.Post(work);

   return LV2_WORKER_SUCCESS;
}

// static callback
LV2_Worker_Status LV2Wrapper::respond(LV2_Worker_Respond_Handle handle,
                                     uint32_t size,
                                     const void *data)
{
   return ((LV2Wrapper *) handle)->Respond(size, data);
}

LV2_Worker_Status LV2Wrapper::Respond(uint32_t size, const void *data)
{
   LV2Work work = {size, data};

   mResponses.Post(work);

   return LV2_WORKER_SUCCESS;
}

#endif
 
