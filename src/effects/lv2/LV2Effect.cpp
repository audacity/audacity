/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

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
#include <functional>

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dcbuffer.h>
#include <wx/dialog.h>
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

#include "AudacityException.h"
#include "ConfigInterface.h"
#include "../../widgets/valnum.h"
#include "../../widgets/AudacityMessageBox.h"
#include "../../widgets/wxPanelWrapper.h"
#include "../../widgets/NumericTextCtrl.h"

#include "lv2/instance-access/instance-access.h"

#if defined(__WXGTK__)
#include <gtk/gtk.h>
#endif

#if defined(__WXMSW__)
#include <wx/msw/wrapwin.h>
#endif

static inline void free_chars (char *p) { lilv_free(p); }
using LilvCharsPtr = Lilv_ptr<char, free_chars>;

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
   LV2EffectSettingsDialog(wxWindow *parent, EffectDefinitionInterface &effect);
   virtual ~LV2EffectSettingsDialog();

   void PopulateOrExchange(ShuttleGui &S);

   void OnOk(wxCommandEvent &evt);

private:
   EffectDefinitionInterface &mEffect;
   int mBufferSize{};
   bool mUseLatency{};
   bool mUseGUI{};

   DECLARE_EVENT_TABLE()
};

BEGIN_EVENT_TABLE(LV2EffectSettingsDialog, wxDialogWrapper)
   EVT_BUTTON(wxID_OK, LV2EffectSettingsDialog::OnOk)
END_EVENT_TABLE()

LV2EffectSettingsDialog::LV2EffectSettingsDialog(
   wxWindow *parent, EffectDefinitionInterface &effect)
:  wxDialogWrapper(parent, wxID_ANY, XO("LV2 Effect Settings"))
, mEffect{ effect }
{
   GetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("BufferSize"), mBufferSize, 8192);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("UseLatency"), mUseLatency, true);
   GetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("UseGUI"), mUseGUI, true);

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

   SetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("BufferSize"), mBufferSize);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("UseLatency"), mUseLatency);
   SetConfig(mEffect, PluginSettings::Shared, wxT("Settings"),
      wxT("UseGUI"), mUseGUI);

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

LV2Effect::LV2Effect(const LilvPlugin *plug) : LV2FeaturesList{ plug }
{
}

LV2Effect::~LV2Effect()
{
}

// ============================================================================
// ComponentInterface Implementation
// ============================================================================

PluginPath LV2Effect::GetPath() const
{
   return LilvString(lilv_plugin_get_uri(mPlug));
}

ComponentInterfaceSymbol LV2Effect::GetSymbol() const
{
   return LilvStringMove(lilv_plugin_get_name(mPlug));
}

VendorSymbol LV2Effect::GetVendor() const
{
   wxString vendor = LilvStringMove(lilv_plugin_get_author_name(mPlug));

   if (vendor.empty())
   {
      return XO("n/a");
   }

   return {vendor};
}

wxString LV2Effect::GetVersion() const
{
   return wxT("1.0");
}

TranslatableString LV2Effect::GetDescription() const
{
   return XO("n/a");
}

// ============================================================================
// EffectDefinitionInterface Implementation
// ============================================================================

EffectType LV2Effect::GetType() const
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

EffectFamilySymbol LV2Effect::GetFamily() const
{
   return LV2EFFECTS_FAMILY;
}

bool LV2Effect::IsInteractive() const
{
   return mControlPorts.size() != 0;
}

bool LV2Effect::IsDefault() const
{
   return false;
}

bool LV2Effect::SupportsRealtime() const
{
   // TODO reenable after achieving statelessness
   return false;
//   return GetType() == EffectTypeProcess;
}

bool LV2Effect::SupportsAutomation() const
{
   return true;
}

bool LV2Effect::InitializePlugin()
{
   using namespace LV2Symbols;

   // To be set up later when making a dialog:
   mExtensionDataFeature = {};

   if (!LV2FeaturesList::InitializeOptions() ||
      !LV2FeaturesList::InitializeFeatures()
   )
      return false;

   AddFeature(LV2_UI__resize, &mUIResizeFeature);
   AddFeature(LV2_DATA_ACCESS_URI, &mExtensionDataFeature);
   AddFeature(LV2_EXTERNAL_UI__Host, &mExternalUIHost);
   AddFeature(LV2_EXTERNAL_UI_DEPRECATED_URI, &mExternalUIHost);
   // Two features must be filled in later
   mInstanceAccessFeature = mFeatures.size();
   AddFeature(LV2_INSTANCE_ACCESS_URI, nullptr);
   mParentFeature = mFeatures.size();
   if (!ValidateFeatures(lilv_plugin_get_uri(mPlug)))
      return false;

   // Collect information in mAudioPorts, mControlPorts, mAtomPorts, mCVPorts
   {
   // Retrieve the port ranges for all ports (some values may be NaN)
   auto numPorts = lilv_plugin_get_num_ports(mPlug);
   Floats minimumVals {numPorts};
   Floats maximumVals {numPorts};
   Floats defaultVals {numPorts};
   lilv_plugin_get_port_ranges_float(mPlug,
      minimumVals.get(), maximumVals.get(), defaultVals.get());

   for (size_t i = 0; i < numPorts; ++i) {
      const auto port = lilv_plugin_get_port_by_index(mPlug, i);
      int index = lilv_port_get_index(mPlug, port);

      // It must be input or output, anything else is bogus
      bool isInput;
      if (lilv_port_is_a(mPlug, port, node_InputPort))
         isInput = true;
      else if (lilv_port_is_a(mPlug, port, node_OutputPort))
         isInput = false;
      else {
         assert(false);
         return false;
      }

      // Get the port name and symbol
      const auto symbol = LilvString(lilv_port_get_symbol(mPlug, port));
      const auto name = LilvStringMove(lilv_port_get_name(mPlug, port));

      // Get the group to which this port belongs or default to the main group
      TranslatableString groupName{};
      if (LilvNodePtr group{ lilv_port_get(mPlug, port, node_Group) }) {
         // lilv.h does not say whether return of lilv_world_get() needs to
         // be freed, but that is easily seen to be so from source
         auto groupMsg = LilvStringMove(
            lilv_world_get(gWorld, group.get(), node_Label, nullptr));
         if (groupMsg.empty())
            groupMsg = LilvStringMove(
               lilv_world_get(gWorld, group.get(), node_Name, nullptr));
         if (groupMsg.empty())
            groupMsg = LilvString(group.get());
         groupName = Verbatim(groupMsg);
      }
      else
         groupName = XO("Effect Settings");

      // Get the latency port
      const auto latencyIndex = lilv_plugin_get_latency_port_index(mPlug);

      // Get the ports designation (must be freed)
      LilvNodePtr designation{ lilv_port_get(mPlug, port, node_Designation) };

      // Check for audio ports
      if (lilv_port_is_a(mPlug, port, node_AudioPort)) {
         mAudioPorts.push_back(std::make_shared<LV2AudioPort>(
            port, index, isInput, symbol, name, groupName));
         isInput ? mAudioIn++ : mAudioOut++;
      }
      // Check for Control ports
      else if (lilv_port_is_a(mPlug, port, node_ControlPort)) {
         // Add group if not previously done
         if (mGroupMap.find(groupName) == mGroupMap.end())
            mGroups.push_back(groupName);
         mGroupMap[groupName].push_back(mControlPorts.size());

         mControlPorts.push_back(std::make_shared<LV2ControlPort>(
            port, index, isInput, symbol, name, groupName));
         const auto controlPort = mControlPorts.back();

         // Get any unit descriptor
         if (LilvNodePtr unit{ lilv_port_get(mPlug, port, node_Unit) })
            // Really should use lilv_world_get_symbol()
            if(LilvNodePtr symbol{ lilv_world_get_symbol(gWorld, unit.get()) })
               controlPort->mUnits = LilvString(symbol.get());

         // Get the scale points
         {
            LilvScalePointsPtr points{
               lilv_port_get_scale_points(mPlug, port) };
            LILV_FOREACH(scale_points, j, points.get()) {
               const auto point = lilv_scale_points_get(points.get(), j);
               controlPort->mScaleValues.push_back(
                  lilv_node_as_float(lilv_scale_point_get_value(point)));
               controlPort->mScaleLabels.push_back(
                  LilvString(lilv_scale_point_get_label(point)));
            }
         }

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
         if (isInput) {
            if (lilv_port_has_property(mPlug, port, node_Toggled))
               controlPort->mToggle = true;
            else if (lilv_port_has_property(mPlug, port, node_Enumeration))
               controlPort->mEnumeration = true;
            else if (lilv_port_has_property(mPlug, port, node_Integer))
               controlPort->mInteger = true;
            else if (lilv_port_has_property(mPlug, port, node_SampleRate))
               controlPort->mSampleRate = true;

            // Trigger properties can be combined with other types, but it
            // seems mostly to be combined with toggle.  So, we turn the
            // checkbox into a button.
            if (lilv_port_has_property(mPlug, port, node_Trigger))
               controlPort->mTrigger = true;

            // We'll make the slider logarithmic
            if (lilv_port_has_property(mPlug, port, node_Logarithmic))
               controlPort->mLogarithmic = true;

            if (lilv_port_has_property(mPlug, port, node_Enumeration))
               controlPort->mEnumeration = true;

            mControlPortMap[controlPort->mIndex] = controlPort;
         }
         else if (controlPort->mIndex == latencyIndex)
            mLatencyPort = i;
      }
      // Check for atom ports
      else if (lilv_port_is_a(mPlug, port, node_AtomPort)) {
         mAtomPorts.push_back(std::make_shared<LV2AtomPort>(
            port, index, isInput, symbol, name, groupName));
         const auto atomPort = mAtomPorts.back();

         atomPort->mMinimumSize = 8192;
         if (LilvNodePtr min{ lilv_port_get(mPlug, port, node_MinimumSize) }
            ; lilv_node_is_int(min.get())
         ){
            if (auto value = lilv_node_as_int(min.get())
               ; value > 0
            )
               atomPort->mMinimumSize =
                  std::max<uint32_t>(atomPort->mMinimumSize, value);
         }

         atomPort->mBuffer.resize(atomPort->mMinimumSize);
         atomPort->mRing.reset(zix_ring_new(atomPort->mMinimumSize));
         zix_ring_mlock(atomPort->mRing.get());

         if (lilv_port_supports_event(mPlug, port, node_Position))
            atomPort->mWantsPosition = true;

         if (lilv_port_supports_event(mPlug, port, node_MidiEvent)) {
            atomPort->mIsMidi = true;
            (isInput ? mMidiIn : mMidiOut) += 1;
         }

         bool isControl = lilv_node_equals(designation.get(), node_Control);
         if (isInput) {
            if (!mControlIn || isControl)
               mControlIn = atomPort;
         }
         else if (!mControlOut || isControl)
            mControlOut = atomPort;
      }
      // Check for CV ports
      else if (lilv_port_is_a(mPlug, port, node_CVPort)) {
         mCVPorts.push_back(std::make_shared<LV2CVPort>(
            port, index, isInput, symbol, name, groupName));
         const auto cvPort = mCVPorts.back();
      
         // Collect the value and range info
         if (!std::isnan(minimumVals[i])) {
            cvPort->mHasLo = true;
            cvPort->mMin = minimumVals[i];
         }
         if (!std::isnan(maximumVals[i])) {
            cvPort->mHasHi = true;
            cvPort->mMax = maximumVals[i];
         }
         if (!std::isnan(defaultVals[i]))
            cvPort->mDef = defaultVals[i];
         else if (cvPort->mHasLo)
            cvPort->mDef = cvPort->mMin;
         else if (cvPort->mHasHi)
            cvPort->mDef = cvPort->mMax;
      }
   }
   }

   // Ignore control designation if one of them is missing
   if ((mControlIn && !mControlOut) || (!mControlIn && mControlOut)) {
      mControlIn.reset();
      mControlOut.reset();
   }

   // Determine available extensions
   mWantsOptionsInterface = false;
   mWantsStateInterface = false;
   if (LilvNodesPtr extdata{ lilv_plugin_get_extension_data(mPlug) }) {
      LILV_FOREACH(nodes, i, extdata.get()) {
         const auto node = lilv_nodes_get(extdata.get(), i);
         const auto uri = lilv_node_as_string(node);
         if (strcmp(uri, LV2_OPTIONS__interface) == 0)
            mWantsOptionsInterface = true;
         else if (strcmp(uri, LV2_STATE__interface) == 0)
            mWantsStateInterface = true;
      }
   }

   return true;
}

std::shared_ptr<EffectInstance> LV2Effect::MakeInstance() const
{
   return const_cast<LV2Effect*>(this)->DoMakeInstance();
}

std::shared_ptr<EffectInstance> LV2Effect::DoMakeInstance()
{
   int userBlockSize;
   GetConfig(*this, PluginSettings::Shared, wxT("Settings"),
      wxT("BufferSize"), userBlockSize, 8192);
   mUserBlockSize = std::max(1, userBlockSize);
   mBlockSize = mUserBlockSize;

   GetConfig(*this, PluginSettings::Shared, wxT("Settings"),
      wxT("UseLatency"), mUseLatency, true);
   GetConfig(*this, PluginSettings::Shared, wxT("Settings"), wxT("UseGUI"),
      mUseGUI, true);

   lv2_atom_forge_init(&mForge, URIDMapFeature());

   return std::make_shared<Instance>(*this);
}

unsigned LV2Effect::GetAudioInCount() const
{
   return mAudioIn;
}

unsigned LV2Effect::GetAudioOutCount() const
{
   return mAudioOut;
}

int LV2Effect::GetMidiInCount() const
{
   return mMidiIn;
}

int LV2Effect::GetMidiOutCount() const
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
   mBlockSize = std::max(mMinBlockSize,
      std::min({maxBlockSize, mUserBlockSize, mMaxBlockSize}));
   if (mMaster)
      mMaster->SetBlockSize();
   for (size_t i = 0, cnt = mSlaves.size(); i < cnt; ++i)
      mSlaves[i]->SetBlockSize();
   return mBlockSize;
}

size_t LV2Effect::GetBlockSize() const
{
   return mBlockSize;
}

sampleCount LV2Effect::GetLatency()
{
   if (mUseLatency && mLatencyPort >= 0 && !mLatencyDone) {
      mLatencyDone = true;
      return sampleCount(mMaster->GetLatency());
   }
   return 0;
}

bool LV2Effect::ProcessInitialize(
   EffectSettings &, sampleCount, ChannelNames chanMap)
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

   mProcess->Activate();

   mLatencyDone = false;

   return true;
}

bool LV2Effect::ProcessFinalize()
{
   mProcess.reset();
   return true;
}

size_t LV2Effect::ProcessBlock(EffectSettings &,
   const float *const *inbuf, float *const *outbuf, size_t size)
{
   using namespace LV2Symbols;
   wxASSERT(size <= ( size_t) mBlockSize);

   LilvInstance *instance = mProcess->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mAudioPorts)
   {
      lilv_instance_connect_port(instance,
         port->mIndex,
         const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));
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

         const auto ring = port->mRing.get();
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

   // Main thread consumes responses
   mProcess->ConsumeResponses();

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

bool LV2Effect::RealtimeInitialize(EffectSettings &)
{
   for (auto & port : mCVPorts)
      port->mBuffer.reinit((unsigned) mBlockSize, port->mIsInput);
   return true;
}

bool LV2Effect::RealtimeFinalize(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   mSlaves.clear();
   for (auto & port : mCVPorts)
      port->mBuffer.reset();
   return true;
});
}

bool LV2Effect::RealtimeAddProcessor(
   EffectSettings &, unsigned, float sampleRate)
{
   auto pInstance = InitInstance(sampleRate);
   if (!pInstance)
      return false;
   pInstance->Activate();
   mSlaves.push_back(move(pInstance));
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

bool LV2Effect::RealtimeProcessStart(EffectSettings &)
{
   using namespace LV2Symbols;
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

         const auto ring = port->mRing.get();
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

size_t LV2Effect::RealtimeProcess(size_t group, EffectSettings &,
   const float *const *inbuf, float *const *outbuf, size_t numSamples)
{
   if (group >= mSlaves.size())
      return 0;
   wxASSERT(numSamples <= (size_t) mBlockSize);

   if (group < 0 || group >= (int) mSlaves.size())
   {
      return 0;
   }

   const auto slave = mSlaves[group].get();
   LilvInstance *instance = slave->GetInstance();

   int i = 0;
   int o = 0;
   for (auto & port : mAudioPorts)
   {
      lilv_instance_connect_port(instance,
         port->mIndex,
         const_cast<float*>(port->mIsInput ? inbuf[i++] : outbuf[o++]));
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

   // Background thread consumes responses from yet another worker thread
   slave->ConsumeResponses();

   for (auto & port : mAtomPorts)
   {
      uint8_t *buf = port->mBuffer.data();

      if (!port->mIsInput)
      {
         port->mBuffer.resize(port->mMinimumSize);

         LV2_Atom *chunk = ( LV2_Atom *) buf;
         chunk->size = port->mMinimumSize;
         chunk->type = LV2Symbols::urid_Chunk;
      }
   }

   if (group == 0)
   {
      mPositionFrame += numSamples;
   }

   return numSamples;
}

bool LV2Effect::RealtimeProcessEnd(EffectSettings &) noexcept
{
return GuardedCall<bool>([&]{
   // Nothing to do if we did process any samples
   if (mNumSamples == 0)
   {
      return true;
   }

   for (auto & port : mAtomPorts)
   {
      if (!port->mIsInput)
      {
         const auto ring = port->mRing.get();

         LV2_ATOM_SEQUENCE_FOREACH((LV2_Atom_Sequence *) port->mBuffer.data(), ev)
         {
            zix_ring_write(ring, &ev->body, ev->body.size + sizeof(LV2_Atom));
         }
      }
   }

   mNumSamples = 0;

   return true;
});
}

int LV2Effect::ShowClientInterface(
   wxWindow &parent, wxDialog &dialog, bool forceModal)
{
   // Remember the dialog with a weak pointer, but don't control its lifetime
   mDialog = &dialog;

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
      return 0;
   }

   return mDialog->ShowModal();
}

bool LV2Effect::SaveSettings(
   const EffectSettings &, CommandParameters & parms) const
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

bool LV2Effect::LoadSettings(
   const CommandParameters & parms, EffectSettings &settings) const
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

std::unique_ptr<EffectUIValidator> LV2Effect::PopulateUI(ShuttleGui &S,
   EffectInstance &, EffectSettingsAccess &access)
{
   auto parent = S.GetParent();
   mParent = parent;

   mParent->PushEventHandler(this);

   mSuilHost.reset();
   mSuilInstance.reset();

   mMaster = InitInstance(mSampleRate);
   if (!mMaster) {
      AudacityMessageBox( XO("Couldn't instantiate effect") );
      return nullptr;
   }

   // Determine if the GUI editor is supposed to be used or not
   GetConfig(*this, PluginSettings::Shared, wxT("Settings"),
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
      if (!BuildPlain(access))
         return nullptr;
   }

   return std::make_unique<DefaultEffectUIValidator>(*this, access);
}

bool LV2Effect::IsGraphicalUI()
{
   return mUseGUI;
}

bool LV2Effect::ValidateUI(EffectSettings &settings)
{
   if (GetType() == EffectTypeGenerate)
      settings.extra.SetDuration(mDuration->GetValue());

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

      mUIIdleInterface = nullptr;
      mUIShowInterface = nullptr;
      mExternalWidget = nullptr;

      mSuilInstance.reset();
   }

   mSuilHost.reset();

   mMaster.reset();

   mParent = NULL;
   mDialog = NULL;

   // Restore initial state
   mExtensionDataFeature = {};

   return true;
}

bool LV2Effect::LoadUserPreset(
   const RegistryPath &name, EffectSettings &settings) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<LV2Effect*>(this)->DoLoadUserPreset(name, settings);
}

bool LV2Effect::DoLoadUserPreset(
   const RegistryPath &name, EffectSettings &settings)
{
   if (!LoadParameters(name, settings))
   {
      return false;
   }

   return TransferDataToWindow();
}

bool LV2Effect::SaveUserPreset(
   const RegistryPath &name, const EffectSettings &settings) const
{
   return SaveParameters(name, settings);
}

RegistryPaths LV2Effect::GetFactoryPresets() const
{
   using namespace LV2Symbols;
   if (mFactoryPresetsLoaded)
      return mFactoryPresetNames;

   if (LilvNodesPtr presets{ lilv_plugin_get_related(mPlug, node_Preset) }) {
      LILV_FOREACH(nodes, i, presets.get()) {
         const auto preset = lilv_nodes_get(presets.get(), i);

         mFactoryPresetUris.push_back(LilvString(preset));

         lilv_world_load_resource(gWorld, preset);

         if (LilvNodesPtr labels{ lilv_world_find_nodes(gWorld, preset,
            node_Label, nullptr) }) {
            const auto label = lilv_nodes_get_first(labels.get());
            mFactoryPresetNames.push_back(LilvString(label));
         }
         else
            mFactoryPresetNames.push_back(
               LilvString(preset).AfterLast(wxT('#')));
      }
   }

   mFactoryPresetsLoaded = true;

   return mFactoryPresetNames;
}

bool LV2Effect::LoadFactoryPreset(int id, EffectSettings &) const
{
   // To do: externalize state so const_cast isn't needed
   return const_cast<LV2Effect*>(this)->DoLoadFactoryPreset(id);
}

bool LV2Effect::DoLoadFactoryPreset(int id)
{
   using namespace LV2Symbols;
   if (id < 0 || id >= (int) mFactoryPresetUris.size())
   {
      return false;
   }

   LilvNodePtr preset{ lilv_new_uri(gWorld, mFactoryPresetUris[id].ToUTF8()) };
   if (!preset)
      return false;

   using LilvStatePtr = Lilv_ptr<LilvState, lilv_state_free>;
   if (LilvStatePtr state{
      lilv_state_new_from_world(gWorld, URIDMapFeature(), preset.get()) } ) {
      lilv_state_restore(
         state.get(), mMaster->GetInstance(), set_value_func, this, 0, nullptr);
      TransferDataToWindow();
      return true;
   }
   else
      return false;
}

bool LV2Effect::CanExportPresets()
{
   return false;
}

void LV2Effect::ExportPresets(const EffectSettings &) const
{
}

void LV2Effect::ImportPresets(EffectSettings &)
{
}

bool LV2Effect::HasOptions()
{
   return true;
}

void LV2Effect::ShowOptions()
{
   LV2EffectSettingsDialog dlg(mParent, *this);
   if (dlg.ShowModal() == wxID_OK)
   {
      // Reinitialize configuration settings
      int userBlockSize;
      GetConfig(*this, PluginSettings::Shared, wxT("Settings"),
         wxT("BufferSize"), userBlockSize, DEFAULT_BLOCKSIZE);
      mUserBlockSize = std::max(1, userBlockSize);
      GetConfig(*this, PluginSettings::Shared, wxT("Settings"),
         wxT("UseLatency"), mUseLatency, true);
   }
}

// ============================================================================
// LV2Effect Implementation
// ============================================================================

bool LV2Effect::LoadParameters(
   const RegistryPath &group, EffectSettings &settings)
{
   wxString parms;
   if (!GetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms, wxEmptyString))
   {
      return false;
   }

   CommandParameters eap;
   if (!eap.SetParameters(parms))
   {
      return false;
   }

   return LoadSettings(eap, settings);
}

bool LV2Effect::SaveParameters(
   const RegistryPath &group, const EffectSettings &settings) const
{
   CommandParameters eap;
   if (!SaveSettings(settings, eap))
      return false;

   wxString parms;
   if (!eap.GetParameters(parms))
      return false;

   return SetConfig(*this,
      PluginSettings::Private, group, wxT("Parameters"), parms);
}

std::unique_ptr<LV2Wrapper> LV2Effect::InitInstance(float sampleRate)
{
   auto wrapper = std::make_unique<LV2Wrapper>(*this, mPlug, sampleRate);
   auto instance = wrapper->GetInstance();
   if (!instance)
      return nullptr;

   wrapper->SetBlockSize();
   wrapper->SetSampleRate();

   // Connect all control ports
   for (auto & port : mControlPorts)
      // If it's not an input port and master has already been created
      // then connect the port to a dummy field since slave output port
      // values are unwanted as the master values will be used.
      //
      // Otherwise, connect it to the real value field.
      lilv_instance_connect_port(instance, port->mIndex,
         !port->mIsInput && mMaster ? &port->mDmy : &port->mVal);

   // Connect all atom ports
   for (auto & port : mAtomPorts)
      lilv_instance_connect_port(instance, port->mIndex, port->mBuffer.data());

   // We don't fully support CV ports, so connect them to dummy buffers for now.
   for (auto & port : mCVPorts)
      lilv_instance_connect_port(instance, port->mIndex, port->mBuffer.get());

   // Give plugin a chance to initialize.  The SWH plugins (like AllPass) need
   // this before it can be safely deleted.
   lilv_instance_activate(instance);
   lilv_instance_deactivate(instance);

   for (auto & port : mAtomPorts)
      if (!port->mIsInput)
         LV2_ATOM_SEQUENCE_FOREACH(
            reinterpret_cast<LV2_Atom_Sequence *>(port->mBuffer.data()), ev) {
            zix_ring_write(port->mRing.get(),
               &ev->body, ev->body.size + sizeof(LV2_Atom));
         }

   return wrapper;
}

bool LV2Effect::BuildFancy()
{
   using namespace LV2Symbols;
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
   const LilvUI *ui = nullptr;
   const LilvNode *uiType = nullptr;
   LilvUIsPtr uis{ lilv_plugin_get_uis(mPlug) };
   if (uis) {
      if (LilvNodePtr containerType{ lilv_new_uri(gWorld, nativeType) }) {
         LILV_FOREACH(uis, iter, uis.get()) {
            ui = lilv_uis_get(uis.get(), iter);
            if (lilv_ui_is_supported(ui,
               suil_ui_supported, containerType.get(), &uiType))
               break;
            if (lilv_ui_is_a(ui, node_Gtk) || lilv_ui_is_a(ui, node_Gtk3)) {
               uiType = node_Gtk;
               break;
            }
            ui = nullptr;
         }
      }
   }

   // Check for other supported UIs
   if (!ui && uis) {
      LILV_FOREACH(uis, iter, uis.get()) {
         ui = lilv_uis_get(uis.get(), iter);
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
      return false;

   const LilvNode *uinode = lilv_ui_get_uri(ui);
   lilv_world_load_resource(gWorld, uinode);
   if (!ValidateFeatures(uinode))
      return false;

   const char *containerType;

   if (uiType == node_ExternalUI)
   {
      containerType = LV2_EXTERNAL_UI__Widget;
   }
   else
   {
      containerType = nativeType;
      mFeatures[mParentFeature].data = mParent->GetHandle();

#if defined(__WXGTK__)
      // Make sure the parent has a window
      if (!gtk_widget_get_window(GTK_WIDGET(mParent->m_wxwindow)))
      {
         gtk_widget_realize(GTK_WIDGET(mParent->m_wxwindow));
      }
#endif
   }

   LilvInstance *instance = mMaster->GetInstance();
   mFeatures[mInstanceAccessFeature].data = lilv_instance_get_handle(instance);
   mExtensionDataFeature =
      { lilv_instance_get_descriptor(instance)->extension_data };

   // Set before creating the UI instance so the initial size (if any) can be captured
   mNativeWinInitialSize = wxDefaultSize;
   mNativeWinLastSize = wxDefaultSize;

   // Create the suil host
   mSuilHost.reset(suil_host_new(LV2Effect::suil_port_write_func,
      LV2Effect::suil_port_index_func, nullptr, nullptr));
   if (!mSuilHost)
      return false;

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   LilvCharsPtr libPath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)),
      nullptr)
   };
   const auto path = wxPathOnly(libPath.get());
   SetDllDirectory(path.c_str());
#endif

   LilvCharsPtr bundlePath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)), nullptr)
   };
   LilvCharsPtr binaryPath{
      lilv_file_uri_parse(lilv_node_as_uri(lilv_ui_get_binary_uri(ui)), nullptr)
   };

   mSuilInstance.reset(suil_instance_new(mSuilHost.get(), this, containerType,
      lilv_node_as_uri(lilv_plugin_get_uri(mPlug)),
      lilv_node_as_uri(lilv_ui_get_uri(ui)), lilv_node_as_uri(uiType),
      bundlePath.get(), binaryPath.get(), GetFeaturePointers().data()));

   // Bail if the instance (no compatible UI) couldn't be created
   if (!mSuilInstance)
   {
#if defined(__WXMSW__)
      SetDllDirectory(NULL);
#endif

      mSuilHost.reset();

      return false;
   }

   if (uiType == node_ExternalUI)
   {
      mParent->SetMinSize(wxDefaultSize);

      mExternalWidget = static_cast<LV2_External_UI_Widget *>(
         suil_instance_get_widget(mSuilInstance.get()));
      mTimer.SetOwner(this, ID_TIMER);
      mTimer.Start(20);

      LV2_EXTERNAL_UI_SHOW(mExternalWidget);
   }
   else
   {
      const auto widget = static_cast<WXWidget>(
         suil_instance_get_widget(mSuilInstance.get()));

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
         return false;

      mParent->SetSizerAndFit(vs.release());
   }

   mUIIdleInterface = static_cast<const LV2UI_Idle_Interface *>(
      suil_instance_extension_data(mSuilInstance.get(), LV2_UI__idleInterface));

   mUIShowInterface = static_cast<const LV2UI_Show_Interface *>(
      suil_instance_extension_data(mSuilInstance.get(), LV2_UI__showInterface));

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

bool LV2Effect::BuildPlain(EffectSettingsAccess &access)
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
            auto &extra = access.Get().extra;
            mDuration = safenew
               NumericTextCtrl(w, ID_Duration,
                               NumericConverter::TIME,
                               extra.GetDurationFormat(),
                               extra.GetDuration(),
                               mSampleRate,
                               NumericTextCtrl::Options {}
            .AutoPos(true));
            mDuration->SetName( XO("Duration") );
            sizer->Add(mDuration, 0, wxALIGN_CENTER | wxALL, 5);

            groupSizer->Add(sizer.release(), 0, wxALIGN_CENTER | wxALL, 5);
            innerSizer->Add(groupSizer.release(), 0, wxEXPAND | wxALL, 5);
         }

         std::sort(mGroups.begin(), mGroups.end(), TranslationLess);

         for (size_t i = 0, groupCount = mGroups.size(); i < groupCount; i++)
         {
            const auto &label = mGroups[i];
            auto groupSizer = std::make_unique<wxStaticBoxSizer>(
               wxVERTICAL, w, label.Translation());

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
                  auto s = port->Discretize(port->mVal);
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
         for (auto & port : mControlPorts)
            if (port->mIsInput)
               suil_instance_port_event(mSuilInstance.get(),
                  port->mIndex, sizeof(float), 0, &port->mVal);
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
            auto s = port->Discretize(port->mVal);
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
      const auto handle = suil_instance_get_handle(mSuilInstance.get());
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
      const auto ring = mControlOut->mRing.get();

      LV2_Atom *atom = (LV2_Atom *) malloc(mControlOut->mMinimumSize);
      if (atom)
      {
         while (zix_ring_read(ring, atom, sizeof(LV2_Atom)))
         {
            uint32_t size = lv2_atom_total_size(atom);

            if (size < mControlOut->mMinimumSize)
            {
               zix_ring_read(ring,
                  LV2_ATOM_CONTENTS(LV2_Atom, atom), atom->size);
               suil_instance_port_event(mSuilInstance.get(),
                  mControlOut->mIndex, size,
                  LV2Symbols::urid_EventTransfer, atom);
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
         suil_instance_port_event(mSuilInstance.get(),
            port->mIndex, sizeof(port->mVal), 0, &port->mVal);
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
int LV2Effect::ui_resize(LV2UI_Feature_Handle handle, int width, int height)
{
   return static_cast<LV2Effect *>(handle)->UIResize(width, height);
}

int LV2Effect::UIResize(int width, int height)
{
   // Queue a wxSizeEvent to resize the plugins UI
   if (mNativeWin) {
      wxSizeEvent sw{ wxSize{ width, height } };
      sw.SetEventObject(mNativeWin);
      mNativeWin->GetEventHandler()->AddPendingEvent(sw);
   }
   else
      // The window hasn't been created yet, so record the desired size
      mNativeWinInitialSize = { width, height };
   return 0;
}

// static callback
void LV2Effect::ui_closed(LV2UI_Controller controller)
{
   return static_cast<LV2Effect *>(controller)->UIClosed();
}

void LV2Effect::UIClosed()
{
   mExternalUIClosed = true;
}

// static callback
void LV2Effect::suil_port_write_func(SuilController controller,
   uint32_t port_index, uint32_t buffer_size, uint32_t protocol,
   const void *buffer)
{
   static_cast<LV2Effect *>(controller)
      ->SuilPortWrite(port_index, buffer_size, protocol, buffer);
}

void LV2Effect::SuilPortWrite(uint32_t port_index,
   uint32_t buffer_size, uint32_t protocol, const void *buffer)
{
   // Handle implicit floats
   if (protocol == 0 && buffer_size == sizeof(float)) {
      if (auto it = mControlPortMap.find(port_index);
         it != mControlPortMap.end())
         it->second->mVal = *static_cast<const float *>(buffer);
   }
   // Handle event transfers
   else if (protocol == LV2Symbols::urid_EventTransfer) {
      if (mControlIn && port_index == mControlIn->mIndex)
         zix_ring_write(mControlIn->mRing.get(), buffer, buffer_size);
   }
}

// static callback
uint32_t LV2Effect::suil_port_index_func(
   SuilController controller, const char *port_symbol)
{
   return static_cast<LV2Effect *>(controller)->SuilPortIndex(port_symbol);
}

uint32_t LV2Effect::SuilPortIndex(const char *port_symbol)
{
   for (size_t i = 0, cnt = lilv_plugin_get_num_ports(mPlug); i < cnt; ++i) {
      const auto port = lilv_plugin_get_port_by_index(mPlug, i);
      if (strcmp(port_symbol,
            lilv_node_as_string(lilv_port_get_symbol(mPlug, port))) == 0)
         return lilv_port_get_index(mPlug, port);
   }
   return LV2UI_INVALID_PORT_INDEX;
}

// static callback
const void *LV2Effect::get_value_func(
   const char *port_symbol, void *user_data, uint32_t *size, uint32_t *type)
{
   return static_cast<LV2Effect *>(user_data)
      ->GetPortValue(port_symbol, size, type);
}

const void *LV2Effect::GetPortValue(
   const char *port_symbol, uint32_t *size, uint32_t *type)
{
   wxString symbol = wxString::FromUTF8(port_symbol);
   for (auto & port : mControlPorts)
      if (port->mSymbol == symbol) {
         *size = sizeof(float);
         *type = LV2Symbols::urid_Float;
         return &port->mVal;
      }
   *size = 0;
   *type = 0;
   return nullptr;
}

// static callback
void LV2Effect::set_value_func(
   const char *port_symbol, void *user_data,
   const void *value, uint32_t size, uint32_t type)
{
   static_cast<LV2Effect *>(user_data)
      ->SetPortValue(port_symbol, value, size, type);
}

void LV2Effect::SetPortValue(
   const char *port_symbol, const void *value, uint32_t size, uint32_t type)
{
   wxString symbol = wxString::FromUTF8(port_symbol);
   for (auto & port : mControlPorts)
      if (port->mSymbol == symbol) {
         using namespace LV2Symbols;
         if (type == urid_Bool && size == sizeof(bool))
            port->mVal = *static_cast<const bool *>(value) ? 1.0f : 0.0f;
         else if (type == urid_Double && size == sizeof(double))
            port->mVal = *static_cast<const double *>(value);
         else if (type == urid_Float && size == sizeof(float))
            port->mVal = *static_cast<const float *>(value);
         else if (type == urid_Int && size == sizeof(int32_t))
            port->mVal = *static_cast<const int32_t *>(value);
         else if (type == urid_Long && size == sizeof(int64_t))
            port->mVal = *static_cast<const int64_t *>(value);
         break;
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

LV2Wrapper::~LV2Wrapper()
{
   if (mInstance) {
      if (mThread.joinable()) {
         mStopWorker = true;
         mRequests.Post({ 0, NULL });  // Must do after writing mStopWorker
         mThread.join();
      }
      Deactivate();
   }
}

LV2Wrapper::LV2Wrapper(const LV2FeaturesList &featuresList,
   const LilvPlugin *plugin, double sampleRate)
:  mFeaturesList{ featuresList }
{
   auto features = mFeaturesList.GetFeaturePointers();
   LV2_Feature tempFeature{ LV2_WORKER__schedule, &mWorkerSchedule };
   if (mFeaturesList.SuppliesWorkerInterface())
      // Insert another pointer before the null
      // Append a feature to the array, only for the plugin instantiation
      // (features are also used elsewhere to instantiate the UI in the
      // suil_* functions)
      // It informs the plugin how to send work to another thread
      features.insert(features.end() - 1, &tempFeature);

#if defined(__WXMSW__)
   // Plugins may have dependencies that need to be loaded from the same path
   // as the main DLL, so add this plugin's path to the DLL search order.
   const auto libNode = lilv_plugin_get_library_uri(plugin);
   const auto libUri = lilv_node_as_uri(libNode);
   LilvCharsPtr libPath{ lilv_file_uri_parse(libUri, nullptr) };
   const auto path = wxPathOnly(libPath.get());
   SetDllDirectory(path.c_str());
#endif

   mInstance.reset(
      lilv_plugin_instantiate(plugin, sampleRate, features.data()));

#if defined(__WXMSW__)
   SetDllDirectory(nullptr);
#endif

   if (!mInstance)
      return;

   mHandle = lilv_instance_get_handle(mInstance.get());
   mOptionsInterface = static_cast<const LV2_Options_Interface *>(
      lilv_instance_get_extension_data(mInstance.get(), LV2_OPTIONS__interface));
   mStateInterface = static_cast<const LV2_State_Interface *>(
      lilv_instance_get_extension_data(mInstance.get(), LV2_STATE__interface));
   mWorkerInterface = static_cast<const LV2_Worker_Interface *>(
      lilv_instance_get_extension_data(mInstance.get(), LV2_WORKER__interface));
   if (mFeaturesList.LatencyPort() >= 0)
      lilv_instance_connect_port(mInstance.get(), mFeaturesList.LatencyPort(), &mLatency);
   if (mWorkerInterface)
      mThread = std::thread{
         std::mem_fn( &LV2Wrapper::ThreadFunction ), std::ref(*this)
      };
}

void LV2Wrapper::Activate()
{
   if (!mActivated) {
      lilv_instance_activate(GetInstance());
      mActivated = true;
   }
}

void LV2Wrapper::Deactivate()
{
   if (mActivated) {
      lilv_instance_deactivate(GetInstance());
      mActivated = false;
   }
}

LilvInstance *LV2Wrapper::GetInstance() const
{
   return mInstance.get();
}

LV2_Handle LV2Wrapper::GetHandle() const
{
   return mHandle;
}

float LV2Wrapper::GetLatency() const
{
   return mLatency;
}

// Where is this called?
void LV2Wrapper::SetFreeWheeling(bool enable)
{
   mFreeWheeling = enable;
}

void LV2Wrapper::SetSampleRate()
{
   if (auto pOption = mFeaturesList.SampleRateOption()
      ; pOption && mOptionsInterface && mOptionsInterface->set
   ){
      LV2_Options_Option options[2]{ *pOption, {} };
      mOptionsInterface->set(mHandle, options);
   }
}

void LV2Wrapper::SetBlockSize()
{
   if (auto pOption = mFeaturesList.NominalBlockLengthOption()
      ; pOption && mOptionsInterface && mOptionsInterface->set
   ){
      LV2_Options_Option options[2]{ *pOption, {} };
      mOptionsInterface->set(mHandle, options);
   }
}

// Thread body
void LV2Wrapper::ThreadFunction()
{
   for (LV2Work work{};
      // Must test mStopWorker only after reading mRequests
      mRequests.Receive(work) == wxMSGQUEUE_NO_ERROR && !mStopWorker;
   )
      mWorkerInterface->work(mHandle, respond, this, work.size, work.data);
}

void LV2Wrapper::ConsumeResponses()
{
   if (mWorkerInterface) {
      LV2Work work{};
      while (mResponses.ReceiveTimeout(0, work) == wxMSGQUEUE_NO_ERROR)
         mWorkerInterface->work_response(mHandle, work.size, work.data);
      if (mWorkerInterface->end_run)
         mWorkerInterface->end_run(mHandle);
   }
}

// static callback
LV2_Worker_Status LV2Wrapper::schedule_work(LV2_Worker_Schedule_Handle handle,
   uint32_t size, const void *data)
{
   return static_cast<LV2Wrapper *>(handle)->ScheduleWork(size, data);
}

LV2_Worker_Status LV2Wrapper::ScheduleWork(uint32_t size, const void *data)
{
   if (mFreeWheeling)
      // Not using another thread
      return mWorkerInterface->work(mHandle, respond, this, size, data);
   else {
      // Put in the queue for the worker thread
      const auto err = mRequests.Post({ size, data });
      return (err == wxMSGQUEUE_NO_ERROR)
         ? LV2_WORKER_SUCCESS : LV2_WORKER_ERR_UNKNOWN;
   }
}

// static callback
LV2_Worker_Status LV2Wrapper::respond(
   LV2_Worker_Respond_Handle handle, uint32_t size, const void *data)
{
   return static_cast<LV2Wrapper*>(handle)->Respond(size, data);
}

LV2_Worker_Status LV2Wrapper::Respond(uint32_t size, const void *data)
{
   // Put in the queue, for another thread (if not "freewheeling")
   // (not necessarily the main thread)
   const auto err = mResponses.Post({ size, data });
   return (err == wxMSGQUEUE_NO_ERROR)
      ? LV2_WORKER_SUCCESS : LV2_WORKER_ERR_UNKNOWN;
}

#endif
