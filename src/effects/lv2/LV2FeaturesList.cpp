/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2FeaturesList.cpp

  Paul Licameli split from LV2Effect.cpp

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

#include "LV2FeaturesList.h"
#include <wx/crt.h>
#include <wx/log.h>
#include "lv2/buf-size/buf-size.h"
#include "lv2_external_ui.h"
#include "lv2/worker/worker.h"

LV2FeaturesList::LV2FeaturesList(const LilvPlugin *plug) : mPlug{ plug }
   , mSuppliesWorkerInterface{ SuppliesWorkerInterface(plug) }
{
}

bool LV2FeaturesList::InitializeOptions()
{
   using namespace LV2Symbols;

   // Construct the null-terminated array describing options, and validate it
   AddOption(urid_SequenceSize, sizeof(mSeqSize), urid_Int, &mSeqSize);
   AddOption(urid_MinBlockLength,
      sizeof(mMinBlockSize), urid_Int, &mMinBlockSize);
   AddOption(urid_MaxBlockLength,
      sizeof(mMaxBlockSize), urid_Int, &mMaxBlockSize);
   // Two options are reset later
   mBlockSizeOption = AddOption(urid_NominalBlockLength,
      sizeof(mBlockSize), urid_Int, &mBlockSize);
   mSampleRateOption = AddOption(urid_SampleRate,
      sizeof(mSampleRate), urid_Float, &mSampleRate);
   AddOption(0, 0, 0, nullptr);
   if (!ValidateOptions(lilv_plugin_get_uri(mPlug)))
      return false;

   // Adjust the values in the block size features according to the plugin
   if (LilvNodePtr minLength{ lilv_world_get(gWorld,
         lilv_plugin_get_uri(mPlug), node_MinBlockLength, nullptr) }
      ; lilv_node_is_int(minLength.get())
   ){
      if (auto value = lilv_node_as_int(minLength.get())
         ; value >= 0
      )
         mMinBlockSize = std::max<size_t>(mMinBlockSize, value);
   }
   if (LilvNodePtr maxLength{ lilv_world_get(gWorld,
         lilv_plugin_get_uri(mPlug), node_MaxBlockLength, nullptr) }
      ; lilv_node_is_int(maxLength.get())
   ){
      if (auto value = lilv_node_as_int(maxLength.get())
         ; value >= 1
      )
         mMaxBlockSize = std::min<size_t>(mMaxBlockSize, value);
   }
   mMaxBlockSize = std::max(mMaxBlockSize, mMinBlockSize);

   return true;
}

bool LV2FeaturesList::InitializeFeatures()
{
   // Construct null-terminated array of "features" describing our capabilities
   // to lv2, and validate
   AddFeature(LV2_UI__noUserResize, nullptr);
   AddFeature(LV2_UI__fixedSize, nullptr);
   AddFeature(LV2_UI__idleInterface, nullptr);
   AddFeature(LV2_UI__makeResident, nullptr);
   AddFeature(LV2_BUF_SIZE__boundedBlockLength, nullptr);
   AddFeature(LV2_BUF_SIZE__fixedBlockLength, nullptr);
   AddFeature(LV2_OPTIONS__options, mOptions.data());
   AddFeature(LV2_URI_MAP_URI, &mUriMapFeature);
   AddFeature(LV2_URID__map, &mURIDMapFeature);
   AddFeature(LV2_URID__unmap, &mURIDUnmapFeature);
   AddFeature(LV2_LOG__log, &mLogFeature);
   // Some plugins specify this as a feature
   AddFeature(LV2_EXTERNAL_UI__Widget, nullptr);
   AddFeature(LV2_UI__parent, nullptr);

   return true;
}

bool LV2FeaturesList::SuppliesWorkerInterface(const LilvPlugin *plug)
{
   bool result = false;
   if (LilvNodesPtr extdata{ lilv_plugin_get_extension_data(plug) }) {
      LILV_FOREACH(nodes, i, extdata.get()) {
         const auto node = lilv_nodes_get(extdata.get(), i);
         const auto uri = lilv_node_as_string(node);
         if (strcmp(uri, LV2_WORKER__interface) == 0)
            result = true;
      }
   }
   return result;
}

size_t LV2FeaturesList::AddOption(
   LV2_URID key, uint32_t size, LV2_URID type, const void *value)
{
   int ndx = mOptions.size();
   if (key != 0)
      mOptions.emplace_back(LV2_Options_Option{
         LV2_OPTIONS_INSTANCE, 0, key, size, type, value });
   else
      mOptions.emplace_back(LV2_Options_Option{});
   return ndx;
}

void LV2FeaturesList::AddFeature(const char *uri, const void *data)
{
   // This casting to const is innocent
   // We pass our "virtual function tables" or array of options, which the
   // library presumably will not change
   mFeatures.emplace_back(LV2_Feature{ uri, const_cast<void*>(data) });
}

auto LV2FeaturesList::GetFeaturePointers() const -> FeaturePointers
{
   FeaturePointers result;
   for (auto &feature : mFeatures)
      result.push_back(&feature);
   result.push_back(nullptr);
   return result;
}

const LV2_Options_Option *LV2FeaturesList::NominalBlockLengthOption() const
{
   if (mSupportsNominalBlockLength)
      return &mOptions[mBlockSizeOption];
   else
      return nullptr;
}

const LV2_Options_Option *LV2FeaturesList::SampleRateOption() const
{
   if (mSupportsSampleRate)
      return &mOptions[mSampleRateOption];
   else
      return nullptr;
}

bool LV2FeaturesList::ValidateFeatures(const LilvNode *subject)
{
   return CheckFeatures(subject, true) && CheckFeatures(subject, false);
}

bool LV2FeaturesList::CheckFeatures(const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   auto predicate = required ? node_RequiredFeature : node_OptionalFeature;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }) {
      LILV_FOREACH(nodes, i, nodes.get()) {
         const auto node = lilv_nodes_get(nodes.get(), i);
         const auto uri = lilv_node_as_string(node);
         if ((strcmp(uri, LV2_UI__noUserResize) == 0) ||
             (strcmp(uri, LV2_UI__fixedSize) == 0))
            mNoResize = true;
         else if (strcmp(uri, LV2_WORKER__schedule) == 0) {
            /* Supported but handled in LV2Wrapper */
         }
         else if (required) {
            const auto end = mFeatures.end();
            supported = (end != std::find_if(mFeatures.begin(), end,
               [&](auto &feature){ return strcmp(feature.URI, uri) == 0; }));
            if (!supported) {
               wxLogError(wxT("%s requires unsupported feature %s"),
                  lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
               break;
            }
         }
      }
   }
   return supported;
}

bool LV2FeaturesList::ValidateOptions(const LilvNode *subject)
{
   return CheckOptions(subject, true) && CheckOptions(subject, false);
}

bool LV2FeaturesList::CheckOptions(const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   const auto predicate =
      required ? node_RequiredOption : node_SupportedOption;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }) {
      LILV_FOREACH(nodes, i, nodes.get()) {
         const auto node = lilv_nodes_get(nodes.get(), i);
         const auto uri = lilv_node_as_string(node);
         const auto urid = URID_Map(uri);
         if (urid == urid_NominalBlockLength)
            mSupportsNominalBlockLength = true;
         else if (urid == urid_SampleRate)
            mSupportsSampleRate = true;
         else if (required) {
            const auto end = mOptions.end();
            supported = (end != std::find_if(mOptions.begin(), end,
               [&](const auto &option){ return option.key == urid; }));
            if (!supported) {
               wxLogError(wxT("%s requires unsupported option %s"),
                  lilv_node_as_string(lilv_plugin_get_uri(mPlug)), uri);
               break;
            }
         }
      }
   }
   return supported;
}

#if 0
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
#endif

// ============================================================================
// Feature handlers
// ============================================================================

// static callback
uint32_t LV2FeaturesList::uri_to_id(
   LV2_URI_Map_Callback_Data callback_data, const char *, const char *uri)
{
   return static_cast<LV2FeaturesList *>(callback_data)->URID_Map(uri);
}

// static callback
LV2_URID LV2FeaturesList::urid_map(LV2_URID_Map_Handle handle, const char *uri)
{
   return static_cast<LV2FeaturesList *>(handle)->URID_Map(uri);
}

LV2_URID LV2FeaturesList::URID_Map(const char *uri)
{
   using namespace LV2Symbols;
   // Map global URIs to lower indices
   auto urid = Lookup_URI(gURIDMap, uri, false);
   if (urid > 0)
      return urid;
   // Map local URIs to higher indices
   urid = Lookup_URI(mURIDMap, uri);
   if (urid > 0)
      return urid + gURIDMap.size();
   return 0;
}

// static callback
const char *LV2FeaturesList::urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid)
{
   return static_cast<LV2FeaturesList *>(handle)->URID_Unmap(urid);
}

const char *LV2FeaturesList::URID_Unmap(LV2_URID urid)
{
   using namespace LV2Symbols;
   if (urid > 0) {
      // Unmap lower indices to global URIs
      if (urid <= static_cast<LV2_URID>(gURIDMap.size()))
         return mURIDMap[urid - 1].get();
      // Unmap higher indices to local URIs
      urid -= gURIDMap.size();
      if (urid <= static_cast<LV2_URID>(mURIDMap.size()))
         return mURIDMap[urid - 1].get();
   }
   return nullptr;
}

// static callback
int LV2FeaturesList::log_printf(
   LV2_Log_Handle handle, LV2_URID type, const char *fmt, ...)
{
   va_list ap;
   int len;

   va_start(ap, fmt);
   len = static_cast<LV2FeaturesList *>(handle)->LogVPrintf(type, fmt, ap);
   va_end(ap);

   return len;
}

// static callback
int LV2FeaturesList::log_vprintf(
   LV2_Log_Handle handle, LV2_URID type, const char *fmt, va_list ap)
{
   return static_cast<LV2FeaturesList *>(handle)->LogVPrintf(type, fmt, ap);
}

int LV2FeaturesList::LogVPrintf(LV2_URID type, const char *fmt, va_list ap)
{
   using namespace LV2Symbols;
   long level = wxLOG_Error;
   if (type == urid_Error)
      level = wxLOG_Error;
   else if (type == urid_Note)
      level = wxLOG_Info;
   else if (type == urid_Trace)
      level = wxLOG_Trace;
   else if (type == urid_Warning)
      level = wxLOG_Warning;
   else
      level = wxLOG_Message;
   int len = wxCRT_VsnprintfA(nullptr, 0, fmt, ap);
   auto msg = std::make_unique<char[]>(len + 1);
   wxCRT_VsnprintfA(msg.get(), len, fmt, ap);
   wxString text(msg.get());
   wxLogGeneric(level,
      wxT("%s: %s"), GetSymbol().Msgid().Translation(), text);
   return len;
}

#endif
