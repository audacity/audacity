/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  SnappingToolBar.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <type_traits>
#include <wx/weakref.h>

#include <array>

#include "ToolBar.h"
#include "widgets/auStaticText.h"

#include "Observer.h"

class wxCheckBox;
class wxComboCtrl;
class wxSizeEvent;

class AudacityProject;

class SnappingToolBar final : public ToolBar {

 public:
   static Identifier ID();

   SnappingToolBar(AudacityProject& project);
   virtual ~SnappingToolBar();

   bool ShownByDefault() const override;
   DockID DefaultDockID() const override;

   static SnappingToolBar& Get(AudacityProject& project);
   static const SnappingToolBar& Get(const AudacityProject& project);

   void Create(wxWindow *parent) override;

   void Populate() override;
   void Repaint(wxDC * WXUNUSED(dc)) override {};
   void EnableDisableButtons() override {};
   void UpdatePrefs() override;
   
   void RegenerateTooltips() override;

 private:
   void OnSize(wxSizeEvent &evt);
   void OnSnapModeChanged();

   wxWeakRef<wxCheckBox> mSnapModeCheckBox;
   wxWeakRef<wxComboCtrl> mSnapToCombo;
   
   Observer::Subscription mSnappingModeChangedSubscription;

 public:

   DECLARE_CLASS(SelectionBar)
   DECLARE_EVENT_TABLE()
};
