/**********************************************************************

Audacity: A Digital Audio Editor

MeterPanelBase.cpp

Paul Licameli split from MeterPanel.cpp

**********************************************************************/

#include "MeterPanelBase.h"
#include "Meter.h"
#include <wx/weakref.h>

bool MeterPanelBase::s_AcceptsFocus{ false };

auto MeterPanelBase::TemporarilyAllowFocus() -> TempAllowFocus {
   s_AcceptsFocus = true;
   return TempAllowFocus{ &s_AcceptsFocus };
}

struct MeterPanelBase::Forwarder : Meter
{
   explicit Forwarder(wxEvtHandler *pMeter)
      : mpMeter{ pMeter } {}
   ~Forwarder() override {}

   void Clear() override
   {
      if (const auto pMeter = GetMeter())
         pMeter->Clear();
   }
   void Reset(double sampleRate, bool resetClipping) override
   {
      if (const auto pMeter = GetMeter())
         pMeter->Reset(sampleRate, resetClipping);
   }
   void Update(unsigned numChannels,
      unsigned long numFrames, const float *sampleData, bool interleaved)
   override
   {
      if (const auto pMeter = GetMeter())
         pMeter->Update(numChannels, numFrames, sampleData, interleaved);
   }
   bool IsDisabled() const override
   {
      if (const auto pMeter = GetMeter())
         return pMeter->IsDisabled();
      else
         return true;
   }

   Meter *GetMeter() const { return dynamic_cast<Meter*>(mpMeter.get()); }
   const wxWeakRef<wxEvtHandler> mpMeter;
};

MeterPanelBase::~MeterPanelBase() = default;

void MeterPanelBase::Init(wxEvtHandler *pMeter)
{
   mForwarder = std::make_shared<Forwarder>(this);
}

std::shared_ptr<Meter> MeterPanelBase::GetMeter() const
{
   return mForwarder;
}
