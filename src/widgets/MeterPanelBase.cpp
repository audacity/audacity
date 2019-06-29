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
   explicit Forwarder( MeterPanelBase *pOwner )
      : mOwner{ pOwner } {}
   ~Forwarder() override {}

   void Clear() override
   {
      if (mOwner)
         mOwner->Clear();
   }
   void Reset(double sampleRate, bool resetClipping) override
   {
      if (mOwner)
         mOwner->Reset( sampleRate, resetClipping );
   }
   void UpdateDisplay(unsigned numChannels,
                      int numFrames, float *sampleData) override
   {
      if (mOwner)
         mOwner->UpdateDisplay( numChannels, numFrames, sampleData );
   }
   bool IsMeterDisabled() const override
   {
      if (mOwner)
         return mOwner->IsMeterDisabled();
      else
         return true;
   }
   float GetMaxPeak() const override
   {
      if (mOwner)
         return mOwner->GetMaxPeak();
      else
         return 0.0;
   }

   bool IsClipping() const override
   {
      if (mOwner)
         return mOwner->IsClipping();
      else
         return false;
   }

   int GetDBRange() const override
   {
      if (mOwner)
         return mOwner->GetDBRange();
      else
         return 0.0;
   }

   const wxWeakRef< MeterPanelBase > mOwner;
};

MeterPanelBase::~MeterPanelBase() = default;

void MeterPanelBase::Init()
{
   mForwarder = std::make_shared< Forwarder >( this );
}

std::shared_ptr<Meter> MeterPanelBase::GetMeter() const
{
   return mForwarder;
}
