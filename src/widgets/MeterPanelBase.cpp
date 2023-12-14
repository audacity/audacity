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
   void Update(unsigned numChannels,
      unsigned long numFrames, const float *sampleData, bool interleaved)
   override
   {
      if (mOwner)
         mOwner->Update(numChannels, numFrames, sampleData, interleaved);
   }
   bool IsDisabled() const override
   {
      if (mOwner)
         return mOwner->IsDisabled();
      else
         return true;
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
