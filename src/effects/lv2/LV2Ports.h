/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Ports.h
  @brief Immutable descriptions of LV2 ports; associated state structures

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_PORTS__
#define __AUDACITY_LV2_PORTS__

#if USE_LV2

#include "lilv/lilv.h"
#include "LV2Utils.h"
#include "MemoryX.h"
#include "TranslatableString.h"
#include <wx/arrstr.h>
#include "zix/ring.h"

using Floats = ArrayOf<float>;

//! Immutable description of an LV2 port
class LV2Port {
public:
   LV2Port(const LilvPort *port, int index, bool isInput,
      const wxString &symbol, const wxString &name,
      const TranslatableString &group
   )  : mPort(port), mIndex(index), mIsInput(isInput)
      , mSymbol(symbol), mName(name), mGroup(group)
   {}
   const LilvPort *const mPort;
   const uint32_t mIndex;
   const bool mIsInput;
   const wxString mSymbol;
   const wxString mName;
   const TranslatableString mGroup;
};

//! Immutable description of an LV2 Audio port
class LV2AudioPort final : public LV2Port {
public:
   using LV2Port::LV2Port;
};
using LV2AudioPortPtr = std::shared_ptr<LV2AudioPort>;
using LV2AudioPortArray = std::vector<LV2AudioPortPtr>;

//! Immutable description of an LV2 Atom port
class LV2AtomPort final : public LV2Port {
public:
   LV2AtomPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      uint32_t minimumSize, bool isMidi, bool wantsPosition
   )  : LV2Port{ port, index, isInput, symbol, name, group }
      , mMinimumSize{ minimumSize }
      , mIsMidi{ isMidi }
      , mWantsPosition{ wantsPosition }
   {}
   const uint32_t mMinimumSize;
   const bool mIsMidi;
   const bool mWantsPosition;
};
using LV2AtomPortPtr = std::shared_ptr<LV2AtomPort>;
using LV2AtomPortArray = std::vector<LV2AtomPortPtr>;

//! State of an instance of an LV2 Atom port
struct LV2AtomPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2AtomPortState(LV2AtomPortPtr pPort)
      : mpPort{ move(pPort) }
      , mRing{ zix_ring_new(mpPort->mMinimumSize) }
      , mBuffer( mpPort->mMinimumSize )
   {
      assert(mpPort);
      zix_ring_mlock(mRing.get());
   }
   const LV2AtomPortPtr mpPort;
   const Lilv_ptr<ZixRing, zix_ring_free> mRing;
   std::vector<uint8_t> mBuffer;
};
using LV2AtomPortStatePtr = std::shared_ptr<LV2AtomPortState>;
using LV2AtomPortStateArray = std::vector<LV2AtomPortStatePtr>;

//! Immutable description of an LV2 CV port (control data signal at sample rate)
class LV2CVPort final : public LV2Port {
public:
   LV2CVPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      float min, float max, float def, bool hasLo, bool hasHi
   )  : LV2Port(port, index, isInput, symbol, name, group)
      , mMin{ min }, mMax{ max }, mDef{ def }, mHasLo{ hasLo }, mHasHi{ hasHi }
   {}
   const float mMin;
   const float mMax;
   const float mDef;
   const bool mHasLo;
   const bool mHasHi;
};
using LV2CVPortPtr = std::shared_ptr<LV2CVPort>;
using LV2CVPortArray = std::vector<LV2CVPortPtr>;

//! State of an instance of an LV2 CV port
struct LV2CVPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2CVPortState(LV2CVPortPtr pPort) : mpPort{ move(pPort) } {
      assert(mpPort);
   }
   const LV2CVPortPtr mpPort;
   Floats mBuffer;
};
//! No need yet for extra indirection
using LV2CVPortStateArray = std::vector<LV2CVPortState>;

//! Immutable description of an LV2 control port
class LV2ControlPort final : public LV2Port
{
public:
   LV2ControlPort(const LilvPort *port, int index, bool isInput,
      const wxString & symbol, const wxString & name,
      const TranslatableString & group,
      std::vector<double> scaleValues, wxArrayString scaleLabels,
      const wxString &units,
      float min, float max, float def, bool hasLo, bool hasHi,
      bool toggle, bool enumeration, bool integer, bool sampleRate,
      bool trigger, bool logarithmic
   )  : LV2Port{ port, index, isInput, symbol, name, group }
      , mScaleValues{ move(scaleValues) }
      , mScaleLabels{ std::move(scaleLabels) }
      , mUnits{ units }
      , mMin{ min }, mMax{ max }, mDef{ def }
      , mHasLo{ hasLo }, mHasHi{ hasHi }
      , mToggle{ toggle }, mEnumeration{ enumeration }, mInteger{ integer }
      , mSampleRate{ sampleRate }
      , mTrigger{ trigger }, mLogarithmic{ logarithmic }
   {}
 
   // ScalePoints
   const std::vector<double> mScaleValues;
   const wxArrayString mScaleLabels;

   const wxString mUnits;
   const float mMin;
   const float mMax;
   const float mDef;
   const bool mHasLo;
   const bool mHasHi;
   const bool mToggle;
   const bool mEnumeration;
   const bool mInteger;
   const bool mSampleRate;
   const bool mTrigger;
   const bool mLogarithmic;

   //! Map a real number to one of the scale points
   size_t Discretize(float value) const;
};
using LV2ControlPortPtr = std::shared_ptr<LV2ControlPort>;
using LV2ControlPortArray = std::vector<LV2ControlPortPtr>;

//! State of an instance of an LV2 Control port
struct LV2ControlPortState final {
   //! @pre `pPort != nullptr`
   explicit LV2ControlPortState(LV2ControlPortPtr pPort)
      : mpPort{ move(pPort) }
   {
      assert(mpPort);
   }
   const LV2ControlPortPtr mpPort;
   //! Value of mTmp last seen by idle-time updater
   float mLst{ 0.0 };
   //! Value of UI control, as scaled by sample rate if that is required
   float mTmp{ 0.0 };
   //! Lower bound, as scaled by sample rate if that is required
   float mLo{ 0.0 };
   //! Upper bound, as scaled by sample rate if that is required
   float mHi{ 0.0 };
};
//! No need yet for extra indirection
using LV2ControlPortStateArray = std::vector<LV2ControlPortState>;

#endif
#endif
