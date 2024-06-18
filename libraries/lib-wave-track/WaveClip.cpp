/**********************************************************************

  Audacity: A Digital Audio Editor

  WaveClip.cpp

  ?? Dominic Mazzoni
  ?? Markus Meyer

*******************************************************************//**

\class WaveClip
\brief This allows multiple clips to be a part of one WaveTrack.

*//*******************************************************************/
#include "WaveClip.h"

#include <math.h>
#include <numeric>
#include <optional>
#include <vector>
#include <wx/log.h>

#include "BasicUI.h"
#include "Envelope.h"
#include "InconsistencyException.h"
#include "Resample.h"
#include "Sequence.h"
#include "TimeAndPitchInterface.h"
#include "UserException.h"

#ifdef _OPENMP
#include <omp.h>
#endif

WaveClipListener::~WaveClipListener()
{
}

WaveClip::WaveClip(size_t width,
   const SampleBlockFactoryPtr &factory,
   sampleFormat format, int rate, int colourIndex)
{
   assert(width > 0);
   mRate = rate;
   mColourIndex = colourIndex;
   mSequences.resize(width);
   for (auto &pSequence : mSequences)
      pSequence = std::make_unique<Sequence>(factory,
         SampleFormats{narrowestSampleFormat, format});

   mEnvelope = std::make_unique<Envelope>(true, 1e-7, 2.0, 1.0);
   assert(CheckInvariants());
}

WaveClip::WaveClip(
   const WaveClip& orig, const SampleBlockFactoryPtr& factory,
   bool copyCutlines)
    : mCentShift { orig.mCentShift }
    , mPitchAndSpeedPreset { orig.mPitchAndSpeedPreset }
    , mClipStretchRatio { orig.mClipStretchRatio }
    , mRawAudioTempo { orig.mRawAudioTempo }
    , mProjectTempo { orig.mProjectTempo }
{
   // essentially a copy constructor - but you must pass in the
   // current sample block factory, because we might be copying
   // from one project to another

   mSequenceOffset = orig.mSequenceOffset;
   mTrimLeft = orig.mTrimLeft;
   mTrimRight = orig.mTrimRight;
   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;
   mSequences.reserve(orig.GetWidth());
   for (auto &pSequence : orig.mSequences)
      mSequences.push_back(
         std::make_unique<Sequence>(*pSequence, factory));

   mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

   mName = orig.mName;

   if (copyCutlines)
      for (const auto &clip: orig.mCutLines)
         mCutLines.push_back(std::make_shared<WaveClip>(*clip, factory, true));

   mIsPlaceholder = orig.GetIsPlaceholder();

   assert(GetWidth() == orig.GetWidth());
   assert(CheckInvariants());
}

WaveClip::WaveClip(
   const WaveClip& orig, const SampleBlockFactoryPtr& factory,
   bool copyCutlines, double t0, double t1)
    : mCentShift { orig.mCentShift }
    , mClipStretchRatio { orig.mClipStretchRatio }
    , mRawAudioTempo { orig.mRawAudioTempo }
    , mProjectTempo { orig.mProjectTempo }
{
   assert(orig.CountSamples(t0, t1) > 0);

   mSequenceOffset = orig.mSequenceOffset;

   //Adjust trim values to sample-boundary
   if(t0 > orig.GetPlayStartTime()) {
      const auto s0 = orig.TimeToSamples(t0 - orig.GetSequenceStartTime());
      mTrimLeft = orig.SamplesToTime(s0);

   }
   else
      mTrimLeft = orig.mTrimLeft;

   if(t1 < orig.GetPlayEndTime())
   {
      const auto s1 = orig.TimeToSamples(orig.GetSequenceEndTime() - t1);
      mTrimRight = orig.SamplesToTime(s1);
   }
   else
      mTrimRight = orig.mTrimRight;

   mRate = orig.mRate;
   mColourIndex = orig.mColourIndex;

   mIsPlaceholder = orig.GetIsPlaceholder();

   mSequences.reserve(orig.GetWidth());
   for (auto &pSequence : orig.mSequences)
      mSequences.push_back(
         std::make_unique<Sequence>(*pSequence, factory));

   mEnvelope = std::make_unique<Envelope>(*orig.mEnvelope);

   if (copyCutlines)
      for (const auto &cutline : orig.mCutLines)
         mCutLines.push_back(
            std::make_shared<WaveClip>(*cutline, factory, true));

   assert(GetWidth() == orig.GetWidth());
   assert(CheckInvariants());
}


WaveClip::~WaveClip()
{
   Observer::Publisher<WaveClipDtorCalled>::Publish(WaveClipDtorCalled {});
}

AudioSegmentSampleView WaveClip::GetSampleView(
   size_t ii, sampleCount start, size_t length, bool mayThrow) const
{
   assert(ii < GetWidth());
   return mSequences[ii]->GetFloatSampleView(
      start + TimeToSamples(mTrimLeft), length, mayThrow);
}

AudioSegmentSampleView WaveClip::GetSampleView(
   size_t iChannel, double t0, double t1, bool mayThrow) const
{
   assert(iChannel < GetWidth());
   const auto start = TimeToSamples(std::max(0., t0));
   const auto length =
      (std::min(GetNumSamples(), TimeToSamples(t1)) - start).as_size_t();
   return GetSampleView(iChannel, start, length, mayThrow);
}

size_t WaveClip::GetWidth() const
{
   return mSequences.size();
}

bool WaveClip::GetSamples(size_t ii,
   samplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   assert(ii < GetWidth());
   return mSequences[ii]
      ->Get(buffer, format, start + TimeToSamples(mTrimLeft), len, mayThrow);
}

bool WaveClip::GetSamples(samplePtr buffers[], sampleFormat format,
   sampleCount start, size_t len, bool mayThrow) const
{
   bool result = true;
   for (size_t ii = 0, width = GetWidth(); result && ii < width; ++ii)
      result = GetSamples(ii, buffers[ii], format, start, len, mayThrow);
   return result;
}

/*! @excsafety{Strong} */
void WaveClip::SetSamples(size_t ii,
   constSamplePtr buffer, sampleFormat format,
   sampleCount start, size_t len, sampleFormat effectiveFormat)
{
   assert(ii < GetWidth());
   // use Strong-guarantee
   mSequences[ii]->SetSamples(buffer, format,
      start + TimeToSamples(mTrimLeft), len, effectiveFormat);

   // use No-fail-guarantee
   MarkChanged();
}

bool WaveClip::GetFloatAtTime(
   double t, size_t iChannel, float& value, bool mayThrow) const
{
   if (!WithinPlayRegion(t))
      return false;
   const auto start = TimeToSamples(t);
   return GetSamples(
      iChannel, reinterpret_cast<samplePtr>(&value), floatSample, start, 1u,
      mayThrow);
}

void WaveClip::SetFloatsFromTime(
   double t, size_t iChannel, const float* buffer, size_t numFloats,
   sampleFormat effectiveFormat)
{
   const auto maybeNegativeStart = TimeToSamples(t);
   const auto maybeOutOfBoundEnd = maybeNegativeStart + numFloats;
   const auto effectiveStart = std::max(sampleCount { 0 }, maybeNegativeStart);
   const auto effectiveEnd =
      std::min(GetVisibleSampleCount(), maybeOutOfBoundEnd);
   if (effectiveStart >= effectiveEnd)
      return;
   // Cannot be greater than `numFloats` -> safe cast
   const auto effectiveLen = (effectiveEnd - effectiveStart).as_size_t();
   // Cannot be greater than `numFloats` -> safe cast
   const auto numLeadingZeros =
      (effectiveStart - maybeNegativeStart).as_size_t();
   const auto offsetBuffer =
      reinterpret_cast<const char*>(buffer + numLeadingZeros);
   SetSamples(
      iChannel, offsetBuffer, floatSample, effectiveStart, effectiveLen,
      effectiveFormat);
}

void WaveClip::SetFloatsCenteredAroundTime(
   double t, size_t iChannel, const float* buffer, size_t numSideSamples,
   sampleFormat effectiveFormat)
{
   SetFloatsFromTime(
      t - SamplesToTime(numSideSamples), iChannel, buffer,
      2 * numSideSamples + 1, effectiveFormat);
}

void WaveClip::SetFloatAtTime(
   double t, size_t iChannel, float value, sampleFormat effectiveFormat)
{
   SetFloatsCenteredAroundTime(t, iChannel, &value, 0u, effectiveFormat);
}

void WaveClip::SetEnvelope(std::unique_ptr<Envelope> p)
{
   mEnvelope = move(p);
}

BlockArray* WaveClip::GetSequenceBlockArray(size_t ii)
{
   assert(ii < GetWidth());
   return &mSequences[ii]->GetBlockArray();
}

const BlockArray* WaveClip::GetSequenceBlockArray(size_t ii) const
{
   assert(ii < GetWidth());
   return &mSequences[ii]->GetBlockArray();
}

size_t WaveClip::GetAppendBufferLen() const
{
   // All append buffers have equal lengths by class invariant
   return mSequences[0]->GetAppendBufferLen();
}

void WaveClip::OnProjectTempoChange(
   const std::optional<double>& oldTempo, double newTempo)
{
   if (!mRawAudioTempo.has_value())
      // When we have tempo detection ready (either by header-file
      // read-up or signal analysis) we can use something smarter than that. In
      // the meantime, use the tempo of the project when the clip is created as
      // source tempo.
      mRawAudioTempo = oldTempo.value_or(newTempo);

   if (oldTempo.has_value())
   {
      const auto ratioChange = *oldTempo / newTempo;
      mSequenceOffset *= ratioChange;
      mTrimLeft *= ratioChange;
      mTrimRight *= ratioChange;
      StretchCutLines(ratioChange);
      mEnvelope->RescaleTimesBy(ratioChange);
   }
   mProjectTempo = newTempo;
   Observer::Publisher<StretchRatioChange>::Publish(
      StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchLeftTo(double to)
{
   const auto pet = GetPlayEndTime();
   if (to >= pet)
      return;
   const auto oldPlayDuration = pet - GetPlayStartTime();
   const auto newPlayDuration = pet - to;
   const auto ratioChange = newPlayDuration / oldPlayDuration;
   mSequenceOffset = pet - (pet - mSequenceOffset) * ratioChange;
   mTrimLeft *= ratioChange;
   mTrimRight *= ratioChange;
   mClipStretchRatio *= ratioChange;
   mEnvelope->SetOffset(mSequenceOffset);
   mEnvelope->RescaleTimesBy(ratioChange);
   StretchCutLines(ratioChange);
   Observer::Publisher<StretchRatioChange>::Publish(
      StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchRightTo(double to)
{
   const auto pst = GetPlayStartTime();
   if (to <= pst)
      return;
   const auto oldPlayDuration = GetPlayEndTime() - pst;
   const auto newPlayDuration = to - pst;
   const auto ratioChange = newPlayDuration / oldPlayDuration;
   StretchBy(ratioChange);
}

void WaveClip::StretchBy(double ratio)
{
   const auto pst = GetPlayStartTime();
   mSequenceOffset = pst - mTrimLeft * ratio;
   mTrimLeft *= ratio;
   mTrimRight *= ratio;
   mClipStretchRatio *= ratio;
   mEnvelope->SetOffset(mSequenceOffset);
   mEnvelope->RescaleTimesBy(ratio);
   StretchCutLines(ratio);
   Observer::Publisher<StretchRatioChange>::Publish(
      StretchRatioChange { GetStretchRatio() });
}

void WaveClip::StretchCutLines(double ratioChange)
{
   for (const auto& cutline : mCutLines)
   {
      cutline->mSequenceOffset *= ratioChange;
      cutline->mTrimLeft *= ratioChange;
      cutline->mTrimRight *= ratioChange;
      cutline->mClipStretchRatio *= ratioChange;
      cutline->mEnvelope->RescaleTimesBy(ratioChange);
   }
}

double WaveClip::GetStretchRatio() const
{
   const auto dstSrcRatio =
      mProjectTempo.has_value() && mRawAudioTempo.has_value() ?
         *mRawAudioTempo / *mProjectTempo :
         1.0;
   double stretchRatio = mClipStretchRatio * dstSrcRatio;

   auto projectTempo = mProjectTempo;
   auto rawAudioTempo = mRawAudioTempo;

   double lowestStretchRatio = 100 / (double)99999;

   if (stretchRatio < lowestStretchRatio) {
      stretchRatio = lowestStretchRatio;

      if (projectTempo.has_value()) {
         rawAudioTempo = projectTempo.value() * (lowestStretchRatio / mClipStretchRatio);
      }
      else if (rawAudioTempo.has_value()) {
         projectTempo = rawAudioTempo.value() / (lowestStretchRatio / mClipStretchRatio);
      }
   }

   if (mProjectTempo.has_value()) {
      const_cast<std::optional<double>&>(mRawAudioTempo) = rawAudioTempo;
   }
   if (mRawAudioTempo.has_value()) {
      const_cast<std::optional<double>&>(mProjectTempo) = projectTempo;
   }

   return stretchRatio;
}

int WaveClip::GetCentShift() const
{
   return mCentShift;
}

Observer::Subscription
WaveClip::SubscribeToCentShiftChange(std::function<void(int)> cb)
{
   return Observer::Publisher<CentShiftChange>::Subscribe(
      [cb](const CentShiftChange& cents) { cb(cents.newValue); });
}

Observer::Subscription WaveClip::SubscribeToPitchAndSpeedPresetChange(
   std::function<void(PitchAndSpeedPreset)> cb)
{
   return Observer::Publisher<PitchAndSpeedPresetChange>::Subscribe(
      [cb](const PitchAndSpeedPresetChange& formant) {
         cb(formant.newValue);
      });
}

bool WaveClip::HasEqualPitchAndSpeed(const WaveClip& other) const
{
   return StretchRatioEquals(other.GetStretchRatio()) &&
          GetCentShift() == other.GetCentShift();
}

bool WaveClip::HasPitchOrSpeed() const
{
   return !StretchRatioEquals(1.0) || GetCentShift() != 0;
}

bool WaveClip::StretchRatioEquals(double value) const
{
   return TimeAndPitchInterface::IsPassThroughMode(
      1 + GetStretchRatio() - value);
}

sampleCount WaveClip::GetNumSamples() const
{
   // All sequences have equal lengths by class invariant
   return mSequences[0]->GetNumSamples();
}

SampleFormats WaveClip::GetSampleFormats() const
{
   // All sequences have the same formats by class invariant
   return mSequences[0]->GetSampleFormats();
}

const SampleBlockFactoryPtr &WaveClip::GetFactory()
{
   // All sequences have the same factory by class invariant
   return mSequences[0]->GetFactory();
}

std::vector<std::unique_ptr<Sequence>> WaveClip::GetEmptySequenceCopies() const
{
   decltype(mSequences) newSequences;
   newSequences.reserve(mSequences.size());
   for (auto& pSequence : mSequences)
      newSequences.push_back(std::make_unique<Sequence>(
         pSequence->GetFactory(), pSequence->GetSampleFormats()));
   return newSequences;
}

constSamplePtr WaveClip::GetAppendBuffer(size_t ii) const
{
   assert(ii < GetWidth());
   return mSequences[ii]->GetAppendBuffer();
}

void WaveClip::MarkChanged() // NOFAIL-GUARANTEE
{
   Caches::ForEach( std::mem_fn( &WaveClipListener::MarkChanged ) );
}

std::pair<float, float> WaveClip::GetMinMax(size_t ii,
   double t0, double t1, bool mayThrow) const
{
   assert(ii < GetWidth());
   t0 = std::max(t0, GetPlayStartTime());
   t1 = std::min(t1, GetPlayEndTime());
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return {
         0.f,  // harmless, but unused since Sequence::GetMinMax does not use these values
         0.f   // harmless, but unused since Sequence::GetMinMax does not use these values
      };
   }

   if (t0 == t1)
      return{ 0.f, 0.f };

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequences[ii]->GetMinMax(s0, s1 - s0, mayThrow);
}

float WaveClip::GetRMS(size_t ii, double t0, double t1, bool mayThrow) const
{
   assert(ii < GetWidth());
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   return mSequences[ii]->GetRMS(s0, s1-s0, mayThrow);
}

void WaveClip::ConvertToSampleFormat(sampleFormat format,
   const std::function<void(size_t)> & progressReport)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get converted as needed when they are expanded.

   Transaction transaction{ *this };

   auto bChanged = mSequences[0]->ConvertToSampleFormat(format, progressReport);
   for (size_t ii = 1, width = GetWidth(); ii < width; ++ii) {
      bool alsoChanged =
         mSequences[ii]->ConvertToSampleFormat(format, progressReport);
      // Class invariant implies:
      assert(bChanged == alsoChanged);
   }
   if (bChanged)
      MarkChanged();

   transaction.Commit();
}

/*! @excsafety{No-fail} */
void WaveClip::UpdateEnvelopeTrackLen()
{
   // The envelope time points account for stretching.
   const auto len = GetNumSamples().as_double() * GetStretchRatio() / mRate;
   if (len != mEnvelope->GetTrackLen())
      mEnvelope->SetTrackLen(len, 1.0 / GetRate());
}

/*! @excsafety{Strong} */
std::shared_ptr<SampleBlock>
WaveClip::AppendNewBlock(constSamplePtr buffer, sampleFormat format, size_t len)
{
   // This is a special use function for legacy files only and this assertion
   // does not need to be relaxed
   assert(GetWidth() == 1);
   return mSequences[0]->AppendNewBlock( buffer, format, len );
}

/*! @excsafety{Strong} */
void WaveClip::AppendSharedBlock(const std::shared_ptr<SampleBlock> &pBlock)
{
   // This is a special use function for legacy files only and this assertion
   // does not need to be relaxed
   assert(GetWidth() == 1);
   mSequences[0]->AppendSharedBlock( pBlock );
}

bool WaveClip::Append(constSamplePtr buffers[], sampleFormat format,
   size_t len, unsigned int stride, sampleFormat effectiveFormat)
{
   Finally Do{ [this]{ assert(CheckInvariants()); } };

   // There is not a transaction to enforce consistency of lengths of sequences
   // (And there is as yet always just one sequence).

   //wxLogDebug(wxT("Append: len=%lli"), (long long) len);

   size_t ii = 0;
   bool appended = false;
   for (auto &pSequence : mSequences)
      appended =
         pSequence->Append(buffers[ii++], format, len, stride, effectiveFormat)
         || appended;

   // use No-fail-guarantee
   UpdateEnvelopeTrackLen();
   MarkChanged();

   return appended;
}

void WaveClip::Flush()
{
   //wxLogDebug(wxT("WaveClip::Flush"));
   //wxLogDebug(wxT("   mAppendBufferLen=%lli"), (long long) mAppendBufferLen);
   //wxLogDebug(wxT("   previous sample count %lli"), (long long) mSequence->GetNumSamples());

   if (GetAppendBufferLen() > 0) {

      Transaction transaction{ *this };

      for (auto &pSequence : mSequences)
         pSequence->Flush();

      transaction.Commit();

      // No-fail operations
      UpdateEnvelopeTrackLen();
      MarkChanged();
   }

   //wxLogDebug(wxT("now sample count %lli"), (long long) mSequence->GetNumSamples());
}

bool WaveClip::HandleXMLTag(const std::string_view& tag, const AttributesList &attrs)
{
   if (tag == "waveclip")
   {
      double dblValue;
      long longValue;
      for (auto pair : attrs)
      {
         auto attr = pair.first;
         auto value = pair.second;

         if (attr == "offset")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetSequenceStartTime(dblValue);
         }
         else if (attr == "trimLeft")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetTrimLeft(dblValue);
         }
         else if (attr == "trimRight")
         {
            if (!value.TryGet(dblValue))
               return false;
            SetTrimRight(dblValue);
         }
         else if (attr == "centShift")
         {
            if (!value.TryGet(dblValue))
               return false;
            mCentShift = dblValue;
         }
         else if (attr == "pitchAndSpeedPreset")
         {
            if (!value.TryGet(longValue))
               return false;
            mPitchAndSpeedPreset = static_cast<PitchAndSpeedPreset>(longValue);
         }
         else if (attr == "rawAudioTempo")
         {
            if (!value.TryGet(dblValue))
               return false;
            if (dblValue == 0)
               mRawAudioTempo.reset();
            else
               mRawAudioTempo = dblValue;
         }
         else if (attr == "clipStretchRatio")
         {
            if (!value.TryGet(dblValue))
               return false;
            mClipStretchRatio = dblValue;
         }
         else if (attr == "name")
         {
            if(value.IsStringView())
               SetName(value.ToWString());
         }
         else if (attr == "colorindex")
         {
            if (!value.TryGet(longValue))
               return false;
            SetColourIndex(longValue);
         }
      }
      return true;
   }

   return false;
}

void WaveClip::HandleXMLEndTag(const std::string_view& tag)
{
   // All blocks were deserialized into new sequences; remove the one made
   // by the constructor which remains empty.
   mSequences.erase(mSequences.begin());
   mSequences.shrink_to_fit();
   if (tag == "waveclip")
      UpdateEnvelopeTrackLen();
   // A proof of this assertion assumes that nothing has happened since
   // construction of this, besides calls to the other deserialization
   // functions
   assert(CheckInvariants());
}

XMLTagHandler *WaveClip::HandleXMLChild(const std::string_view& tag)
{
   auto &pFirst = mSequences[0];
   if (tag == "sequence") {
      mSequences.push_back(std::make_unique<Sequence>(
         pFirst->GetFactory(), pFirst->GetSampleFormats()));
      return mSequences.back().get();
   }
   else if (tag == "envelope")
      return mEnvelope.get();
   else if (tag == "waveclip")
   {
      // Nested wave clips are cut lines
      auto format = pFirst->GetSampleFormats().Stored();
      // The format is not stored in WaveClip itself but passed to
      // Sequence::Sequence; but then the Sequence will deserialize format
      // again
      mCutLines.push_back(
         std::make_shared<WaveClip>(
            // Make only one channel now, but recursive deserialization
            // increases the width later
            1, pFirst->GetFactory(),
            format, mRate, 0 /*colourindex*/));
      return mCutLines.back().get();
   }
   else
      return NULL;
}

void WaveClip::WriteXML(XMLWriter &xmlFile) const
// may throw
{
   if (GetSequenceSamplesCount() <= 0)
      // Oops, I'm empty? How did that happen? Anyway, I do nothing but causing
      // problems, don't save me.
      return;

   xmlFile.StartTag(wxT("waveclip"));
   xmlFile.WriteAttr(wxT("offset"), mSequenceOffset, 8);
   xmlFile.WriteAttr(wxT("trimLeft"), mTrimLeft, 8);
   xmlFile.WriteAttr(wxT("trimRight"), mTrimRight, 8);
   xmlFile.WriteAttr(wxT("centShift"), mCentShift);
   xmlFile.WriteAttr(
      wxT("pitchAndSpeedPreset"), static_cast<long>(mPitchAndSpeedPreset));
   xmlFile.WriteAttr(wxT("rawAudioTempo"), mRawAudioTempo.value_or(0.), 8);
   xmlFile.WriteAttr(wxT("clipStretchRatio"), mClipStretchRatio, 8);
   xmlFile.WriteAttr(wxT("name"), mName);
   xmlFile.WriteAttr(wxT("colorindex"), mColourIndex );

   for (auto &pSequence : mSequences)
      pSequence->WriteXML(xmlFile);
   mEnvelope->WriteXML(xmlFile);

   for (const auto &clip: mCutLines)
      clip->WriteXML(xmlFile);

   xmlFile.EndTag(wxT("waveclip"));
}

/*! @excsafety{Strong} */
bool WaveClip::Paste(double t0, const WaveClip& other)
{
   if (GetWidth() != other.GetWidth())
      return false;

   if (GetSequenceSamplesCount() == 0)
   {
      // Empty clip: we're flexible and adopt the other's stretching.
      mRawAudioTempo = other.mRawAudioTempo;
      mClipStretchRatio = other.mClipStretchRatio;
      mProjectTempo = other.mProjectTempo;
   }
   else if (GetStretchRatio() != other.GetStretchRatio())
      return false;

   Finally Do{ [this]{ assert(CheckInvariants()); } };

   Transaction transaction{ *this };

   const bool clipNeedsResampling = other.mRate != mRate;
   const bool clipNeedsNewFormat =
      other.GetSampleFormats().Stored() != GetSampleFormats().Stored();
   std::shared_ptr<WaveClip> newClip;

   t0 = std::clamp(t0, GetPlayStartTime(), GetPlayEndTime());

   //seems like edge cases cannot happen, see WaveTrack::PasteWaveTrack
   auto &factory = GetFactory();
   if (t0 == GetPlayStartTime())
   {
       ClearSequence(GetSequenceStartTime(), t0);
       SetTrimLeft(other.GetTrimLeft());

       auto copy = std::make_shared<WaveClip>(other, factory, true);
       copy->ClearSequence(copy->GetPlayEndTime(), copy->GetSequenceEndTime());
       newClip = std::move(copy);
   }
   else if (t0 == GetPlayEndTime())
   {
       ClearSequence(GetPlayEndTime(), GetSequenceEndTime());
       SetTrimRight(other.GetTrimRight());

       auto copy = std::make_shared<WaveClip>(other, factory, true);
       copy->ClearSequence(copy->GetSequenceStartTime(), copy->GetPlayStartTime());
       newClip = std::move(copy);
   }
   else
   {
      newClip = std::make_shared<WaveClip>(other, factory, true);
      newClip->ClearSequence(newClip->GetPlayEndTime(), newClip->GetSequenceEndTime());
      newClip->ClearSequence(newClip->GetSequenceStartTime(), newClip->GetPlayStartTime());
      newClip->SetTrimLeft(0);
      newClip->SetTrimRight(0);
   }

   if (clipNeedsResampling || clipNeedsNewFormat)
   {
      auto copy = std::make_shared<WaveClip>(*newClip.get(), factory, true);

      if (clipNeedsResampling)
         // The other clip's rate is different from ours, so resample
         copy->Resample(mRate);

      if (clipNeedsNewFormat)
         // Force sample formats to match.
         copy->ConvertToSampleFormat(GetSampleFormats().Stored());
      newClip = std::move(copy);
   }

   // Paste cut lines contained in pasted clip
   WaveClipHolders newCutlines;
   for (const auto &cutline: newClip->mCutLines)
   {
      auto cutlineCopy = std::make_shared<WaveClip>(*cutline, factory,
         // Recursively copy cutlines of cutlines.  They don't need
         // their offsets adjusted.
         true);
      cutlineCopy->ShiftBy(t0 - GetSequenceStartTime());
      newCutlines.push_back(std::move(cutlineCopy));
   }

   sampleCount s0 = TimeToSequenceSamples(t0);

   // Because newClip was made above as a copy of (a copy of) other
   assert(other.GetWidth() == newClip->GetWidth());
   // And other has the same width as this, so this loop is safe
   // Assume Strong-guarantee from Sequence::Paste
   for (size_t ii = 0, width = GetWidth(); ii < width; ++ii)
      mSequences[ii]->Paste(s0, newClip->mSequences[ii].get());

   transaction.Commit();

   // Assume No-fail-guarantee in the remaining
   MarkChanged();
   const auto sampleTime = 1.0 / GetRate();
   const auto timeOffsetInEnvelope =
      s0.as_double() * GetStretchRatio() / mRate + GetSequenceStartTime();
   mEnvelope->PasteEnvelope(
      timeOffsetInEnvelope, newClip->mEnvelope.get(), sampleTime);
   OffsetCutLines(t0, newClip->GetPlayEndTime() - newClip->GetPlayStartTime());

   for (auto &holder : newCutlines)
      mCutLines.push_back(std::move(holder));

   return true;
}

/*! @excsafety{Strong} */
void WaveClip::InsertSilence( double t, double len, double *pEnvelopeValue )
{
   Transaction transaction{ *this };

   if (t == GetPlayStartTime() && t > GetSequenceStartTime())
      ClearSequence(GetSequenceStartTime(), t);
   else if (t == GetPlayEndTime() && t < GetSequenceEndTime()) {
      ClearSequence(t, GetSequenceEndTime());
      SetTrimRight(.0);
   }

   const auto s0 = TimeToSequenceSamples(t);
   const auto slen = TimeToSamples(len);

   // use Strong-guarantee
   for (auto &pSequence : mSequences)
      pSequence->InsertSilence(s0, slen);

   transaction.Commit();

   // use No-fail-guarantee
   OffsetCutLines(t, len);

   const auto sampleTime = 1.0 / GetRate();
   auto pEnvelope = GetEnvelope();
   if ( pEnvelopeValue ) {

      // Preserve limit value at the end
      auto oldLen = pEnvelope->GetTrackLen();
      auto newLen = oldLen + len;
      pEnvelope->Cap( sampleTime );

      // Ramp across the silence to the given value
      pEnvelope->SetTrackLen( newLen, sampleTime );
      pEnvelope->InsertOrReplace
         ( pEnvelope->GetOffset() + newLen, *pEnvelopeValue );
   }
   else
      pEnvelope->InsertSpace( t, len );

   MarkChanged();
}

/*! @excsafety{Strong} */
void WaveClip::AppendSilence( double len, double envelopeValue )
{
   auto t = GetPlayEndTime();
   InsertSilence( t, len, &envelopeValue );
}

/*! @excsafety{Strong} */
void WaveClip::Clear(double t0, double t1)
{
    auto st0 = t0;
    auto st1 = t1;
    auto offset = .0;
    if (st0 <= GetPlayStartTime())
    {
        offset = (t0 - GetPlayStartTime()) + GetTrimLeft();
        st0 = GetSequenceStartTime();

        SetTrimLeft(.0);
    }
    if (st1 >= GetPlayEndTime())
    {
        st1 = GetSequenceEndTime();
        SetTrimRight(.0);
    }
    ClearSequence(st0, st1);

    if (offset != .0)
       ShiftBy(offset);
}

void WaveClip::ClearLeft(double t)
{
   if (t > GetPlayStartTime() && t < GetPlayEndTime())
   {
      ClearSequence(GetSequenceStartTime(), t);
      SetTrimLeft(.0);
      SetSequenceStartTime(t);
   }
}

void WaveClip::ClearRight(double t)
{
   if (t > GetPlayStartTime() && t < GetPlayEndTime())
   {
      ClearSequence(t, GetSequenceEndTime());
      SetTrimRight(.0);
   }
}

void WaveClip::ClearSequence(double t0, double t1)
{
   Transaction transaction{ *this };

    auto clip_t0 = std::max(t0, GetSequenceStartTime());
    auto clip_t1 = std::min(t1, GetSequenceEndTime());

    auto s0 = TimeToSequenceSamples(clip_t0);
    auto s1 = TimeToSequenceSamples(clip_t1);

    if (s0 != s1)
    {
        // use Strong-guarantee
        for (auto &pSequence : mSequences)
           pSequence->Delete(s0, s1 - s0);

        // use No-fail-guarantee in the remaining

        // msmeyer
        //
        // Delete all cutlines that are within the given area, if any.
        //
        // Note that when cutlines are active, two functions are used:
        // Clear() and ClearAndAddCutLine(). ClearAndAddCutLine() is called
        // whenever the user directly calls a command that removes some audio, e.g.
        // "Cut" or "Clear" from the menu. This command takes care about recursive
        // preserving of cutlines within clips. Clear() is called when internal
        // operations want to remove audio. In the latter case, it is the right
        // thing to just remove all cutlines within the area.
        //

        // May DELETE as we iterate, so don't use range-for
        for (auto it = mCutLines.begin(); it != mCutLines.end();)
        {
            WaveClip* clip = it->get();
            double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
            if (cutlinePosition >= t0 && cutlinePosition <= t1)
            {
                // This cutline is within the area, DELETE it
                it = mCutLines.erase(it);
            }
            else
            {
                if (cutlinePosition >= t1)
                {
                    clip->ShiftBy(clip_t0 - clip_t1);
                }
                ++it;
            }
        }

        // Collapse envelope
        auto sampleTime = 1.0 / GetRate();
        GetEnvelope()->CollapseRegion(t0, t1, sampleTime);
    }

    transaction.Commit();
    MarkChanged();
}

/*! @excsafety{Weak}
-- This WaveClip remains destructible in case of AudacityException.
But some cutlines may be deleted */
void WaveClip::ClearAndAddCutLine(double t0, double t1)
{
   if (t0 > GetPlayEndTime() || t1 < GetPlayStartTime() || CountSamples(t0, t1) == 0)
      return; // no samples to remove

   Transaction transaction{ *this };

   const double clip_t0 = std::max( t0, GetPlayStartTime() );
   const double clip_t1 = std::min( t1, GetPlayEndTime() );

   auto newClip = std::make_shared<WaveClip>(
      *this, GetFactory(), true, clip_t0, clip_t1);
   if(t1 < GetPlayEndTime())
   {
      newClip->ClearSequence(t1, newClip->GetSequenceEndTime());
      newClip->SetTrimRight(.0);
   }
   if(t0 > GetPlayStartTime())
   {
      newClip->ClearSequence(newClip->GetSequenceStartTime(), t0);
      newClip->SetTrimLeft(.0);
   }

   newClip->SetSequenceStartTime( clip_t0 - GetSequenceStartTime() );

   // Remove cutlines from this clip that were in the selection, shift
   // left those that were after the selection
   // May DELETE as we iterate, so don't use range-for
   for (auto it = mCutLines.begin(); it != mCutLines.end();)
   {
      WaveClip* clip = it->get();
      double cutlinePosition = GetSequenceStartTime() + clip->GetSequenceStartTime();
      if (cutlinePosition >= t0 && cutlinePosition <= t1)
         it = mCutLines.erase(it);
      else
      {
         if (cutlinePosition >= t1)
         {
            clip->ShiftBy(clip_t0 - clip_t1);
         }
         ++it;
      }
   }

   // Clear actual audio data
   auto s0 = TimeToSequenceSamples(t0);
   auto s1 = TimeToSequenceSamples(t1);

   // use Weak-guarantee
   for (auto &pSequence : mSequences)
      pSequence->Delete(s0, s1-s0);

   // Collapse envelope
   auto sampleTime = 1.0 / GetRate();
   GetEnvelope()->CollapseRegion( t0, t1, sampleTime );

   transaction.Commit();
   MarkChanged();

   mCutLines.push_back(std::move(newClip));

   // New cutline was copied from this so will have correct width
   assert(CheckInvariants());
}

bool WaveClip::FindCutLine(double cutLinePosition,
                           double* cutlineStart /* = NULL */,
                           double* cutlineEnd /* = NULL */) const
{
   for (const auto &cutline: mCutLines)
   {
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         auto startTime = GetSequenceStartTime() + cutline->GetSequenceStartTime();
         if (cutlineStart)
            *cutlineStart = startTime;
         if (cutlineEnd)
            *cutlineEnd = startTime + cutline->SamplesToTime(cutline->GetVisibleSampleCount());
         return true;
      }
   }

   return false;
}

/*! @excsafety{Strong} */
void WaveClip::ExpandCutLine(double cutLinePosition)
{
   auto end = mCutLines.end();
   auto it = std::find_if( mCutLines.begin(), end,
      [&](const WaveClipHolder &cutline) {
         return fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001;
      } );

   if ( it != end ) {
      auto *cutline = it->get();
      // assume Strong-guarantee from Paste

      // Envelope::Paste takes offset into account, WaveClip::Paste doesn't!
      // Do this to get the right result:
      cutline->mEnvelope->SetOffset(0);
      bool success = Paste(
         GetSequenceStartTime() + cutline->GetSequenceStartTime(), *cutline);
      assert(success); // class invariant promises cutlines have correct width

      // Now erase the cutline,
      // but be careful to find it again, because Paste above may
      // have modified the array of cutlines (if our cutline contained
      // another cutline!), invalidating the iterator we had.
      end = mCutLines.end();
      it = std::find_if(mCutLines.begin(), end,
         [=](const WaveClipHolder &p) { return p.get() == cutline; });
      if (it != end)
         mCutLines.erase(it); // deletes cutline!
      else {
         wxASSERT(false);
      }
   }
}

bool WaveClip::RemoveCutLine(double cutLinePosition)
{
   for (auto it = mCutLines.begin(); it != mCutLines.end(); ++it)
   {
      const auto &cutline = *it;
      //std::numeric_limits<double>::epsilon() or (1.0 / static_cast<double>(mRate))?
      if (fabs(GetSequenceStartTime() + cutline->GetSequenceStartTime() - cutLinePosition) < 0.0001)
      {
         mCutLines.erase(it); // deletes cutline!
         return true;
      }
   }

   return false;
}

/*! @excsafety{No-fail} */
void WaveClip::OffsetCutLines(double t0, double len)
{
   for (const auto &cutLine : mCutLines)
   {
      if (GetSequenceStartTime() + cutLine->GetSequenceStartTime() >= t0)
         cutLine->ShiftBy(len);
   }
}

void WaveClip::CloseLock() noexcept
{
   // Don't need a Transaction for noexcept operations
   for (auto &pSequence : mSequences)
      pSequence->CloseLock();
   for (const auto &cutline: mCutLines)
      cutline->CloseLock();
}

void WaveClip::SetRate(int rate)
{
   const auto trimLeftSampleNum = TimeToSamples(mTrimLeft);
   const auto trimRightSampleNum = TimeToSamples(mTrimRight);
   auto ratio = static_cast<double>(mRate) / rate;
   mRate = rate;
   mTrimLeft = SamplesToTime(trimLeftSampleNum);
   mTrimRight = SamplesToTime(trimRightSampleNum);
   const auto newLength =
      GetNumSamples().as_double() * GetStretchRatio() / mRate;
   mEnvelope->RescaleTimes(newLength);
   MarkChanged();
   SetSequenceStartTime(GetSequenceStartTime() * ratio);
}

void WaveClip::SetRawAudioTempo(double tempo)
{
   mRawAudioTempo = tempo;
}

bool WaveClip::SetCentShift(int cents)
{
   if (
      cents < TimeAndPitchInterface::MinCents ||
      cents > TimeAndPitchInterface::MaxCents)
      return false;
   mCentShift = cents;
   Observer::Publisher<CentShiftChange>::Publish(CentShiftChange { cents });
   return true;
}

void WaveClip::SetPitchAndSpeedPreset(PitchAndSpeedPreset preset)
{
   mPitchAndSpeedPreset = preset;
   Observer::Publisher<PitchAndSpeedPresetChange>::Publish(
      PitchAndSpeedPresetChange { mPitchAndSpeedPreset });
}

PitchAndSpeedPreset WaveClip::GetPitchAndSpeedPreset() const
{
   return mPitchAndSpeedPreset;
}

/*! @excsafety{Strong} */
void WaveClip::Resample(int rate, BasicUI::ProgressDialog *progress)
{
   // Note:  it is not necessary to do this recursively to cutlines.
   // They get resampled as needed when they are expanded.

   if (rate == mRate)
      return; // Nothing to do

   // This function does its own RAII without a Transaction

   double factor = (double)rate / (double)mRate;
   ::Resample resample(true, factor, factor); // constant rate resampling

   const size_t bufsize = 65536;
   Floats inBuffer{ bufsize };
   Floats outBuffer{ bufsize };
   sampleCount pos = 0;
   bool error = false;
   int outGenerated = 0;
   const auto numSamples = GetNumSamples();

   // These sequences are appended to below
   auto newSequences = GetEmptySequenceCopies();

   /**
    * We want to keep going as long as we have something to feed the resampler
    * with OR as long as the resampler spews out samples (which could continue
    * for a few iterations after we stop feeding it)
    */
   while (pos < numSamples || outGenerated > 0) {
      const auto inLen = limitSampleBufferSize( bufsize, numSamples - pos );

      bool isLast = ((pos + inLen) == numSamples);

      auto ppNewSequence = newSequences.begin();
      std::optional<std::pair<size_t, size_t>> results{};
      for (auto &pSequence : mSequences) {
         auto &pNewSequence = *ppNewSequence++;
         if (!pSequence->Get((samplePtr)inBuffer.get(), floatSample, pos, inLen, true))
         {
            error = true;
            break;
         }

         // Expect the same results for all channels, or else fail
         auto newResults = resample.Process(factor, inBuffer.get(), inLen,
            isLast, outBuffer.get(), bufsize);
         if (!results)
            results.emplace(newResults);
         else if (*results != newResults) {
            error = true;
            break;
         }

         outGenerated = results->second;
         if (outGenerated < 0) {
            error = true;
            break;
         }

         pNewSequence->Append((samplePtr)outBuffer.get(), floatSample,
            outGenerated, 1,
            widestSampleFormat /* computed samples need dither */
         );
      }
      if (results)
         pos += results->first;

      if (progress)
      {
         auto updateResult = progress->Poll(
            pos.as_long_long(),
            numSamples.as_long_long()
         );
         error = (updateResult != BasicUI::ProgressResult::Success);
         if (error)
            throw UserException{};
      }
   }

   if (error)
      throw SimpleMessageBoxException{
         ExceptionType::Internal,
         XO("Resampling failed."),
         XO("Warning"),
         "Error:_Resampling"
      };
   else
   {
      // Use No-fail-guarantee in these steps
      mSequences = move(newSequences);
      mRate = rate;
      Flush();
      Caches::ForEach( std::mem_fn( &WaveClipListener::Invalidate ) );
   }
}

// Used by commands which interact with clips using the keyboard.
// When two clips are immediately next to each other, the GetPlayEndTime()
// of the first clip and the GetPlayStartTime() of the second clip may not
// be exactly equal due to rounding errors.
bool WaveClip::SharesBoundaryWithNextClip(const WaveClip* next) const
{
   double endThis = GetRate() * GetPlayStartTime() +
                    GetVisibleSampleCount().as_double() * GetStretchRatio();
   double startNext = next->GetRate() * next->GetPlayStartTime();

   // given that a double has about 15 significant digits, using a criterion
   // of half a sample should be safe in all normal usage.
   return fabs(startNext - endThis) < 0.5;
}

void WaveClip::SetName(const wxString& name)
{
   mName = name;
}

const wxString& WaveClip::GetName() const
{
   return mName;
}

sampleCount WaveClip::TimeToSamples(double time) const
{
   return sampleCount(floor(time * mRate / GetStretchRatio() + 0.5));
}

double WaveClip::SamplesToTime(sampleCount s) const noexcept
{
   return s.as_double() * GetStretchRatio() / mRate;
}

double WaveClip::SnapToTrackSample(double t) const noexcept
{
   return std::round(t * mRate) / mRate;
}

void WaveClip::SetSilence(sampleCount offset, sampleCount length)
{
   const auto start = TimeToSamples(mTrimLeft) + offset;
   Transaction transaction{ *this };
   for (auto &pSequence : mSequences)
      pSequence->SetSilence(start, length);
   transaction.Commit();
   MarkChanged();
}

sampleCount WaveClip::GetSequenceSamplesCount() const
{
    return GetNumSamples() * GetWidth();
}

double WaveClip::GetPlayStartTime() const noexcept
{
   return SnapToTrackSample(mSequenceOffset + mTrimLeft);
}

void WaveClip::SetPlayStartTime(double time)
{
   SetSequenceStartTime(time - mTrimLeft);
}

double WaveClip::GetPlayEndTime() const
{
    const auto numSamples = GetNumSamples();
    double maxLen = mSequenceOffset +
                    ((numSamples + GetAppendBufferLen()).as_double()) *
                       GetStretchRatio() / mRate -
                    mTrimRight;
    // JS: calculated value is not the length;
    // it is a maximum value and can be negative; no clipping to 0
    return SnapToTrackSample(maxLen);
}

double WaveClip::GetPlayDuration() const
{
   return GetPlayEndTime() - GetPlayStartTime();
}

bool WaveClip::IsEmpty() const
{
   return std::floor(GetPlayDuration() * mRate + 0.5) < 2.0;
}

sampleCount WaveClip::GetPlayStartSample() const
{
   return sampleCount { GetPlayStartTime() * mRate + 0.5 };
}

sampleCount WaveClip::GetPlayEndSample() const
{
   return sampleCount { GetPlayEndTime() * mRate + 0.5 };
}

sampleCount WaveClip::GetVisibleSampleCount() const
{
    return GetNumSamples()
       - TimeToSamples(mTrimRight) - TimeToSamples(mTrimLeft);
}

void WaveClip::SetTrimLeft(double trim)
{
    mTrimLeft = std::max(.0, trim);
}

double WaveClip::GetTrimLeft() const noexcept
{
    return mTrimLeft;
}

void WaveClip::SetTrimRight(double trim)
{
    mTrimRight = std::max(.0, trim);
}

double WaveClip::GetTrimRight() const noexcept
{
    return mTrimRight;
}

void WaveClip::TrimLeft(double deltaTime)
{
   SetTrimLeft(mTrimLeft + deltaTime);
}

void WaveClip::TrimRight(double deltaTime)
{
   SetTrimRight(mTrimRight + deltaTime);
}

void WaveClip::TrimQuarternotesFromRight(double quarters)
{
   assert(mRawAudioTempo.has_value());
   if (!mRawAudioTempo.has_value())
      return;
   const auto secondsPerQuarter = 60 * GetStretchRatio() / *mRawAudioTempo;
   // MH https://github.com/audacity/audacity/issues/5878: Clip boundaries are
   // quantized to the sample period. Music durations aren't, though.
   // `quarters` was probably chosen such that the clip ends exactly at some
   // musical grid snapping point. However, if we right-trim by `quarters`,
   // the clip's play end time might be rounded up to the next sample period,
   // overlapping the next snapping point on the musical grid. We don't want
   // this, or it would disturb music producers who want to horizontally
   // duplicate loops.
   const auto quantizedTrim =
      std::ceil(quarters * secondsPerQuarter * GetRate()) / GetRate();
   TrimRight(quantizedTrim);
}

void WaveClip::TrimLeftTo(double to)
{
   mTrimLeft =
      std::clamp(to, SnapToTrackSample(mSequenceOffset), GetPlayEndTime()) -
      mSequenceOffset;
}

void WaveClip::TrimRightTo(double to)
{
   const auto endTime = SnapToTrackSample(GetSequenceEndTime());
   mTrimRight = endTime - std::clamp(to, GetPlayStartTime(), endTime);
}

double WaveClip::GetSequenceStartTime() const noexcept
{
    // JS: mSequenceOffset is the minimum value and it is returned; no clipping to 0
    // Do we need to `SnapToTrackSample` before returning?
    return mSequenceOffset;
}

void WaveClip::SetSequenceStartTime(double startTime)
{
    mSequenceOffset = startTime;
    mEnvelope->SetOffset(startTime);
}

double WaveClip::GetSequenceEndTime() const
{
   const auto numSamples = GetNumSamples();
   double maxLen = GetSequenceStartTime() +
                   numSamples.as_double() * GetStretchRatio() / mRate;
   return maxLen;
}

sampleCount WaveClip::GetSequenceStartSample() const
{
    return TimeToSamples(mSequenceOffset);
}

void WaveClip::ShiftBy(double delta) noexcept
{
    SetSequenceStartTime(GetSequenceStartTime() + delta);
}

bool WaveClip::SplitsPlayRegion(double t) const
{
   return GetPlayStartTime() < t && t < GetPlayEndTime();
}

bool WaveClip::WithinPlayRegion(double t) const
{
   return GetPlayStartTime() <= t && t < GetPlayEndTime();
}

bool WaveClip::EntirelyWithinPlayRegion(double t0, double t1) const
{
   assert(t0 <= t1);
   // t1 is the open end of the interval, hence it's ok if it's equal to the
   // open end of the play region.
   return !BeforePlayRegion(t0) && t1 <= GetPlayEndTime();
}

bool WaveClip::PartlyWithinPlayRegion(double t0, double t1) const
{
   assert(t0 <= t1);
   return WithinPlayRegion(t0) != WithinPlayRegion(t1);
}

bool WaveClip::IntersectsPlayRegion(double t0, double t1) const
{
   assert(t0 <= t1);
   // t1 is the open end of the interval, so it must be excluded from the closed
   // begin of the play region.
   return t0 < GetPlayEndTime() && GetPlayStartTime() < t1;
}

bool WaveClip::CoversEntirePlayRegion(double t0, double t1) const
{
   assert(t0 <= t1);
   return t0 <= GetPlayStartTime() && GetPlayEndTime() <= t1;
}

bool WaveClip::BeforePlayRegion(double t) const
{
   return t < GetPlayStartTime();
}

bool WaveClip::AtOrBeforePlayRegion(double t) const
{
   return t <= GetPlayStartTime();
}

bool WaveClip::AfterPlayRegion(double t) const
{
   return GetPlayEndTime() <= t;
}

sampleCount WaveClip::CountSamples(double t0, double t1) const
{
   if(t0 < t1)
   {
      t0 = std::max(t0, GetPlayStartTime());
      t1 = std::min(t1, GetPlayEndTime());
      const auto s0 = TimeToSamples(t0 - GetPlayStartTime());
      const auto s1 = TimeToSamples(t1 - GetPlayStartTime());
      return s1 - s0;
   }
   return { 0 };
}

sampleCount WaveClip::TimeToSequenceSamples(double t) const
{
    if (t < GetSequenceStartTime())
        return 0;
    else if (t > GetSequenceEndTime())
        return GetNumSamples();
    return TimeToSamples(t - GetSequenceStartTime());
}

bool WaveClip::CheckInvariants() const
{
   const auto width = GetWidth();
   auto iter = mSequences.begin(),
      end = mSequences.end();
   // There must be at least one pointer
   if (iter != end) {
      // All pointers mut be non-null
      auto &pFirst = *iter++;
      if (pFirst) {
         // All sequences must have the sample formats, and sample block factory
         return
         std::all_of(iter, end, [&](decltype(pFirst) pSequence) {
            return pSequence &&
               pSequence->GetSampleFormats() == pFirst->GetSampleFormats() &&
               pSequence->GetFactory() == pFirst->GetFactory();
         }) &&
         // All cut lines are non-null, satisfy the invariants, and match width
         std::all_of(mCutLines.begin(), mCutLines.end(),
         [width](const WaveClipHolder &pCutLine) {
            return pCutLine && pCutLine->GetWidth() == width &&
               pCutLine->CheckInvariants();
         });
      }
   }
   return false;
}

WaveClip::Transaction::Transaction(WaveClip &clip)
   : clip{ clip }
   , mTrimLeft{ clip.mTrimLeft }
   , mTrimRight{ clip.mTrimRight }
{
   sequences.reserve(clip.mSequences.size());
   auto &factory = clip.GetFactory();
   for (auto &pSequence : clip.mSequences)
      sequences.push_back(
         //! Does not copy un-flushed append buffer data
         std::make_unique<Sequence>(*pSequence, factory));
}

WaveClip::Transaction::~Transaction()
{
   if (!committed) {
      clip.mSequences.swap(sequences);
      clip.mTrimLeft = mTrimLeft;
      clip.mTrimRight = mTrimRight;
   }
}
