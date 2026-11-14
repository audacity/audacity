/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.cpp

  Michael Papadopoulos

**********************************************************************/

#include "BeatsFormat.h"
#include "ProjectTimeSignature.h"

#include <cassert>

void BeatsFormat::SetTickSizes(
    double units, double& major, double& minor, double& minorMinor,
    int& mDigits) const
{
    const_cast<BeatsFormat*>(this)->UpdateSubdivision(units);

    major = mTicks.major.duration;
    minor = mTicks.minor.duration;
    minorMinor = mTicks.minorMinor.duration;

    mDigits = 0;
}

void BeatsFormat::SetLabelString(
    wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const
{
    if (d < 0) {
        return;
    }

    const auto lower = static_cast<double>(mTimeSigLower);
    double val = (mBpm * (lower / 4) * d) / (60 * mTimeSigUpper);
    double beatApprox = (val - floor(val)) * mTimeSigUpper + 1;
    int beat = round(beatApprox);

    // Don't add decimal if it's a major tick or is on the beat
    // Segment by distance with units
    if (units < .4 * (60 / mBpm) * (4 / lower)) {
        if (tickType == RulerFormat::t_major) {
            s.Printf(wxT("%d"), (int)round(val + 1));
        } else if (tickType == RulerFormat::t_minor && abs(beat - beatApprox) < 1.0e-5f) {
            s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
        }
    } else if (units < .8 * (60 / mBpm) * (4 / lower)) {
        if (tickType == RulerFormat::t_major) {
            s.Printf(wxT("%d"), (int)round(val + 1));
        } else if (tickType == RulerFormat::t_minor && beat != 1) {
            s.Printf(wxT("%d.%d"), (int)floor(val + 1), (int)beat);
        }
    } else {
        if (tickType == RulerFormat::t_major) {
            s.Printf(wxT("%d"), (int)round(val + 1));
        }
    }
}

void BeatsFormat::SetData(double bpm, int timeSigUpper, int timeSigLower)
{
    // Check preconditions
    assert(bpm > 0);
    assert(timeSigUpper > 0);
    assert(timeSigLower > 1);
    assert((timeSigLower & (timeSigLower - 1)) == 0);

    if (!(mBpm > 0 && mTimeSigUpper > 1 && mTimeSigLower > 1)) {
        return;
    }

    if (mTimeSigLower & (mTimeSigLower - 1)) {
        return;
    }

    mBpm = bpm;
    mTimeSigUpper = timeSigUpper;
    mTimeSigLower = timeSigLower;
}

void BeatsFormat::UpdateSubdivision(double units)
{
    Ticks ticks;

    const auto lower = static_cast<double>(mTimeSigLower);
    if (units < .025 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // thirtysecondth notes (label every quarter note)
        ticks.minor = { 1, 32, 60 / (mBpm * (lower * 2)) };
        // hundredtwentyeighth notes
        ticks.minorMinor = { 1, 128, 60 / (mBpm * (lower * 8)) };
    } else if (units < .05 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // sixteenth notes (label every quarter note)
        ticks.minor = { 1, 16, 60 / (mBpm * (lower)) };
        // sixtyfourth notes
        ticks.minorMinor = { 1, 64, 60 / (mBpm * (lower * 4)) };
    } else if (units < .1 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // eigth notes (label every quarter note)
        ticks.minor = { 1, 8, 60 / (mBpm * (lower / 2)) };
        // thirtysecondth notes
        ticks.minorMinor = { 1, 32, 60 / (mBpm * (lower * 2)) };
    } else if (units < .4 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // eigth notes (label every quarter note)
        ticks.minor = { 1, 8, 60 / (mBpm * (lower / 2)) };
        // sixteenth notes
        ticks.minorMinor = { 1, 16, 60 / (mBpm * (lower)) };
    } else if (units < .8 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // quarter notes
        ticks.minor = { 1, 4, 60 / (mBpm * (lower / 4)) };
        // sixteenth notes
        ticks.minorMinor = { 1, 16, 60 / (mBpm * (lower)) };
    } else if (units < 4 * (60 / mBpm) * (4 / lower)) {
        // measures
        ticks.major = { mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 4)) };
        // quarter notes
        ticks.minorMinor = { 1, 4, 60 / (mBpm * (lower / 4)) };
    } else if (units < 8 * (60 / mBpm) * (4 / lower)) {
        // four-measures
        ticks.major = { 4 * mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / 16)) };
        // measures
        ticks.minor = { 4, mTimeSigLower, 60 / (mBpm * (lower / 16)) };
        // half measures
        ticks.minorMinor = { 2, mTimeSigLower, 60 / (mBpm * (lower / 8)) };
    } else {
        int factor = pow(
            2, std::floor(
                log2(std::ceil(units) * (mBpm / 60) * (mTimeSigLower / 4)))
            - 2);
        ticks.major = { factor* 4 * mTimeSigUpper, mTimeSigLower,
                        (60 * mTimeSigUpper) / (mBpm * (lower / (16 * factor))) };
        ticks.minorMinor = { factor* 2, mTimeSigLower,
                             60 / (mBpm * (lower / (8 * factor))) };
    }

    mTicks = ticks;
}

const BeatsFormat::Ticks& BeatsFormat::GetSubdivision() const
{
    return mTicks;
}

BeatsFormat::BeatsFormat(const ProjectTimeSignature& sig)
{
    SetData(
        sig.GetTempo(), sig.GetUpperTimeSignature(), sig.GetLowerTimeSignature());
}

BeatsFormat::~BeatsFormat() = default;
