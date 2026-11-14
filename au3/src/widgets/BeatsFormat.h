/**********************************************************************

  Audacity: A Digital Audio Editor

  BeatsFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_BEATS_FORMAT__
#define __AUDACITY_BEATS_FORMAT__

#include "RulerFormat.h"

class ProjectTimeSignature;

class BeatsFormat final : public RulerFormat
{
public:
    struct Tick final
    {
        int upper { 1 };
        int lower { 1 };
        // Duration is in seconds
        double duration {};
    };

    struct Ticks final
    {
        Tick major;
        Tick minor;
        Tick minorMinor;
    };

    using RulerFormat::RulerFormat;
    BeatsFormat(const ProjectTimeSignature& timeSignature);
    BeatsFormat() = delete;

    ~BeatsFormat() override;

    void SetTickSizes(
        double units, double& major, double& minor, double& minorMinor, int& mDigits) const override;

    void SetLabelString(
        wxString& s, double d, double units, double minor, int mDigits, TickType tickType) const override;

    /*!
       @pre `bpm > 0`
       @pre `timeSigUpper > 0`
       @pre `timeSigLower > 1`
       @pre `(timeSigLower & (timeSigLower - 1)) == 0`
      */
    void SetData(double bpm, int timeSigUpper, int timeSigLower);

    void UpdateSubdivision(double units);

    const Ticks& GetSubdivision() const;

private:
    double mBpm{ 60.0 };
    int mTimeSigUpper{ 4 };
    int mTimeSigLower{ 4 };

    Ticks mTicks;
};

#endif
