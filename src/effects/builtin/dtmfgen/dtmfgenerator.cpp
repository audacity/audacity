#include "dtmfgenerator.h"
#include <cmath>

using namespace au::effects;

// used for fadein/out needed to remove clicking noise
static const double kFadeInOut = 250.0;

const ComponentInterfaceSymbol DtmfGenerator::Symbol { XO("DTMF Tones") };

DtmfGenerator::DtmfGenerator()
    : GeneratorEffect(mT0, mT1)
{
}

DtmfGenerator::~DtmfGenerator()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol DtmfGenerator::GetSymbol() const
{
    return Symbol;
}

TranslatableString DtmfGenerator::GetDescription() const
{
    return XO("Generates dual-tone multi-frequency (DTMF) tones like those produced by the keypad on telephones");
}

ManualPageID DtmfGenerator::ManualPage() const
{
    return L"DTMF_Tones";
}

// EffectDefinitionInterface implementation

EffectType DtmfGenerator::GetType() const
{
    return EffectTypeGenerate;
}

bool DtmfGenerator::Instance::ProcessInitialize(
    EffectSettings& settings, double sampleRate, ChannelNames)
{
    mSampleRate = sampleRate;

    auto& dtmfSettings = GetSettings(settings);
    if (dtmfSettings.dtmfNTones == 0) { // Bail if no DTFM sequence.
        return false;
    }

    double duration = settings.extra.GetDuration();

    // all dtmf sequence durations in samples from seconds
    // MJS: Note that mDuration is in seconds but will have been quantised to the
    // units of the TTC. If this was 'samples' and the project rate was lower
    // than the track rate, extra samples may get created as mDuration may now be
    // > mT1 - mT0; However we are making our best efforts at creating what was
    // asked for.

    auto nT0 = (sampleCount)floor(mT0 * mSampleRate + 0.5);
    auto nT1 = (sampleCount)floor((mT0 + duration) * mSampleRate + 0.5);
    numSamplesSequence
        =nT1 - nT0; // needs to be exact number of samples selected

    // make under-estimates if anything, and then redistribute the few remaining
    // samples
    numSamplesTone = sampleCount(floor(dtmfSettings.dtmfTone * mSampleRate));
    numSamplesSilence
        =sampleCount(floor(dtmfSettings.dtmfSilence * mSampleRate));

    // recalculate the sum, and spread the difference - due to approximations.
    // Since diff should be in the order of "some" samples, a division (resulting
    // in zero) is not sufficient, so we add the additional remaining samples in
    // each tone/silence block, at least until available.
    diff = numSamplesSequence - (dtmfSettings.dtmfNTones * numSamplesTone)
           - (dtmfSettings.dtmfNTones - 1) * numSamplesSilence;
    while (diff > 2 * dtmfSettings.dtmfNTones - 1)
    { // more than one per thingToBeGenerated
      // in this case, both numSamplesTone and numSamplesSilence would change,
      // so it makes sense
      //  to recalculate diff here, otherwise just keep the value we already
      //  have

        // should always be the case that dtmfNTones>1, as if 0, we don't even
        // start processing, and with 1 there is no difference to spread (no
        // silence slot)...
        wxASSERT(dtmfSettings.dtmfNTones > 1);
        numSamplesTone += (diff / (dtmfSettings.dtmfNTones));
        numSamplesSilence += (diff / (dtmfSettings.dtmfNTones - 1));
        diff = numSamplesSequence - (dtmfSettings.dtmfNTones * numSamplesTone)
               - (dtmfSettings.dtmfNTones - 1) * numSamplesSilence;
    }
    wxASSERT(diff >= 0); // should never be negative

    curSeqPos = -1; // pointer to string in dtmfSequence
    isTone = false;
    numRemaining = 0;

    return true;
}

size_t DtmfGenerator::Instance::ProcessBlock(
    EffectSettings& settings, const float* const*, float* const* outbuf,
    size_t size)
{
    auto& dtmfSettings = GetSettings(settings);
    float* buffer = outbuf[0];
    decltype(size) processed = 0;

    // for the whole dtmf sequence, we will be generating either tone or silence
    // according to a bool value, and this might be done in small chunks of size
    // 'block', as a single tone might sometimes be larger than the block
    // tone and silence generally have different duration, thus two generation
    // blocks
    //
    // Note: to overcome a 'clicking' noise introduced by the abrupt transition
    // from/to silence, I added a fade in/out of 1/250th of a second (4ms). This
    // can still be tweaked but gives excellent results at 44.1kHz: I haven't
    // tried other freqs. A problem might be if the tone duration is very short
    // (<10ms)... (?)
    //
    // One more problem is to deal with the approximations done when calculating
    // the duration of both tone and silence: in some cases the final sum might
    // not be same as the initial duration. So, to overcome this, we had a
    // redistribution block up, and now we will spread the remaining samples in
    // every bin in order to achieve the full duration: test case was to generate
    // an 11 tone DTMF sequence, in 4 seconds, and with DutyCycle=75%: after
    // generation you ended up with 3.999s or in other units: 3 seconds and 44097
    // samples.
    //
    while (size)
    {
        if (numRemaining == 0) {
            isTone = !isTone;

            if (isTone) {
                curSeqPos++;
                numRemaining = numSamplesTone;
                curTonePos = 0;
            } else {
                numRemaining = numSamplesSilence;
            }

            // the statement takes care of extracting one sample from the diff bin
            // and adding it into the current block until depletion
            numRemaining += (diff-- > 0 ? 1 : 0);
        }

        const auto len = limitSampleBufferSize(size, numRemaining);

        if (isTone) {
            // generate the tone and append
            assert(curSeqPos < dtmfSettings.dtmfNTones);
            MakeDtmfTone(
                buffer, len, mSampleRate, dtmfSettings.dtmfSequence[curSeqPos],
                curTonePos, numSamplesTone, dtmfSettings.dtmfAmplitude);
            curTonePos += len;
        } else {
            memset(buffer, 0, sizeof(float) * len);
        }

        numRemaining -= len;

        buffer += len;
        size -= len;
        processed += len;
    }

    return processed;
}

// EffectDtmf implementation

// Updates dtmfNTones, dtmfTone, dtmfSilence, and sometimes duration
// They depend on dtmfSequence, dtmfDutyCycle, and duration
void DtmfSettings::Recalculate(EffectSettings& settings)
{
    auto& extra = settings.extra;
    // remember that dtmfDutyCycle is in range (0.0-100.0)

    dtmfNTones = dtmfSequence.length();

    if (dtmfNTones == 0) {
        // no tones, all zero: don't do anything
        // this should take care of the case where user got an empty
        // dtmf sequence into the generator: track won't be generated
        dtmfTone = 0;
        dtmfSilence = 0;
    } else {
        if (dtmfNTones == 1) {
            // single tone, as long as the sequence
            dtmfTone = extra.GetDuration();
            dtmfSilence = 0;
        } else {
            // Don't be fooled by the fact that you divide the sequence into
            // dtmfNTones: the last slot will only contain a tone, not ending with
            // silence. Given this, the right thing to do is to divide the sequence
            // duration by dtmfNTones tones and (dtmfNTones-1) silences each sized
            // according to the duty cycle: original division was: slot=mDuration /
            // (dtmfNTones*(dtmfDutyCycle/DutyCycle.max)+(dtmfNTones-1)*(1.0-dtmfDutyCycle/DutyCycle.max))
            // which can be simplified in the one below.
            // Then just take the part that belongs to tone or silence.
            //
            double slot = extra.GetDuration()
                          / ((double)dtmfNTones + (dtmfDutyCycle / 100.0) - 1);
            dtmfTone = slot * (dtmfDutyCycle / 100.0);         // seconds
            dtmfSilence = slot * (1.0 - (dtmfDutyCycle / 100.0)); // seconds

            // Note that in the extremes we have:
            // - dutyCycle=100%, this means no silence, so each tone will measure
            // mDuration/dtmfNTones
            // - dutyCycle=0%, this means no tones, so each silence slot will
            // measure mDuration/(NTones-1) But we always count:
            // - dtmfNTones tones
            // - dtmfNTones-1 silences
        }
    }

    // `this` is the settings copy in the validator
    // Update the EffectSettings held by the dialog
    DtmfGenerator::GetSettings(settings) = *this;
}

bool DtmfSettings::isApplyAllowed() const
{
    return dtmfNTones > 0 && dtmfAmplitude >= AmplitudeMin && dtmfAmplitude <= AmplitudeMax && dtmfDutyCycle >= DutyCycleMin
           && dtmfDutyCycle <= DutyCycleMax;
}

bool DtmfGenerator::MakeDtmfTone(
    float* buffer, size_t len, float fs, char tone, sampleCount last,
    sampleCount total, float amplitude)
{
    /*
      --------------------------------------------
                  1209 Hz 1336 Hz 1477 Hz 1633 Hz

                              ABC     DEF
       697 Hz          1       2       3       A

                      GHI     JKL     MNO
       770 Hz          4       5       6       B

                      PQRS     TUV     WXYZ
       852 Hz          7       8       9       C

                              oper
       941 Hz          *       0       #       D
      --------------------------------------------
      Essentially we need to generate two sin with
      frequencies according to this table, and sum
      them up.
      sin wave is generated by:
       s(n)=sin(2*pi*n*f/fs)

      We will precalculate:
         A= 2*pi*f1/fs
         B= 2*pi*f2/fs

      And use two switch statements to select the frequency

      Note: added support for letters, like those on the keypad
            This support is only for lowercase letters: uppercase
            are still considered to be the 'military'/carrier extra
            tones.
    */

    float f1, f2 = 0.0;
    double A, B;

    // select low tone: left column
    switch (tone) {
    case '1':
    case '2':
    case '3':
    case 'A':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
        f1 = 697;
        break;
    case '4':
    case '5':
    case '6':
    case 'B':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
        f1 = 770;
        break;
    case '7':
    case '8':
    case '9':
    case 'C':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
        f1 = 852;
        break;
    case '*':
    case '0':
    case '#':
    case 'D':
        f1 = 941;
        break;
    default:
        f1 = 0;
    }

    // select high tone: top row
    switch (tone) {
    case '1':
    case '4':
    case '7':
    case '*':
    case 'g':
    case 'h':
    case 'i':
    case 'p':
    case 'q':
    case 'r':
    case 's':
        f2 = 1209;
        break;
    case '2':
    case '5':
    case '8':
    case '0':
    case 'a':
    case 'b':
    case 'c':
    case 'j':
    case 'k':
    case 'l':
    case 't':
    case 'u':
    case 'v':
        f2 = 1336;
        break;
    case '3':
    case '6':
    case '9':
    case '#':
    case 'd':
    case 'e':
    case 'f':
    case 'm':
    case 'n':
    case 'o':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
        f2 = 1477;
        break;
    case 'A':
    case 'B':
    case 'C':
    case 'D':
        f2 = 1633;
        break;
    default:
        f2 = 0;
    }

    // precalculations
    A = B = 2 * M_PI / fs;
    A *= f1;
    B *= f2;

    // now generate the wave: 'last' is used to avoid phase errors
    // when inside the inner for loop of the Process() function.
    for (decltype(len) i = 0; i < len; i++) {
        buffer[i]
            =amplitude * 0.5
              * (sin(A * (i + last).as_double()) + sin(B * (i + last).as_double()));
    }

    // generate a fade-in of duration 1/250th of second
    if (last == 0) {
        A = std::min<double>(len, (fs / kFadeInOut));
        for (size_t i = 0; i < A; i++) {
            buffer[i] *= i / A;
        }
    }

    // generate a fade-out of duration 1/250th of second
    if (last >= total - len) {
        // we are at the last buffer of 'len' size, so, offset is to
        // backup 'A' samples, from 'len'
        A = std::min<double>(len, (fs / kFadeInOut));
        size_t offset = len - A;
        wxASSERT(offset >= 0);
        for (size_t i = 0; i < A; i++) {
            buffer[i + offset] *= (1 - (i / A));
        }
    }
    return true;
}

std::shared_ptr<EffectInstance> DtmfGenerator::MakeInstance() const
{
    // TODO: don't use Effect::mT0 and Effect::mSampleRate, but use an
    // EffectContext (that class is not yet defined)
    return std::make_shared<Instance>(*this, mT0);
}
