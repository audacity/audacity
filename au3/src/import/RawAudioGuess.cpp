/**********************************************************************

  Audacity: A Digital Audio Editor

  RawAudioGuess.cpp

  Dominic Mazzoni

  Attempts to determine the format of an audio file that doesn't
  have any header information.  Returns the format as a
  libsndfile-compatible format, along with the guessed number of
  channels and the byte-offset.

**********************************************************************/

#include "RawAudioGuess.h"

#include "AudacityException.h"

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <wx/defs.h>
#include <wx/ffile.h>

#define RAW_GUESS_DEBUG 0

#if RAW_GUESS_DEBUG
static FILE* g_raw_debug_file = NULL;
#endif

static float AmpStat(float* data, size_t len)
{
    float sum, sumofsquares, avg, variance, dev;

    if (len == 0) {
        return 1.0;
    }

    /* Calculate standard deviation of the amplitudes */

    sum = 0.0;
    sumofsquares = 0.0;

    for (size_t i = 0; i < len; i++) {
        float x = fabs(data[i]);
        sum += x;
        sumofsquares += x * x;
    }

    avg = sum / len;
    variance = sumofsquares / len - (avg * avg);

    dev = sqrt(variance);

    return dev;
}

static float JumpStat(float* data, size_t len)
{
    float avg;

    /* Calculate 1.0 - avg jump
     * A score near 1.0 means avg jump is pretty small
     */

    avg = 0.0;
    for (size_t i = 0; i + 1 < len; i++) {
        avg += fabs(data[i + 1] - data[i]);
    }
    avg = 1.0 - (avg / (len - 1) / 2.0);

    return avg;
}

static float SecondDStat(float* data, size_t len)
{
    float v1=0, v2=0;
    float a1=0, a2=0;
    float sum=0;

    for (size_t i = 1; i < len; i++) {
        a2 = a1;
        v2 = v1;
        v1 = data[i] - data[i - 1];
        a1 = v1 - v2;
        sum += fabs(a1 - a2);
    }

    return sum / len;
}

static float RedundantStereo(float* data, size_t len)
{
    int c = 0;

    for (size_t i = 1; i + 1 < len; i += 2) {
        if (fabs(data[i + 1] - data[i]) > 2 * fabs(data[i] - data[i - 1])
            || 2 * fabs(data[i + 1] - data[i]) < fabs(data[i] - data[i - 1])) {
            c++;
        }
    }

    return (c * 2.0) / (len - 2);
}

static void ExtractFloats(bool doublePrec,
                          bool bigendian,
                          bool stereo,
                          size_t offset,
                          char* rawData, size_t dataSize,
                          float* data1, float* data2, size_t* len1, size_t* len2)
{
    size_t rawCount = 0;
    size_t dataCount1 = 0;
    size_t dataCount2 = 0;
    bool swap;

    *len1 = 0;
    *len2 = 0;

    if (offset) {
        rawData += offset;
        dataSize -= std::min(dataSize, offset);
    }

   #if WORDS_BIGENDIAN
    swap = !bigendian;
   #else
    swap = bigendian;
   #endif

    if (doublePrec) {
        union {
            unsigned char c[8];
            double d;
        } u;

        u.d = 0.0f;
        while (rawCount + 7 < dataSize) {
            if (swap) {
                for (size_t i = 0; i < 8; i++) {
                    u.c[7 - i] = rawData[rawCount + i];
                }
            } else {
                for (size_t i = 0; i < 8; i++) {
                    u.c[i] = rawData[rawCount + i];
                }
            }
            data1[dataCount1] = (float)u.d;
            dataCount1++;
            rawCount += 8;
        }
    } else {
        union {
            unsigned char c[4];
            float f;
        } u;

        u.f = 0.0f;
        while (rawCount + 3 < dataSize) {
            if (swap) {
                for (size_t i = 0; i < 4; i++) {
                    u.c[3 - i] = rawData[rawCount + i];
                }
            } else {
                for (size_t i = 0; i < 4; i++) {
                    u.c[i] = rawData[rawCount + i];
                }
            }
            data1[dataCount1] = u.f;
            dataCount1++;
            rawCount += 4;
        }
    }

    if (stereo) {
        dataCount1 /= 2;
        for (size_t i = 0; i < dataCount1; i++) {
            data2[i] = data1[2 * i + 1];
            data1[i] = data1[2 * i];
        }
        dataCount2 = dataCount1;
    }

    *len1 = dataCount1;
    *len2 = dataCount2;
}

static void Extract(bool bits16,
                    bool sign,
                    bool stereo,
                    bool bigendian,
                    bool offset,
                    char* rawData, int dataSizeIn,
                    float* data1, float* data2, size_t* len1, size_t* len2)
{
    size_t rawCount = 0;
    size_t dataCount1 = 0;
    size_t dataCount2 = 0;

    *len1 = 0;
    *len2 = 0;

    if (offset && bits16) {
        /* Special case so as to not flip stereo channels during analysis */
        if (stereo && !bigendian) {
            rawData += 3;
            dataSizeIn -= 3;
        } else {
            rawData++;
            dataSizeIn--;
        }
    }

    if (dataSizeIn < 1) {
        throw SimpleMessageBoxException{
                  ExceptionType::BadUserAction,
                  XO("Bad data size. Could not import audio"),
                  XO("Warning"),
                  "Error:_Importing_raw_audio"
        }
    }

    size_t dataSize = (size_t)dataSizeIn;

    if (bits16) {
        if (sign && bigendian) {
            while (rawCount + 1 < dataSize) {
                /* 16-bit signed BE */
                data1[dataCount1]
                    =(wxINT16_SWAP_ON_LE(*((signed short*)
                                           &rawData[rawCount])))
                      / 32768.0;
                dataCount1++;
                rawCount += 2;
            }
        }
        if (!sign && bigendian) {
            while (rawCount + 1 < dataSize) {
                /* 16-bit unsigned BE */
                data1[dataCount1]
                    =(wxUINT16_SWAP_ON_LE(*((unsigned short*)
                                            &rawData[rawCount])))
                      / 32768.0 - 1.0;
                dataCount1++;
                rawCount += 2;
            }
        }
        if (sign && !bigendian) {
            while (rawCount + 1 < dataSize) {
                /* 16-bit signed LE */
                data1[dataCount1]
                    =(wxINT16_SWAP_ON_BE(*((signed short*)
                                           &rawData[rawCount])))
                      / 32768.0;
                dataCount1++;
                rawCount += 2;
            }
        }
        if (!sign && !bigendian) {
            while (rawCount + 1 < dataSize) {
                /* 16-bit unsigned LE */
                data1[dataCount1]
                    =(wxUINT16_SWAP_ON_BE(*((unsigned short*)
                                            &rawData[rawCount])))
                      / 32768.0 - 1.0;
                dataCount1++;
                rawCount += 2;
            }
        }
    } else {
        /* 8-bit */
        if (sign) {
            while (rawCount < dataSize) {
                /* 8-bit signed */
                data1[dataCount1++]
                    =(*(signed char*)(&rawData[rawCount++])) / 128.0;
            }
        } else {
            while (rawCount < dataSize) {
                /* 8-bit unsigned */
                data1[dataCount1++]
                    =(*(unsigned char*)&rawData[rawCount++]) / 128.0 - 1.0;
            }
        }
    }

    if (stereo) {
        dataCount1 /= 2;
        for (size_t i = 0; i < dataCount1; i++) {
            data2[i] = data1[2 * i + 1];
            data1[i] = data1[2 * i];
        }
        dataCount2 = dataCount1;
    }

    *len1 = dataCount1;
    *len2 = dataCount2;
}

static int GuessFloatFormats(unsigned numTests, const ArrayOf<char> rawData[], size_t dataSize,
                             size_t* out_offset, unsigned* out_channels)
{
    int format;
    size_t bestOffset = 0;
    int bestEndian = 0;
    int bestPrec = 0;
    float bestSmoothAvg = 1000.0;
    size_t len1;
    size_t len2;
    bool guessStereo = false;
    unsigned stereoVotes = 0;
    unsigned monoVotes = 0;

  #if RAW_GUESS_DEBUG
    FILE* af = g_raw_debug_file;
    wxFprintf(af, "Testing float\n");
  #endif

    ArrayOf<float> data1{ dataSize + 4 };
    ArrayOf<float> data2{ dataSize + 4 };

    /*
     * First determine if it is possibly in a floating-point
     * format.  The nice thing about floating-point formats
     * is that random bytes or even random integers are
     * extremely unlikely to result in meaningful floats.
     * All we do is try interpreting the raw bytes as floating-point,
     * with a variety of offsets, both endiannesses, and both
     * precisions (32-bit float and 64-bit double), and if any of
     * them result in all finite numbers with reasonable ranges,
     * we accept them.
     *
     * Sometimes there is more than one plausible candidate, in
     * which case we take the smoothest one.  This usually happens
     * because big-endian floats actually still look and act
     * like floats when you interpret them as little-endian
     * floats with a 1-byte offset.
     */

    for (unsigned int prec = 0; prec < 2; prec++) {
        for (int endian = 0; endian < 2; endian++) {
            for (size_t offset = 0; offset < (4 * prec + 4); offset++) {
                unsigned finiteVotes = 0;
                unsigned maxminVotes = 0;
                float smoothAvg = 0;

           #if RAW_GUESS_DEBUG
                wxFprintf(af, "prec=%d endian=%d offset=%d\n",
                          prec, endian, (int)offset);
           #endif

                for (unsigned test = 0; test < numTests; test++) {
                    float min, max;

                    ExtractFloats(prec == 1, endian == 1,
                                  true, /* stereo */
                                  offset,
                                  rawData[test].get(), dataSize,
                                  data1.get(), data2.get(), &len1, &len2);

                    size_t i = 0;
                    for (; i < len1; i++) {
                        // This code is testing for NaNs.
                        // We'd like to know if all data is finite.
                        if (!(data1[i] >= 0 || data1[i] <= 0)
                            || !(data2[i] >= 0 || data2[i] <= 0)) {
                            break;
                        }
                    }
                    if (i == len1) {
                        // all data is finite.
                        finiteVotes++;
                    }

                    min = data1[0];
                    max = data1[0];
                    for (i = 1; i < len1; i++) {
                        if (data1[i] < min) {
                            min = data1[i];
                        }
                        if (data1[i] > max) {
                            max = data1[i];
                        }
                    }
                    for (i = 1; i < len2; i++) {
                        if (data2[i] < min) {
                            min = data2[i];
                        }
                        if (data2[i] > max) {
                            max = data2[i];
                        }
                    }

                    if (min < -0.01 && min >= -100000
                        && max > 0.01 && max <= 100000) {
                        maxminVotes++;
                    }

                    smoothAvg += SecondDStat(data1.get(), len1) / max;
                }

                smoothAvg /= numTests;

           #if RAW_GUESS_DEBUG
                wxFprintf(af, "finite: %ud/%ud maxmin: %ud/%ud smooth: %f\n",
                          finiteVotes, numTests, maxminVotes, numTests,
                          smoothAvg);
           #endif

                if (finiteVotes > numTests / 2
                    && finiteVotes > numTests - 2
                    && maxminVotes > numTests / 2
                    && smoothAvg < bestSmoothAvg) {
                    bestSmoothAvg = smoothAvg;
                    bestOffset = offset;
                    bestPrec = prec;
                    bestEndian = endian;
                }
            }
        }
    }

    /*
     * If none of those tests succeeded, it's probably not
     * actually floating-point data.  Return 0 so the
     * main function will try guessing an integer format.
     */

    if (bestSmoothAvg >= 1000.0) {
        return 0;
    }

    /*
     * We still have to test for mono/stereo.  For an explanation
     * of these tests, see the comments next to the stereo/mono
     * tests for 8-bit or 16-bit data.
     */

    for (unsigned test = 0; test < numTests; test++) {
        float leftChannel, rightChannel, combinedChannel;

        ExtractFloats(bestPrec == 1, bestEndian == 1,
                      true, /* stereo */
                      bestOffset,
                      rawData[test].get(), dataSize,
                      data1.get(), data2.get(), &len1, &len2);
        leftChannel = JumpStat(data1.get(), len1);
        rightChannel = JumpStat(data2.get(), len2);
        ExtractFloats(bestPrec == 1, bestEndian == 1,
                      false, /* stereo */
                      bestOffset,
                      rawData[test].get(), dataSize,
                      data1.get(), data2.get(), &len1, &len2);
        combinedChannel = JumpStat(data1.get(), len1);

        if (leftChannel > combinedChannel
            && rightChannel > combinedChannel) {
            stereoVotes++;
        } else {
            monoVotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "stereo: %ud mono: %ud\n", stereoVotes, monoVotes);
  #endif

    guessStereo = (stereoVotes > monoVotes);

    if (!guessStereo) {
        /* test for repeated-byte, redundant stereo */

        unsigned rstereoVotes = 0;
        unsigned rmonoVotes = 0;

        for (unsigned test = 0; test < numTests; test++) {
            float redundant;

            ExtractFloats(bestPrec == 1, bestEndian == 1,
                          false, /* stereo */
                          bestOffset,
                          rawData[test].get(), dataSize,
                          data1.get(), data2.get(), &len1, &len2);
            redundant = RedundantStereo(data1.get(), len1);

        #if RAW_GUESS_DEBUG
            wxFprintf(af, "redundant: %f\n", redundant);
        #endif

            if (redundant > 0.8) {
                rstereoVotes++;
            } else {
                rmonoVotes++;
            }
        }

     #if RAW_GUESS_DEBUG
        wxFprintf(af, "rstereo: %ud rmono: %ud\n", rstereoVotes, rmonoVotes);
     #endif

        guessStereo = (rstereoVotes > rmonoVotes);
    }

  #if RAW_GUESS_DEBUG
    if (guessStereo) {
        wxFprintf(af, "stereo\n");
    } else {
        wxFprintf(af, "mono\n");
    }
  #endif

    *out_offset = bestOffset;

    if (guessStereo) {
        *out_channels = 2;
    } else {
        *out_channels = 1;
    }

    if (bestPrec) {
        format = SF_FORMAT_RAW | SF_FORMAT_DOUBLE;
    } else {
        format = SF_FORMAT_RAW | SF_FORMAT_FLOAT;
    }

    if (bestEndian) {
        format |= SF_ENDIAN_BIG;
    } else {
        format |= SF_ENDIAN_LITTLE;
    }

    return format;
}

static int Guess8Bit(unsigned numTests, const ArrayOf<char> rawData[], size_t dataSize, unsigned* out_channels)
{
    bool guessSigned = false;
    bool guessStereo = false;
    unsigned signvotes = 0;
    unsigned unsignvotes = 0;
    unsigned stereoVotes = 0;
    unsigned monoVotes = 0;

    ArrayOf<float> data1 { dataSize + 4 };
    ArrayOf<float> data2 { dataSize + 4 };
    size_t len1;
    size_t len2;

  #if RAW_GUESS_DEBUG
    FILE* af = g_raw_debug_file;
    wxFprintf(af, "8-bit\n");
  #endif

    /*
     * Compare signed to unsigned, interpreted as if the file were
     * stereo just to be safe.  If the file is actually mono, the test
     * still works, and we lose a tiny bit of accuracy.  (It would not make
     * sense to assume the file is mono, because if the two tracks are not
     * very similar we would get inaccurate results.)
     *
     * The JumpTest measures the average jump between two successive samples
     * and returns a value 0-1.  0 is maximally discontinuous, 1 is smooth.
     */

    for (unsigned test = 0; test < numTests; test++) {
        float signL, signR, unsignL, unsignR;

        Extract(0, 1, 1, 0, /* 8-bit signed stereo */
                false, rawData[test].get(), dataSize,
                data1.get(), data2.get(), &len1, &len2);
        signL = JumpStat(data1.get(), len1);
        signR = JumpStat(data2.get(), len2);
        Extract(0, 0, 1, 0, /* 8-bit unsigned stereo */
                false, rawData[test].get(), dataSize,
                data1.get(), data2.get(), &len1, &len2);
        unsignL = JumpStat(data1.get(), len1);
        unsignR = JumpStat(data2.get(), len2);

        if (signL > unsignL) {
            signvotes++;
        } else {
            unsignvotes++;
        }

        if (signR > unsignR) {
            signvotes++;
        } else {
            unsignvotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "sign: %ud unsign: %ud\n", signvotes, unsignvotes);
  #endif

    guessSigned = (signvotes > unsignvotes);

  #if RAW_GUESS_DEBUG
    if (guessSigned) {
        wxFprintf(af, "signed\n");
    } else {
        wxFprintf(af, "unsigned\n");
    }
  #endif

    /* Finally we test stereo/mono.  We use the same JumpStat, and say
     * that the file is stereo if and only if for the majority of the
     * tests, the left channel and the right channel are more smooth than
     * the entire stream interpreted as one channel.
     */

    for (unsigned test = 0; test < numTests; test++) {
        float leftChannel, rightChannel, combinedChannel;

        Extract(0, guessSigned, 1, 0, 0, rawData[test].get(), dataSize, data1.get(),
                data2.get(), &len1, &len2);
        leftChannel = JumpStat(data1.get(), len1);
        rightChannel = JumpStat(data2.get(), len2);
        Extract(0, guessSigned, 0, 0, 0, rawData[test].get(), dataSize, data1.get(),
                data2.get(), &len1, &len2);
        combinedChannel = JumpStat(data1.get(), len1);

        if (leftChannel > combinedChannel
            && rightChannel > combinedChannel) {
            stereoVotes++;
        } else {
            monoVotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "stereo: %ud mono: %ud\n", stereoVotes, monoVotes);
  #endif

    guessStereo = (stereoVotes > monoVotes);

    if (!guessStereo) {
        /* test for repeated-byte, redundant stereo */

        unsigned rstereoVotes = 0;
        unsigned rmonoVotes = 0;

        for (unsigned test = 0; test < numTests; test++) {
            float redundant;

            Extract(0, guessSigned, 0, 0, 0, rawData[test].get(), dataSize,
                    data1.get(), data2.get(), &len1, &len2);
            redundant = RedundantStereo(data1.get(), len1);

        #if RAW_GUESS_DEBUG
            wxFprintf(af, "redundant: %f\n", redundant);
        #endif

            if (redundant > 0.8) {
                rstereoVotes++;
            } else {
                rmonoVotes++;
            }
        }

     #if RAW_GUESS_DEBUG
        wxFprintf(af, "rstereo: %ud rmono: %ud\n", rstereoVotes, rmonoVotes);
     #endif

        guessStereo = (rstereoVotes > rmonoVotes);
    }

  #if RAW_GUESS_DEBUG
    if (guessStereo) {
        wxFprintf(af, "stereo\n");
    } else {
        wxFprintf(af, "mono\n");
    }
  #endif

    if (guessStereo) {
        *out_channels = 2;
    } else {
        *out_channels = 1;
    }

    if (guessSigned) {
        return SF_FORMAT_RAW | SF_FORMAT_PCM_S8;
    } else {
        return SF_FORMAT_RAW | SF_FORMAT_PCM_U8;
    }
}

static int Guess16Bit(unsigned numTests, const ArrayOf<char> rawData[],
                      size_t dataSize, bool evenMSB,
                      size_t* out_offset, unsigned* out_channels)
{
    int format;
    bool guessSigned = false;
    bool guessStereo = false;
    bool guessBigEndian = false;
    bool guessOffset = false;
    unsigned signvotes = 0;
    unsigned unsignvotes = 0;
    unsigned stereoVotes = 0;
    unsigned monoVotes = 0;
    unsigned formerVotes = 0;
    unsigned latterVotes = 0;
    ArrayOf<char> rawData2{ dataSize + 4 };
    ArrayOf<float> data1{ dataSize + 4 };
    ArrayOf<float> data2{ dataSize + 4 };
    size_t len1;
    size_t len2;

  #if RAW_GUESS_DEBUG
    FILE* af = g_raw_debug_file;
    wxFprintf(af, "16-bit\n");
  #endif

    /*
     * Do the signed/unsigned test by using only the MSB.
     */

    for (unsigned test = 0; test < numTests; test++) {
        float signL, signR, unsignL, unsignR;

        /* Extract a NEW array of the MSBs only: */

        for (size_t i = 0; i < dataSize / 2; i++) {
            rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];
        }

        /* Test signed/unsigned of the MSB */

        Extract(0, 1, 1, 0, /* 8-bit signed stereo */
                0, rawData2.get(), dataSize / 2, data1.get(), data2.get(), &len1, &len2);
        signL = JumpStat(data1.get(), len1);
        signR = JumpStat(data2.get(), len2);
        Extract(0, 0, 1, 0, /* 8-bit unsigned stereo */
                0, rawData2.get(), dataSize / 2, data1.get(), data2.get(), &len1, &len2);
        unsignL = JumpStat(data1.get(), len1);
        unsignR = JumpStat(data2.get(), len2);

        if (signL > unsignL) {
            signvotes++;
        } else {
            unsignvotes++;
        }

        if (signR > unsignR) {
            signvotes++;
        } else {
            unsignvotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "sign: %ud unsign: %ud\n", signvotes, unsignvotes);
  #endif

    guessSigned = (signvotes > unsignvotes);

  #if RAW_GUESS_DEBUG
    if (guessSigned) {
        wxFprintf(af, "signed\n");
    } else {
        wxFprintf(af, "unsigned\n");
    }
  #endif

    /*
     * Test mono/stereo using only the MSB
     */

    for (unsigned test = 0; test < numTests; test++) {
        float leftChannel, rightChannel, combinedChannel;

        /* Extract a NEW array of the MSBs only: */

        for (size_t i = 0; i < dataSize / 2; i++) {
            rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];
        }

        Extract(0, guessSigned, 1, 0, 0,
                rawData2.get(), dataSize / 2, data1.get(), data2.get(), &len1, &len2);
        leftChannel = JumpStat(data1.get(), len1);
        rightChannel = JumpStat(data2.get(), len2);
        Extract(0, guessSigned, 0, 0, 0,
                rawData2.get(), dataSize / 2, data1.get(), data2.get(), &len1, &len2);
        combinedChannel = JumpStat(data1.get(), len1);

        if (leftChannel > combinedChannel
            && rightChannel > combinedChannel) {
            stereoVotes++;
        } else {
            monoVotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "stereoVotes: %ud monoVotes: %ud\n", stereoVotes, monoVotes);
  #endif

    guessStereo = (stereoVotes > monoVotes);

    if (!guessStereo) {
        /* Test for repeated-byte, redundant stereo */

        unsigned rstereoVotes = 0;
        unsigned rmonoVotes = 0;

        for (unsigned test = 0; test < numTests; test++) {
            float redundant;

            /* Extract a NEW array of the MSBs only: */

            for (size_t i = 0; i < dataSize / 2; i++) {
                rawData2[i] = rawData[test][2 * i + (evenMSB ? 0 : 1)];
            }

            Extract(0, guessSigned, 0, 0, 0, rawData2.get(), dataSize / 2,
                    data1.get(), data2.get(), &len1, &len2);

            redundant = RedundantStereo(data1.get(), len1);

            if (redundant > 0.8) {
                rstereoVotes++;
            } else {
                rmonoVotes++;
            }
        }

     #if RAW_GUESS_DEBUG
        wxFprintf(af, "rstereoVotes: %ud rmonoVotes: %ud\n",
                  rstereoVotes, rmonoVotes);
     #endif

        guessStereo = (rstereoVotes > rmonoVotes);
    }

  #if RAW_GUESS_DEBUG
    if (guessStereo) {
        wxFprintf(af, "stereo\n");
    } else {
        wxFprintf(af, "mono\n");
    }
  #endif

    /*
     * Finally, determine the endianness and offset.
     *
     * Even MSB -> BigEndian or LittleEndian with Offset
     * Odd MSB -> LittleEndian or BigEndian with Offset
     */

    guessBigEndian = evenMSB;
    guessOffset = 0;

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "evenMSB: %d BE: %d\n", evenMSB, guessBigEndian);
  #endif

    for (unsigned test = 0; test < numTests; test++) {
        float former, latter;
        int offs;

        /* Extract a NEW array of the MSBs only: */

        if (guessStereo) {
            for (size_t i = 0; i + 1 < (dataSize / 4); i++) {
                rawData2[i]
                    =rawData[test][4 * i + (evenMSB ? 0 : 1)];
            }
        } else {
            for (size_t i = 0; i + 1 < (dataSize / 2); i++) {
                rawData2[i]
                    =rawData[test][2 * i + (evenMSB ? 0 : 1)];
            }
        }

        former = 0.0;
        Extract(1, guessSigned, guessStereo, guessBigEndian, guessOffset,
                rawData[test].get(), dataSize - 4, data1.get(), data2.get(), &len1, &len2);

        offs=(!guessBigEndian);

        for (size_t i = 3; i + 4 < len1; i++) {
            if (rawData2[offs + i - 2] == rawData2[offs + i - 1]
                && rawData2[offs + i] == rawData2[offs + i - 1] + 1
                && rawData2[offs + i] == rawData2[offs + i + 1]) {
                former += data1[i] - data1[i - 1];
            }
        }

        latter = 0.0;
        Extract(1, guessSigned, guessStereo, !guessBigEndian,
                !guessOffset, rawData[test].get(), dataSize, data1.get(), data2.get(),
                &len1, &len2);

        offs=(guessBigEndian);

        for (size_t i = 3; i + 4 < len1; i++) {
            if (rawData2[offs + i - 2] == rawData2[offs + i - 1]
                && rawData2[offs + i] == rawData2[offs + i - 1] + 1
                && rawData2[offs + i] == rawData2[offs + i + 1]) {
                latter += data1[i] - data1[i - 1];
            }
        }

     #if RAW_GUESS_DEBUG
        wxFprintf(af, "former: %f latter: %f\n", former, latter);
     #endif

        if (former <= latter) {
            formerVotes++;
        } else {
            latterVotes++;
        }
    }

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "former (BE/LE): %ud latter (LE+/BE+): %ud\n",
              formerVotes, latterVotes);
  #endif

    // High barrier, since odd byte offsets are very rare
    if (latterVotes > formerVotes * 2) {
        guessBigEndian = !guessBigEndian;
        guessOffset = !guessOffset;
    }

  #if RAW_GUESS_DEBUG
    if (guessBigEndian) {
        wxFprintf(af, "big endian\n");
    } else {
        wxFprintf(af, "little endian\n");
    }
  #endif

  #if RAW_GUESS_DEBUG
    if (guessOffset) {
        wxFprintf(af, "offset 1 byte\n");
    } else {
        wxFprintf(af, "no byte offset\n");
    }
  #endif

    format = SF_FORMAT_RAW | SF_FORMAT_PCM_16;

    if (guessBigEndian) {
        format |= SF_ENDIAN_BIG;
    } else {
        format |= SF_ENDIAN_LITTLE;
    }

    if (guessOffset) {
        *out_offset = 1;
    }

    if (guessStereo) {
        *out_channels = 2;
    } else {
        *out_channels = 1;
    }

    return format;
}

static int GuessIntFormats(unsigned numTests, const ArrayOf<char> rawData[], size_t dataSize,
                           size_t* out_offset, unsigned* out_channels)
{
    int format = SF_FORMAT_RAW;
    bool guess16bit = false;
    bool evenMSB;
    ArrayOf<float> data1{ dataSize + 4 };
    ArrayOf<float> data2{ dataSize + 4 };
    size_t len1;
    size_t len2;
    unsigned vote8 = 0;
    unsigned vote16 = 0;
    unsigned evenMSBVotes = 0;
    unsigned oddMSBVotes = 0;

  #if RAW_GUESS_DEBUG
    FILE* af = g_raw_debug_file;
  #endif

    *out_channels = 1;
    *out_offset = 0;

    /*
     * First test: we attempt to determine if the data is 8-bit or 16-bit.
     * We extract the odd and even bytes interpreted as signed-valued samples,
     * and compare their amplitude distributions.  Noting that in 16-bit values,
     * the less significant 8 bits should have roughly flat distribution, while
     * the more significant 8 bits should have a tighter distribution, with a
     * smaller standard deviation.
     *
     * Note that this correctly makes the distinction whether we are dealing
     * with mono or stereo data.
     */

    for (unsigned test = 0; test < numTests; test++) {
        float even, odd;

        Extract(0, 1, 1, 0, /* 8-bit signed stereo */
                false, rawData[test].get(), dataSize,
                data1.get(), data2.get(), &len1, &len2);
        even = AmpStat(data1.get(), len1);
        odd = AmpStat(data2.get(), len2);
        if ((even > 0.15) && (odd > 0.15)) {
        #if RAW_GUESS_DEBUG
            wxFprintf(af, "Both appear random: %.2f, %.2f.\n", even, odd);
        #endif
        } else if ((even > 0.15) || (odd > 0.15)) {
            vote16++;
        } else {
            vote8++;
        }

        /* Record which of the two was the MSB for future reference */
        if (even < odd) {
            evenMSBVotes++;
        } else {
            oddMSBVotes++;
        }
    }

    evenMSB = (evenMSBVotes > oddMSBVotes);

  #if RAW_GUESS_DEBUG
    wxFprintf(af, "evenMSBVote: %ud oddMSBVote: %ud\n",
              evenMSBVotes, oddMSBVotes);
    wxFprintf(af, "vote8: %ud vote16: %ud\n", vote8, vote16);
  #endif

    guess16bit = (vote8 <= vote16);

    if (!guess16bit) {
        format = Guess8Bit(numTests, rawData, dataSize, out_channels);
    } else {
        format = Guess16Bit(numTests, rawData,
                            dataSize, evenMSB,
                            out_offset, out_channels);
    }

    return format;
}

int RawAudioGuess(const wxString& in_fname,
                  size_t* out_offset, unsigned* out_channels)
{
    const unsigned numTests = 11;
    size_t headerSkipSize = 64;
    size_t dataSize = 16384;
    int format = SF_FORMAT_RAW;
    FILE* inf;
    size_t fileLen;
    size_t read_data;

  #if RAW_GUESS_DEBUG
    FILE* af = fopen("raw.txt", "a");
    g_raw_debug_file = af;
    wxFprintf(af, "File: %s\n", in_fname);
  #endif

    *out_offset = 0;
    *out_channels = 1;

    wxFFile in_wxFFile(in_fname, wxT("rb"));

    // JKC FALSE changed to -1.
    if (!in_wxFFile.IsOpened()) {
        return -1;
    }
    inf = in_wxFFile.fp();

    if (!inf) {
     #if RAW_GUESS_DEBUG
        fclose(af);
        g_raw_debug_file = NULL;
     #endif

        return -1;
    }

    // FIXME: TRAP_ERR fseek return in RawAudioGuess unchecked.
    fseek(inf, 0, SEEK_END);
    fileLen = ftell(inf);

    if (fileLen < 8) {
        return -1;
    }

    if (fileLen < headerSkipSize) {
        headerSkipSize = 0;
    }

    if (fileLen < dataSize) {
        dataSize = fileLen / 2;
    }

    wxASSERT(dataSize >= 4);
    wxASSERT(dataSize <= fileLen);

    ArraysOf<char> rawData{ numTests, dataSize + 4 };

    for (unsigned test = 0; test < numTests; test++) {
        int startPoint;

        startPoint = (fileLen - dataSize) * (test + 1) / (numTests + 2);

        /* Make it a multiple of 16 (stereo double-precision) */
        startPoint = (startPoint / 16) * 16;

        // FIXME: TRAP_ERR fseek return in MultiFormatReader unchecked.
        fseek(inf, headerSkipSize + startPoint, SEEK_SET);
        read_data = fread(rawData[test].get(), 1, dataSize, inf);
        if (read_data != dataSize && ferror(inf)) {
            perror("fread error in RawAudioGuess");
        }
    }

    in_wxFFile.Close();

    /*
     * The floating-point tests will only return a valid format
     * if it's almost certainly floating-point data.  On the other
     * hand, the integer tests will always return something, since
     * almost anything looks like it could be integer data...
     */

    format = GuessFloatFormats(numTests,
                               rawData.get(),
                               dataSize,
                               out_offset, out_channels);

    if (format == 0) {
        format = GuessIntFormats(numTests,
                                 rawData.get(),
                                 dataSize,
                                 out_offset, out_channels);
    }

  #if RAW_GUESS_DEBUG
    fclose(af);
    g_raw_debug_file = NULL;
  #endif

    return format;
}
