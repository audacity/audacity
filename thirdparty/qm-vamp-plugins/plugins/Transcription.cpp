/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
  QM Vamp Plugin Set

  Centre for Digital Music, Queen Mary, University of London.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

/*transcription vamp plugin: Ruohua Zhou, Josh Reiss, josh.reiss@elec.qmul.ac.uk */

#include "Transcription.h"
#include <vamp-sdk/PluginAdapter.h>
#include <math.h>
#include <stdlib.h>
#include <algorithm>

using std::string;
using std::vector;
using std::cerr;
using std::endl;

const double EualCurve960[960] = {
    83.750025,83.532690,83.315770,83.099260,82.883159,82.667463,82.452170,82.237276,82.022779,81.808675,
    81.594963,81.381639,81.168699,80.956142,80.743964,80.532163,80.320735,80.109677,79.898987,79.688663,79.478700,79.269096,79.059848,78.850953,
    78.642408,78.434211,78.226359,78.018848,77.811676,77.604839,77.398336,77.192162,76.986316,76.780794,76.575593,76.370710,76.166143,75.961889,
    75.757945,75.554307,75.350973,75.147940,74.945205,74.742766,74.540618,74.338761,74.137189,73.935902,73.734895,73.534166,73.333712,73.133529,
    72.933616,72.733970,72.534586,72.335463,72.136598,71.937987,71.739628,71.541517,71.343653,71.146032,70.948650,70.751506,70.554597,70.357919,
    70.161469,69.965245,69.769244,69.573462,69.377898,69.182548,68.987408,68.792477,68.597752,68.403228,68.208905,68.014781,67.820873,67.627197,
    67.433772,67.240617,67.047749,66.855187,66.662949,66.471053,66.279516,66.088358,65.897597,65.707250,65.517336,65.327873,65.138879,64.950373,
    64.762372,64.574894,64.387959,64.201583,64.015785,63.830584,63.645997,63.462043,63.278739,63.096105,62.914158,62.732915,62.552397,62.372620,
    62.193602,62.015363,61.837920,61.661291,61.485494,61.310549,61.136471,60.963274,60.790941,60.619447,60.448770,60.278885,60.109770,59.941401,
    59.773755,59.606807,59.440536,59.274916,59.109924,58.945538,58.781733,58.618486,58.455773,58.293572,58.131858,57.970608,57.809799,57.649407,
    57.489408,57.329780,57.170498,57.011539,56.852880,56.694496,56.536366,56.378464,56.220768,56.063255,55.905900,55.748680,55.591571,55.434551,
    55.277595,55.120681,54.963784,54.806886,54.649983,54.493077,54.336169,54.179261,54.022353,53.865448,53.708546,53.551650,53.394759,53.237877,
    53.081003,52.924139,52.767287,52.610448,52.453624,52.296815,52.140023,51.983250,51.826496,51.669763,51.513053,51.356366,51.199705,51.043070,
    50.886463,50.729885,50.573337,50.416821,50.260338,50.103890,49.947478,49.791103,49.634766,49.478469,49.322214,49.166001,49.009832,48.853710,
    48.697648,48.541659,48.385757,48.229958,48.074273,47.918719,47.763308,47.608055,47.452974,47.298080,47.143385,46.988904,46.834652,46.680642,
    46.526889,46.373405,46.220207,46.067307,45.914720,45.762460,45.610540,45.458976,45.307780,45.156968,45.006553,44.856549,44.706971,44.557832,
    44.409146,44.260928,44.113192,43.965951,43.819220,43.673013,43.527344,43.382227,43.237676,43.093703,42.950305,42.807478,42.665218,42.523520,
    42.382381,42.241794,42.101757,41.962264,41.823311,41.684894,41.547008,41.409648,41.272811,41.136491,41.000685,40.865387,40.730594,40.596301,
    40.462503,40.329195,40.196375,40.064036,39.932175,39.800787,39.669867,39.539412,39.409417,39.279876,39.150787,39.022143,38.893942,38.766178,
    38.638846,38.511944,38.385465,38.259405,38.133761,38.008525,37.883679,37.759203,37.635076,37.511278,37.387789,37.264588,37.141656,37.018971,
    36.896513,36.774262,36.652197,36.530298,36.408545,36.286918,36.165395,36.043957,35.922583,35.801253,35.679947,35.558643,35.437322,35.315964,
    35.194547,35.073052,34.951458,34.829745,34.707892,34.585879,34.463686,34.341293,34.218678,34.095822,33.972704,33.849303,33.725600,33.601574,
    33.477205,33.352481,33.227425,33.102069,32.976445,32.850585,32.724520,32.598284,32.471906,32.345420,32.218858,32.092250,31.965629,31.839028,
    31.712477,31.586009,31.459655,31.333448,31.207419,31.081601,30.956024,30.830722,30.705725,30.581067,30.456777,30.332890,30.209436,30.086447,
    29.963955,29.841993,29.720591,29.599783,29.479599,29.360071,29.241233,29.123114,29.005748,28.889166,28.773400,28.658474,28.544378,28.431095,
    28.318607,28.206897,28.095947,27.985740,27.876257,27.767481,27.659396,27.551982,27.445224,27.339102,27.233599,27.128699,27.024383,26.920633,
    26.817433,26.714764,26.612609,26.510951,26.409772,26.309053,26.208779,26.108930,26.009491,25.910442,25.811766,25.713446,25.615465,25.517804,
    25.420446,25.323374,25.226570,25.130016,25.033695,24.937589,24.841681,24.745955,24.650409,24.555043,24.459856,24.364847,24.270016,24.175363,
    24.080887,23.986588,23.892466,23.798520,23.704751,23.611156,23.517737,23.424492,23.331422,23.238526,23.145803,23.053253,22.960877,22.868672,
    22.776640,22.684779,22.593090,22.501572,22.410224,22.319046,22.228038,22.137200,22.046530,21.956029,21.865697,21.775532,21.685535,21.595704,
    21.506041,21.416544,21.327213,21.238047,21.149047,21.060211,20.971540,20.883034,20.794691,20.706512,20.618496,20.530642,20.442952,20.355423,
    20.268057,20.180852,20.093808,20.006925,19.920202,19.833640,19.747237,19.660994,19.574910,19.488985,19.403218,19.317610,19.232159,19.146866,
    19.061729,18.976750,18.891927,18.807260,18.722749,18.638393,18.554193,18.470147,18.386255,18.302518,18.218934,18.135504,18.052227,17.969105,
    17.886151,17.803379,17.720805,17.638444,17.556310,17.474419,17.392786,17.311425,17.230351,17.149581,17.069127,16.989007,16.909233,16.829822,
    16.750789,16.672148,16.593914,16.516103,16.438729,16.361808,16.285354,16.209382,16.133907,16.058945,15.984510,15.910617,15.837282,15.764518,
    15.692342,15.620768,15.549811,15.479486,15.409809,15.340793,15.272455,15.204808,15.137869,15.071646,15.006129,14.941300,14.877144,14.813643,
    14.750781,14.688540,14.626906,14.565860,14.505386,14.445467,14.386088,14.327231,14.268879,14.211016,14.153626,14.096691,14.040195,13.984121,
    13.928453,13.873174,13.818267,13.763716,13.709504,13.655615,13.602031,13.548736,13.495714,13.442948,13.390420,13.338115,13.286016,13.234107,
    13.182369,13.130788,13.079346,13.028026,12.976813,12.925693,12.874671,12.823756,12.772958,12.722285,12.671746,12.621351,12.571107,12.521025,
    12.471113,12.421380,12.371835,12.322488,12.273346,12.224419,12.175717,12.127248,12.079020,12.031044,11.983328,11.935880,11.888711,11.841828,
    11.795242,11.748960,11.702993,11.657348,11.612035,11.567063,11.522441,11.478178,11.434282,11.390764,11.347631,11.304893,11.262558,11.220637,
    11.179137,11.138068,11.097437,11.057252,11.017521,10.978252,10.939452,10.901129,10.863290,10.825944,10.789098,10.752760,10.716937,10.681638,
    10.646869,10.612640,10.578956,10.545827,10.513259,10.481261,10.449840,10.419004,10.388760,10.359117,10.330082,10.301663,10.273867,10.246702,
    10.220176,10.194296,10.169071,10.144508,10.120615,10.097399,10.074868,10.053030,10.031892,10.011463,9.991749,9.972762,9.954523,9.937056,9.920385,
    9.904534,9.889527,9.875389,9.862144,9.849815,9.838428,9.828005,9.818572,9.810152,9.802770,9.796449,9.791214,9.787089,9.784099,9.782266,9.781616,
    9.782172,9.783959,9.787001,9.791322,9.796946,9.803897,9.812200,9.821878,9.832956,9.845457,9.859407,9.874829,9.891747,9.910185,9.930168,9.951720,
    9.974864,9.999625,10.026008,10.053933,10.083304,10.114023,10.145991,10.179112,10.213287,10.248419,10.284410,10.321161,10.358576,10.396556,10.435004,
    10.473821,10.512911,10.552175,10.591516,10.630835,10.670035,10.709018,10.747686,10.785942,10.823688,10.860826,10.897258,10.932886,10.967613,11.001341,
    11.033972,11.065408,11.095552,11.124305,11.151570,11.177249,11.201245,11.223459,11.243793,11.262151,11.278450,11.292676,11.304827,11.314906,11.322913,
    11.328848,11.332713,11.334508,11.334233,11.331889,11.327477,11.320998,11.312453,11.301841,11.289164,11.274422,11.257616,11.238747,11.217816,11.194822,
    11.169767,11.142652,11.113476,11.082241,11.048948,11.013597,10.976189,10.936724,10.895203,10.851627,10.805996,10.758312,10.708574,10.656784,10.602942,
    10.547049,10.489106,10.429113,10.367082,10.303073,10.237155,10.169399,10.099876,10.028655,9.955807,9.881403,9.805512,9.728206,9.649554,9.569627,9.488495,
    9.406228,9.322897,9.238573,9.153325,9.067225,8.980341,8.892745,8.804508,8.715698,8.626388,8.536646,8.446544,8.356152,8.265539,8.174778,8.083937,7.993087,
    7.902299,7.811643,7.721190,7.631008,7.541170,7.451746,7.362804,7.274417,7.186644,7.099504,7.013003,6.927151,6.841956,6.757424,6.673565,6.590385,6.507894,
    6.426099,6.345008,6.264629,6.184970,6.106039,6.027843,5.950392,5.873692,5.797752,5.722579,5.648183,5.574570,5.501748,5.429727,5.358512,5.288114,5.218538,
    5.149794,5.081890,5.014832,4.948630,4.883292,4.818824,4.755236,4.692535,4.630729,4.569826,4.509834,4.450761,4.392616,4.335415,4.279172,4.223905,4.169630,
    4.116362,4.064118,4.012914,3.962766,3.913691,3.865703,3.818820,3.773058,3.728432,3.684960,3.642656,3.601538,3.561621,3.522921,3.485455,3.449239,3.414289,
    3.380620,3.348250,3.317194,3.287469,3.259090,3.232074,3.206437,3.182194,3.159363,3.137959,3.117999,3.099498,3.082473,3.066939,3.052914,3.040413,3.029451,
    3.020039,3.012186,3.005904,3.001201,2.998087,2.996571,2.996665,2.998377,3.001718,3.006696,3.013323,3.021607,3.031559,3.043187,3.056503,3.071516,3.088235,
    3.106671,3.126833,3.148731,3.172374,3.197773,3.224938,3.253877,3.284601,3.317120,3.351444,3.387581,3.425543,3.465339,3.506978,3.550470,3.595826,3.643054,
    3.692166,3.743169,3.796075,3.850896,3.907655,3.966377,4.027088,4.089815,4.154581,4.221415,4.290340,4.361382,4.434569,4.509924,4.587474,4.667245,4.749261,
    4.833550,4.920136,5.009046,5.100305,5.193938,5.289972,5.388432,5.489343,5.592732,5.698625,5.807046,5.918022,6.031578,6.147741,6.266535,6.387986,6.512121,
    6.638964,6.768542,6.900880,7.036004,7.173939,7.314712,7.458348,7.604856,7.754175,7.906227,8.060936,8.218223,8.378012,8.540225,8.704784,8.871612,9.040631,
    9.211765,9.384934,9.560063,9.737073,9.915888,10.096429,10.278619,10.462380,10.647636,10.834309,11.022321,11.211594,11.402052,11.593616,11.786210,11.979755,
    12.174175,12.369392,12.565329,12.761907,12.959049,13.156679,13.354718,13.553089,13.751715,13.950518,14.149420,14.348345,14.547211,14.745925,14.944391,
    15.142512,15.340191,15.537333,15.733840,15.929615,16.124564   
};
void Transcribe(int Len,int inputLen,double *SoundIn,double *out,double *outArray2,double *outArray3,double SampleRate);

Transcription::Transcription(float inputSampleRate) :
    Plugin(inputSampleRate),
    m_stepSize(0)
{
    m_SoundIn=0;
    m_SampleN=0;
    m_AllocN = 0;
    m_Excess = false;
}

Transcription::~Transcription()
{
    free(m_SoundIn);
}

string
Transcription::getIdentifier() const
{
    return "qm-transcription";
}

string
Transcription::getName() const
{
    return "Polyphonic Transcription";
}

string
Transcription::getDescription() const
{
    return "Transcribe the input audio to estimated notes";
}

string
Transcription::getMaker() const
{
    return "Queen Mary, University of London";
}

int
Transcription::getPluginVersion() const
{
    return 1;
}

string
Transcription::getCopyright() const
{
    return "Plugin by Dr. Ruohua Zhou.  Copyright (c) 2008-2009 QMUL - All Rights Reserved";
}

size_t
Transcription::getPreferredStepSize() const
{
    return 441;
}

size_t
Transcription::getPreferredBlockSize() const
{
    return 441;
}

bool
Transcription::initialise(size_t channels, size_t stepSize, size_t blockSize)
{
    if (channels < getMinChannelCount() ||
	channels > getMaxChannelCount()) return false;

    if (m_inputSampleRate < 4410 || m_inputSampleRate > 441000) return false;

    m_stepSize = std::min(stepSize, blockSize);
    m_blockSize=blockSize;

    m_SampleN = 0;

    return true;
}

void
Transcription::reset()
{
    free(m_SoundIn);
    m_SoundIn = 0;
    m_SampleN = 0;
    m_AllocN = 0;
    m_Excess = false;
    m_Base = Vamp::RealTime();
}

Transcription::OutputList
Transcription::getOutputDescriptors() const
{
    OutputList list;

    OutputDescriptor zc;
    zc.identifier = "transcription";
    zc.name = "Transcription";
    zc.description = "Estimated note pitch (MIDI note number from 0 to 127)";
    zc.unit = "MIDI units";
    zc.hasFixedBinCount = true;
    zc.binCount = 1;
    zc.hasKnownExtents = true;
    zc.minValue = 0;
    zc.maxValue = 127;
    zc.isQuantized = true;
    zc.quantizeStep = 1.0;
    zc.hasDuration = true;
    zc.sampleType = OutputDescriptor::VariableSampleRate;

/* no -- this is the result of a confusion between bin indices and values
    {

        zc.binNames.push_back("A0");// MIDI Note 21
        zc.binNames.push_back("A0#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C1");
        zc.binNames.push_back("C1#");
        zc.binNames.push_back("D1");
        zc.binNames.push_back("D1#");
        zc.binNames.push_back("E1");
        zc.binNames.push_back("F1");
        zc.binNames.push_back("F1#");
        zc.binNames.push_back("G1");
        zc.binNames.push_back("G1#");

        zc.binNames.push_back("A1");// MIDI Note 33
        zc.binNames.push_back("A1#");
        zc.binNames.push_back("B1");
        zc.binNames.push_back("C2");
        zc.binNames.push_back("C2#");
        zc.binNames.push_back("D2");
        zc.binNames.push_back("D2#");
        zc.binNames.push_back("E2");
        zc.binNames.push_back("F2");
        zc.binNames.push_back("F2#");
        zc.binNames.push_back("G2");
        zc.binNames.push_back("G2#");

        zc.binNames.push_back("A2");// MIDI Note 45
        zc.binNames.push_back("A2#");
        zc.binNames.push_back("B3");
        zc.binNames.push_back("C3");
        zc.binNames.push_back("C3#");
        zc.binNames.push_back("D3");
        zc.binNames.push_back("D3#");
        zc.binNames.push_back("E3");
        zc.binNames.push_back("F3");
        zc.binNames.push_back("F3#");
        zc.binNames.push_back("G3");
        zc.binNames.push_back("G3#");

        zc.binNames.push_back("A3");// MIDI Note 57
        zc.binNames.push_back("A3#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C4");
        zc.binNames.push_back("C4#");
        zc.binNames.push_back("D4");
        zc.binNames.push_back("D4#");
        zc.binNames.push_back("E4");
        zc.binNames.push_back("F4");
        zc.binNames.push_back("F4#");
        zc.binNames.push_back("G4");
        zc.binNames.push_back("G4#");

        zc.binNames.push_back("A4");// MIDI Note 69
        zc.binNames.push_back("A4#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C5");
        zc.binNames.push_back("C5#");
        zc.binNames.push_back("D5");
        zc.binNames.push_back("D5#");
        zc.binNames.push_back("E5");
        zc.binNames.push_back("F5");
        zc.binNames.push_back("F5#");
        zc.binNames.push_back("G5");
        zc.binNames.push_back("G5#");

        zc.binNames.push_back("A5");// MIDI Note 85
        zc.binNames.push_back("A5#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C6");
        zc.binNames.push_back("C6#");
        zc.binNames.push_back("D6");
        zc.binNames.push_back("D6#");
        zc.binNames.push_back("E6");
        zc.binNames.push_back("F6");
        zc.binNames.push_back("F6#");
        zc.binNames.push_back("G6");
        zc.binNames.push_back("G6#");

        zc.binNames.push_back("A6");// MIDI Note 93
        zc.binNames.push_back("A6#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C7");
        zc.binNames.push_back("C7#");
        zc.binNames.push_back("D7");
        zc.binNames.push_back("D7#");
        zc.binNames.push_back("E7");
        zc.binNames.push_back("F7");
        zc.binNames.push_back("F7#");
        zc.binNames.push_back("G7");
        zc.binNames.push_back("G7#");

        zc.binNames.push_back("A7");// MIDI Note 105
        zc.binNames.push_back("A7#");
        zc.binNames.push_back("B0");
        zc.binNames.push_back("C8");
    }
*/

    list.push_back(zc);

/*    zc.identifier = "Transcriptions";
      zc.name = "Polyphonic Transcription";
      zc.description = "Polyphonic Music Transcription";
      zc.unit = "";
      zc.hasFixedBinCount = true;
      zc.binCount = 0;
      zc.sampleType = OutputDescriptor::VariableSampleRate;
      zc.sampleRate = m_inputSampleRate;
      list.push_back(zc);*/

    return list;
}

Transcription::FeatureSet
Transcription::process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp)
{
    if (m_stepSize == 0) {
	cerr << "ERROR: Transcription::process: "
	     << "Transcription has not been initialised"
	     << endl;
	return FeatureSet();
    }

    if (m_SampleN == 0) {
        m_Base = timestamp;
    }

    if (m_Excess) return FeatureSet();

    for (int i = 0; i < m_blockSize;i++) {

        if (m_SampleN >= m_AllocN) {
            int newsize = m_AllocN * 2;
            if (newsize < 10000) newsize = 10000;
            double *newbuf = (double *)realloc(m_SoundIn, newsize * sizeof(double));
            if (!newbuf) {
                m_Excess = true;
                break;
            }
            m_SoundIn = newbuf;
            m_AllocN = newsize;
        }

        m_SoundIn[m_SampleN]=inputBuffers[0][i];
        m_SampleN=m_SampleN+1;
    }

    return FeatureSet();
}

Transcription::FeatureSet
Transcription::getRemainingFeatures()
{
    FeatureSet returnFeatures;

    double * OutArray;
    double *OutArray2;
    double *hello1;
    double *hello2;
    int Msec;
    int i;
    int j;
    int n;

    Msec=(int)(100*m_SampleN/m_inputSampleRate);

    if (Msec < 100) return returnFeatures;

    OutArray=(double *)malloc(3*3000*sizeof(double));
    OutArray2=(double *)malloc(88*Msec*sizeof(double));
    hello1=(double *)malloc(112*Msec*sizeof(double));
    hello2=(double *)malloc(112*Msec*sizeof(double));
	
    for (j = 0; j <Msec; j++) {

        for(n=0;n<88;n++)
        {

            OutArray2[j*88+n]=0;
        }

    }

    
    Transcribe(Msec,m_SampleN,m_SoundIn,hello1,hello2,OutArray,m_inputSampleRate);
    int start;
    int endd;


    /* for (i = 0; i < 3000; i++) {

       if((OutArray[3*i]>0)&&(OutArray[3*i]<88))
       {
       start=OutArray[3*i+1];endd=OutArray[3*i+2];
       for(j=start;j<(start+0.05);j=j+0.01)
       {
            
       Feature feature;
       Vamp::RealTime ts;

       feature.hasTimestamp = true;
		 
       feature.timestamp =ts.fromSeconds(j);
       feature.values.push_back(OutArray[3*i]+21);
       returnFeatures[0].push_back(feature);

       }


       
       }
       else
       {

       break;
       }


       }

    */

    
    for (i = 0; i < 3000; i++) {

        if((OutArray[3*i]>0)&&(OutArray[3*i]<88))
        {
            start=100*OutArray[3*i+1];
            endd=100*OutArray[3*i+2]-5;
            for(j=start;j<endd;j++)
            {
                n=OutArray[3*i];
                OutArray2[j*88+n]=OutArray[3*i];
            }
       
        }
        else
        {

            break;
        }


    }

    double starts[88];
    for (n = 0; n < 88; ++n) starts[n] = -1.0;

    for (j = 0; j <Msec; j++) {
        
        for(n=0;n<88;n++)
        {
            if(OutArray2[j*88+n]>0)
            {

                if (starts[n] < 0.)
                {
                    starts[n] = j * 0.01;
                }
            }
            else 
            {
                if (starts[n] > 0.)
                {
                    Feature feature;
                    feature.hasTimestamp = true;
                    feature.timestamp = m_Base + Vamp::RealTime::fromSeconds(starts[n]);
                    feature.hasDuration = true;
                    feature.duration = Vamp::RealTime::fromSeconds(j * 0.01 - starts[n]);
                    feature.values.push_back(n+20);
                    returnFeatures[0].push_back(feature);

                    starts[n] = -1.0;
                }
            }
        }
    }

     
    for(n=0;n<88;n++)
    {
        if (starts[n] > 0.)
        {
            Feature feature;
            feature.hasTimestamp = true;
            feature.timestamp = m_Base + Vamp::RealTime::fromSeconds(starts[n]);
            feature.hasDuration = true;
            feature.duration = Vamp::RealTime::fromSeconds(j * 0.01 - starts[n]);
            feature.values.push_back(n+20);
            returnFeatures[0].push_back(feature);

            starts[n] = -1.0;
        }
    }

    free(OutArray2);
    free(OutArray);

    free(hello1);
    free(hello2);

    return returnFeatures;

}





void sofacomplexMex(double *y, double *z, int ncols,double StartNote,double NoteInterval1,double NoteNum,double C,double D,double SR)
{
    int mseconds,i,el,count,count2;
    double  Snote,NoteInterval,NoteN;
    double  *signs;
    double  *rwork;  
    double  freq,R,gain,gainI,gainII,coefI,coefM;
    double output,input,outputI,outputM;
    double *x;
    double *sum,*sum2;
    double power;
    
    //SR=44100;
    Snote=StartNote;
    NoteInterval=NoteInterval1;
    NoteN=NoteNum;
  
    signs=(double*)malloc((int)NoteN*5*sizeof(double));
     
    for (i = 0; i <NoteN; i++) {
           
        freq=exp((log(2.0))*(Snote+i*NoteInterval-69)/12)*440;
        R=exp(-(D+C*freq*2*3.1415926)/(SR*3.1415926)); 
        gain=(1*(sqrt(1+R*R-2*R*cos(2*freq*2*3.1415926/SR)))-1*R*(sqrt(1+R*R-2*R*cos(2*freq*2*3.1415926/SR))))/sin(freq*2*3.1415926/SR);
        gainI=-2*R*cos(freq*2*3.1415926/SR);
        gainII =R*R ;
        coefI=cos(freq*2*3.1415926/SR);
        coefM=sin(freq*2*3.1415926/SR);
            
        signs[i*5+0]=gain*gain;
        signs[i*5+1]=gainI;
        signs[i*5+2]=gainII;
        signs[i*5+3]=coefI;
        signs[i*5+4]=coefM;
            
    }
    
    x=(double*)malloc((int)NoteN*2*sizeof(double));
    rwork=(double*)malloc((int)NoteN*sizeof(double));
    sum=(double*)malloc((int)NoteN*sizeof(double));
    sum2=(double*)malloc((int)NoteN*sizeof(double));
    mseconds=(int)(100*ncols/SR);
    power=0;
    for (i=0;i<mseconds*(int)(SR/100);i++)
    { 
        power=power+y[i]*y[i];
    }
    power=sqrt(power);
    for(i=0;i<NoteN*2;i++)
        x[i]=0;
    for (i=0;i<NoteN;i++)
    {
        sum[i]=0;
        sum2[i]=0;
    };
    count=0;
    count2=0;
    for (i=0;i<(mseconds*(int)(SR/100));i++)
    {    
        count=count+1;
        input=y[i];
        for(el=0;el<NoteN;el++)
        { 
            output=(input-signs[5*el+1]*x[2*el+0]-signs[5*el+2]*x[2*el+1]);
            outputI=output-signs[5*el+3]*x[2*el+0];
            outputM=signs[5*el+4]*x[2*el+0];
            sum[el]=sum[el]+signs[5*el+0]*(outputI*outputI+ outputM*outputM);
            rwork[el]=output;
            x[el+el+1]=x[el+el+0];
            x[el+el+0]=rwork[el];
                   
        }
        if(count==(int)(SR/100))
        {
            for(el=0;el<NoteN;el++)
            {
                *(z+count2*(int)NoteN+el)=1000000*(sum[el]+sum2[el])/(2*(int)(SR/100))+0.00001;
                sum2[el]=sum[el];
                sum[el]=0;
            }                 
            count2=count2+1;
            count=0;
        }
       
    }    
    for (i=0;i<NoteN;i++)
    {
        sum[i]=0;
        sum2[i]=0;
    };
    for (el=0;el<NoteN;el++)
    {  
        for (i=0;i<mseconds;i++)
        {
            sum[el]=sum[el]+*(z+i*(int)NoteN+el);
        }       
            
    }
         
    free(x);
    free(rwork);
    free(sum);
    free(sum2);
    free(signs);
         
}
       
void FindMaxN( double *InputArray, int InputLen,int MaxOrder)
{
    int i,j,MaxIndex = 0;
    double MaxValue; 
    double *In2;
    
    In2=(double*)malloc(InputLen*sizeof(double));
    for (i=0;i<InputLen;i++)
    {
        In2[i]=InputArray[i];
        InputArray[i]=0;
    }
    for (i=0;i<MaxOrder;i++)
    {
        MaxValue=0;
        for (j=0;j<InputLen;j++)
        {
            if(In2[j]>MaxValue)
            {
                MaxValue=In2[j];
                MaxIndex=j;
            }
        }
        InputArray[MaxIndex]=In2[MaxIndex];
        In2[MaxIndex]=0;        
    }
    
    free(In2);
}

double SumF(double *InputArray,int Start, int End)
{
    double Value;
    int i;
    Value=0;
    for (i=Start;i<(End+1);i++)
    {
        Value=Value+InputArray[i];
    }
    
    return Value;
    
}

int round10(int x) 
{       
    int I,I2;
    I=((int)(x/10));
    I2=x-I*10;
    
    if(I2>5)
        return (I+1);
    else
        return I;
  
}
 

void ConToPitch1250(double *In, int InLen)
{
    int i,j,k, nn,col;
    double *Out;
    const int A[12]={0, 120, 190, 240, 279, 310, 337, 360, 380, 399, 415, 430};
    Out=(double*)malloc(InLen*sizeof(double));

	   
    col=InLen;

    for (i=0;i<col;i++)
    {
        Out[i]=0;
    }

    for (i=0;i<col;i++)
    {
        k=0;
        nn=5;
        for (j=0;j<nn;j++)
        {
            if((i+A[j])<col)
            {
                k=k+1;
                Out[i]=Out[i]+In[i+A[j]]; 
            }
                    
            if((i+A[j])>(col-1))
            {
                k=k+1;
                Out[i]=Out[i]+In[col-1];    
            }
        }
        if(k>0)
        {
            Out[i]=Out[i]/k;
        }
    }
    for (i=0;i<col;i++)
    {
        In[i]=Out[i];  
    }
    
    
    free(Out);
}

void Norm1(double *In, int InLen)
{
    double MaxValue;
    int i;
    double *Out;
    Out=(double*)malloc(InLen*sizeof(double));
    
    MaxValue=In[0];
    for (i=1;i<InLen;i++)
    {
        if(In[i]>MaxValue)
            MaxValue=In[i];
    }
    
    for (i=0;i<InLen;i++)
    {
        Out[i]=In[i]-MaxValue;
    }
    
    for (i=0;i<InLen;i++)
    {
        In[i]=Out[i];
    }
    
    free(Out);
}

void Smooth(double *In, int InLen,int smoothLen)
{
    double sum;
    int i,j,nn,n,count;
    double *Out;
    Out=(double*)malloc(InLen*sizeof(double));
    nn=InLen;
    n=(smoothLen-1)/2;
    for (i=0;i<nn;i++)
    {
        sum=0;
        count=0;
        for (j=0;j<(n+1);j++)
        {
            if ((i-j)>-1)
            {
                sum=sum+In[i-j];
                count=count+1;
            }
        }
  
        for (j=1;j<(n+1);j++)
        {
            if ((i+j)<nn)
            {
                sum=sum+In[i+j];
                count=count+1;
            }
        }
        Out[i]=sum/count; 
    }
    for (i=0;i<InLen;i++)
        In[i]=Out[i];
   
    free(Out);
}


void FindPeaks(double *In, int InLen,double *Out1,double *Out2, int /* db */, int db2, int db3)
{
    int i,lastout;
    for (i=0;i<InLen;i++)
    {
        Out1[i]=0; 
        Out2[1]=0;   
    }

    for (i=20;i<(InLen-20-1);i++)
    {
        if( /**/ ((In[i]>(db2+In[i-6]))||(In[i]>(db2+In[i+6]))
                  ||(In[i]>(db3+In[i+20]))||(In[i]>(db3+In[i-20])))
            /*&&(In[i]>db)*/&&(In[i]>In[i+3])&&(In[i]>In[i-3])
            &&(In[i]>In[i+2])&&(In[i]>In[i-2])
            &&(In[i]>In[i+1])&&(In[i]>In[i-1]))
        {
            Out1[i]=In[i];
            Out2[i]=1;
        }
    
    }

    lastout=1;
    for(i=0;i<InLen;i++)
    {
    
        if(Out2[i]==1)
        {
            if((i-lastout)<5)
            {
                if(Out1[i]>Out1[lastout])
                {
                    Out2[lastout]=0;
                    Out1[lastout]=0;
                    lastout=i;
                }
                else
                {
                    Out2[i]=0;
                    Out1[i]=0;
                }
                 
            }
            else
            {
                lastout=i;
            }
        }
       
    }
    
}


void ConFrom1050To960(double *In, double *out, int InputLen)
{
    int i,j;

    for(i=0;i<960;i++)
    {
        for (j=0;j<InputLen;j++)
        {
            out[i+j*960]=In[i+j*1050];

        }
    }
	 

}

void Move( double *InputArray, int InputLen,int m)
{
    int i;
    double *OutArray;
    
    OutArray=(double *)malloc(InputLen*sizeof(double));
    for (i=0;i<InputLen;i++)
        OutArray[i]=0;
    
    for (i=0;i<InputLen;i++)
    {  if(((i+m)>-1)&&((i+m)<InputLen))
            OutArray[i+m]=InputArray[i];
    }
    
    for (i=0;i<InputLen;i++)
    {  
        InputArray[i]=OutArray[i];
    }
    
    free(OutArray);
}


double SumArray( double *InputArray, int InputHLen, int InputVLen)
{
    int i;
    int j;
    double sum;
    int count;
    count=0;
    sum=0;
    for (j=0;j<InputHLen;j++)
    {
        for (i=0;i<InputVLen;i++)
        {  
            count=count+1;
            sum=sum+InputArray[i+j*InputVLen];
               
        }
    }
    return sum;          
}

double Sum( double *InputArray, int InputHLen)
{
    int i;
    double sum;
    int count;
    count=0;
    sum=0;
    for (i=0;i<InputHLen;i++)
    {  
        count=count+1;
        sum=sum+InputArray[i];
               
    }
    return sum;          
}

void MeanV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double sum;
    for (i=0;i<InputVLen;i++)
    {  
        sum=0;   
        for (j=0;j<InputHLen;j++)
        {
                 
            sum=sum+InputArray[i+j*InputVLen];
               
        }
        OutArray[i]=sum/InputHLen;
    }
                  
}

void SumV( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double sum;
    
    for (j=0;j<InputHLen;j++)
    {
        sum=0;
        for (i=0;i<InputVLen;i++)
        {  
            
            sum=sum+InputArray[i+j*InputVLen];
               
        }
        OutArray[j]=sum;
    }
                  
}



void SumV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double sum;
    for (i=0;i<InputVLen;i++)
    {  
        sum=0;   
        for (j=0;j<InputHLen;j++)
        {
                 
            sum=sum+InputArray[i+j*InputVLen];
               
        }
        OutArray[i]=sum;
    }
                  
}

void MaxV( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double MaxVal;
    
    for (j=0;j<InputHLen;j++)
    {
        MaxVal=InputArray[j*InputVLen];
        for (i=0;i<InputVLen;i++)
        {  
            if(InputArray[i+j*InputVLen]>MaxVal)
            {
                MaxVal=InputArray[i+j*InputVLen];
            }
               
        }
        OutArray[j]=MaxVal;
    }
                  
}

void MaxV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double MaxVal;
    for (i=0;i<InputVLen;i++)
    {  
        MaxVal=InputArray[i];   
        for (j=0;j<InputHLen;j++)
        {
            if(InputArray[i+j*InputVLen]>MaxVal)
            {
                MaxVal=InputArray[i+j*InputVLen];
            }
               
        }
        OutArray[i]=MaxVal;
    }
                  
}



void MinArray( double *InputArray, int InputHLen, int InputVLen, double MinValue)
{
    int i;
    int j;
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            if(InputArray[i+j*InputVLen]<MinValue)
                InputArray[i+j*InputVLen]=MinValue;
            
        }
              
    }
    
}
        
  
void MaxArray( double *InputArray, int InputHLen, int InputVLen, double MaxValue)
{
    int i;
    int j;
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            if(InputArray[i+j*InputVLen]>MaxValue)
                InputArray[i+j*InputVLen]=MaxValue;
            
        }
              
    }
    
}
          
double  GetMaxValue( double *InputArray, int InputHLen, int InputVLen)
{
    int i;
    int j;
    
    double MaxValue;
    MaxValue=InputArray[0];
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            if(InputArray[i*InputHLen+j]>MaxValue)
                MaxValue=InputArray[i*InputHLen+j];
            
        }
              
    }
      
    return MaxValue;
}

void RemoveNoise( double *InputArray, int InputHLen, int InputVLen)
{
    int i;
    int j;
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            
            InputArray[i+j*InputVLen]=InputArray[i+j*InputVLen]-EualCurve960[i];
            
        }
              
    }
      
}
double MeanArray( double *InputArray, int InputHLen, int InputVLen)
{
    int i;
    int j;
    double sum;
    int count;
    count=0;
    sum=0;
    for (j=0;j<InputHLen;j++)
    {
        for (i=0;i<InputVLen;i++)
        {  
            count=count+1;
            sum=sum+InputArray[i+j*InputVLen];
               
        }
    }
    return sum/count;          
}
void Mydiff( double *InputArray, int InputHLen, int InputVLen,int n)
{
    int i;
    int j;
    double * OutArray;
    
    OutArray=(double*)malloc(InputHLen*InputVLen*sizeof(double));
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=n;j<InputHLen;j++)
        {
            OutArray[i+j*InputVLen]=InputArray[i+j*InputVLen]-InputArray[i+(j-n)*InputVLen];
            
        }  
    }
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=n;j<InputHLen;j++)
        {
            InputArray[i+j*InputVLen]=OutArray[i+j*InputVLen];
            
        }  
    }
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<n;j++)
        {
            InputArray[i+j*InputVLen]=0;
            
        }  
    }
    
    free(OutArray);
}

void PeakDetect(double *In, int InLen)
{
    int i;
    double *Out1;
 
    Out1=(double*)malloc(InLen*sizeof(double));
    for (i=0;i<InLen;i++)
    {
        Out1[i]=0;   
    }

 
 
    for (i=2;i<(InLen-3);i++)
    {
        if( (In[i]>In[i+2])&&(In[i]>In[i-2])
            &&(In[i]>In[i+1])&&(In[i]>In[i-1]))
        {
            Out1[i]=In[i];
        }
    
    }

    for(i=0;i<InLen;i++)
    {
     
        In[i]=Out1[i]; 
    }
 
    free(Out1);
}
void MeanV( double *InputArray, int InputHLen, int InputVLen, double *OutArray)
{
    int i;
    int j;
    double sum;
    
    for (j=0;j<InputHLen;j++)
    {
        sum=0;
        for (i=0;i<InputVLen;i++)
        {  
            
            sum=sum+InputArray[i+j*InputVLen];
               
        }
        OutArray[j]=sum/InputVLen;
    }
                  
}
void Edetect(double *InputArray, int InputHLen, int InputVLen, double MinT, double db1,double *OutOne)
{
    int i;
    int j;
    double MaxValue;
  
//    printf(" Starting Energy Onset Detection.. %f\n",db1);
    RemoveNoise(InputArray, InputHLen, InputVLen);
    
    
    MaxValue=GetMaxValue(InputArray,InputHLen,InputVLen);
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            InputArray[i*InputHLen+j]=InputArray[i*InputHLen+j]-MaxValue;
            
        }
              
    }
    
    MinArray(InputArray, InputHLen, InputVLen, -100);
    Mydiff(InputArray, InputHLen, InputVLen,3);
    MinArray(InputArray, InputHLen, InputVLen, MinT);
   
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            InputArray[i*InputHLen+j]=InputArray[i*InputHLen+j]-MinT;
            
        }
              
    }
    
    MeanV(InputArray,InputHLen,InputVLen,OutOne);
    Smooth(OutOne, InputHLen,3);
    Smooth(OutOne, InputHLen,3);
    Move(OutOne,InputHLen,-2);
    PeakDetect(OutOne,InputHLen);
    MinArray(OutOne, InputHLen,1, db1);
    
    for (j=0;j<InputHLen;j++)
    {
        OutOne[j]=OutOne[j]-db1;
            
    }
}



void OnsetDetection2(double *In,int InputLen,double *OutOne,double a,double b)
{
    int mseconds;
    double *Input;
    

    mseconds=InputLen;

    Input=(double *)malloc(mseconds*960*sizeof(double));
     
    ConFrom1050To960(In,Input,InputLen);

    
 

    if(a>0)
    {
        Edetect(Input,mseconds,960, a,b,OutOne);
    }


    free(Input);   

}

void PitchEstimation(double *In, int /* InLen */, double *OutArray,double *OutArray2)
{
    double *xx,*x,*y,*y1,*PeakPitch1, *PeakPitch2,*PeakInput1, *PeakInput2;
    double *out,*outValue;
    double *output,*output1;
    int *outc;
    double temp;
    int i,sumI;
    int Len;
 
    Len=1050;
    xx=(double*)malloc(Len*sizeof(double));
    x=(double*)malloc(Len*sizeof(double));
    y=(double*)malloc(Len*sizeof(double));
    y1=(double*)malloc(Len*sizeof(double));
    PeakPitch1=(double*)malloc(Len*sizeof(double));
    PeakPitch2=(double*)malloc(Len*sizeof(double));
    PeakInput1=(double*)malloc(Len*sizeof(double));
    PeakInput2=(double*)malloc(Len*sizeof(double));
    out=(double*)malloc(Len*sizeof(double));
    outValue=(double*)malloc(Len*sizeof(double));
    output=(double*)malloc(112*sizeof(double));
    output1=(double*)malloc(112*sizeof(double));
    outc=(int*)malloc(112*sizeof(int)); 
// yI=(double*)malloc(12*sizeof(double));
 
 
    for (i=0;i<Len;i++)
    {
        x[i]=In[i]; 
    }

    for (i=0;i<Len;i++)
    {
        y1[i]=x[i];   
    }

    ConToPitch1250(y1,Len);

    for (i=0;i<Len;i++)
    {
        y[i]=y1[i];   
    }

    Smooth(y,Len,30);

    for (i=0;i<Len;i++)
    {  
        y1[i]=y1[i]-y[i];   
    }

    for (i=0;i<Len;i++)
    {  
        y1[i]=y1[i]+20;   
    }

    temp=0;
    for (i=0;i<Len;i++)
    {
        temp=temp+x[i];
    }
    temp=temp/Len;
    for (i=0;i<Len;i++)
    {
        y[i]=x[i]-temp;
    }


    for (i=0;i<Len;i++)
    {  
        PeakPitch2[i]=0;
        PeakPitch1[i]=0;
        PeakInput1[i]=0;
        PeakInput2[i]=0;
    }
    FindPeaks(y1, Len,PeakPitch1,PeakPitch2, 0, -1000, -1000);
    FindPeaks(y, Len,PeakInput1,PeakInput2, 0, 6, 15);



    sumI=0;
    for (i=0;i<Len;i++)
    {  
        sumI=sumI+PeakPitch2[i];
    }


    if (sumI>12)
    {
        FindMaxN(PeakPitch1,Len,12);
    
        for (i=0;i<Len;i++)
        {
            if(PeakPitch1[i]==0)
            {
                PeakPitch2[i]=0;
            } 
        }
    
    }

    for (i=0;i<Len;i++)
    {  
        out[i]=0;
        outValue[i]=0;
    }

    for (i=0;i<(Len-300);i++)
    {
        if(PeakPitch2[i]==1)
 
        {
            if (
                ((SumF(PeakInput2,i-4,i+4)>0)&&(SumF(PeakInput2,i+120-4,i+120+4)>0))
                ||((SumF(PeakInput2,i-4,i+4)>0)&&(SumF(PeakInput2,i+190-4,i+190+4)>0))
                ||((SumF(PeakInput2,i+190-4,i+190+4)>0)&&(SumF(PeakInput2,i+120-4,i+120+4)>0))
                )            
            {
                out[i]=1;
                outValue[i]=y1[i];
              
            }
        }
    }

    for (i=0;i<112;i++)
    {  
        output[i]=0;
        outc[i]=0;
    }



    for (i=0;i<Len;i++)
    {
        if(out[i]==1)
        {
            output[20+round10(i+1)-1]=1;
            outc[20+round10(i+1)-1]=i;
        }
    }


    for (i=0;i<112;i++)
    {  
        output1[i]=output[i];
    }



    for (i=20;i<(112-28);i++) 
    {
        if((output[i]>0)&&(SumF(PeakInput2,outc[i]-5,outc[i]+5)==0))
        {
            output1[i]=0;
        }
    }


    for (i=0;i<112;i++)
    {
        OutArray[i]=0;
        OutArray2[i]=0;
     
    }

    for(i=20;i<105;i++)
    {
        if(output1[i]==1)
        {
            OutArray[i]=outc[i]+200+2;
            OutArray2[i]=y[outc[i]];
        } 
    }

    free(xx); // xx=(double*)malloc(Len*sizeof(double));
    free(x); // x=(double*)malloc(Len*sizeof(double));
    free(y); // y=(double*)malloc(Len*sizeof(double));
    free(y1);  // y1=(double*)malloc(Len*sizeof(double));
    free(PeakPitch1); //=(double*)malloc(Len*sizeof(double));
    free(PeakPitch2); //=(double*)malloc(Len*sizeof(double));
    free(PeakInput1); //=(double*)malloc(Len*sizeof(double));
    free(PeakInput2); //=(double*)malloc(Len*sizeof(double));
    free(out); //=(double*)malloc(Len*sizeof(double));
    free(outValue); //=(double*)malloc(Len*sizeof(double));
    free(output); //=(double*)malloc(112*sizeof(double));
    free(output1); //=(double*)malloc(112*sizeof(double));
    free(outc); //=(double*)malloc(112*sizeof(int)); 
//free(yI); //=(double*)malloc(12*sizeof(int));
//  printf(" end free \n");
}

void DoMultiPitch(double *In, int RLen,int CLen, double *Out1, double *Out2)
{
  
    int i, j;
    double *sum1,*mean1;
    double MaxV;
    double *OutArray1, *OutArray2,*tempArray;
 
    OutArray1=(double *)malloc(112*sizeof(double));
    OutArray2=(double *)malloc(112*sizeof(double));
    tempArray=(double *)malloc(RLen*sizeof(double));
 
    sum1=(double*)malloc(CLen*sizeof(double));
    mean1=(double*)malloc(CLen*sizeof(double));
 
    for (j=0;j<CLen;j++)
    {
        sum1[j]=0;
        for (i=0;i<RLen;i++)
        {
            sum1[j]=sum1[j]+In[j*RLen+i];
        }  
     
        mean1[j]=sum1[j]/CLen;
    }
    MaxV=mean1[0];
    for (j=0;j<CLen;j++)
    {
        if(mean1[j]>MaxV)
        {
            MaxV=mean1[j];
        }
    }
   
    for (j=0;j<CLen;j++)
    {   
        mean1[j]=mean1[j]-MaxV;
    }  
 
  
    for (j=0;j<CLen;j++)
    {
      
        for (i=0;i<112;i++)
        {
        
            OutArray1[i]=0;
            OutArray2[i]=0;;
        
        } 
        MaxV=In[j*RLen];
        for(i=0;i<RLen;i++)
        {
            tempArray[i]=In[j*RLen+i]; 
            if(tempArray[i]>MaxV)
                MaxV=tempArray[i];
        }
      
        if(mean1[j]>-55)
        {
     
            PitchEstimation(tempArray,RLen,OutArray1,OutArray2);  
     
            for(i=0;i<112;i++)
            {
                if(OutArray1[i]>0)
                {
                    if((MaxV-tempArray[(int)OutArray1[i]-201-1])>40)
                    {
                        OutArray1[i]=0;
                        OutArray2[i]=0;
                    }
           
                }
            }
           
        }
    
        for (i=0;i<112;i++)
        {
        
            Out1[j*112+i]=OutArray1[i];
            Out2[j*112+i]=OutArray2[i];
        
        }
    
    }  

    free(OutArray1);
    free(OutArray2);
    free(tempArray);
    free(sum1);
    free(mean1);
}


int OnsetToArray(double *In, int Len, double *OutStart,double *OutEnd)
{
    int count,i;
  
    count=0;
  
    for (i=0;i<Len;i++)
    {     
        if(In[i]>0)
        {  
            OutStart[count]=i+1;
            if(count>0)
            {
                OutEnd[count-1]=i+1;
            }  
            count=count+1; 
        }
    }
    if (count>0)
    {
        OutEnd[count-1]=Len;
    }
    return count;
   
}
void dbfunction( double *InputArray, int InputHLen, int InputVLen,double *OutArray)
{
    int i;
    int j;
    
    for (i=0;i<InputVLen;i++)
    {
        for (j=0;j<InputHLen;j++)
        {
            OutArray[i*InputHLen+j]=20*log10(InputArray[i*InputHLen+j]);
            
        }
              
    }
}    

void Transcribe(int Len,int inputLen,double *SoundIn,double *out,double *outArray2,double *outArray3,double SampleRate)
{
    int OnsetN;
    int i,j,k;
    int count;
    int index;
    double *OutStart,*OutEnd;
    int start,endd,startb=1,start2,endd2;
    double *A1,*A2,*A3,*A4,*A5,*A6,*D,*D2;
    double *A6A;
    double *out2, *PitchOut1,*PitchOut2,*PitchOut3;
    double sum,maxV,maxVal;
    double *tempArray;
    double temp;
    double p;
    double M1,M2;
    double *In;
    int Len2;
    double *dbs,*ss,*dbs1,*jj;
    int TempInt;
  
    
    A1=(double *)malloc(112*sizeof(double));
    A2=(double *)malloc(112*sizeof(double));
    A3=(double *)malloc(112*sizeof(double));
    A4=(double *)malloc(112*sizeof(double));
    A5=(double *)malloc(112*sizeof(double));
    A6=(double *)malloc(112*sizeof(double));
    D=(double *)malloc(112*sizeof(double));
    D2=(double *)malloc(112*sizeof(double));
    PitchOut1=(double *)malloc(112*Len*sizeof(double));
    PitchOut2=(double *)malloc(112*Len*sizeof(double));
    PitchOut3=(double *)malloc(112*Len*sizeof(double));
    OutStart=(double *)malloc(Len*sizeof(double));
    OutEnd=(double *)malloc(Len*sizeof(double));
    tempArray=(double *)malloc(sizeof(double)*Len);
    In=(double *)malloc(sizeof(double)*Len);
    dbs=(double *)malloc(1050*sizeof(double)*Len);
    dbs1=(double *)malloc(210*sizeof(double)*Len);
    ss=(double *)malloc(210*sizeof(double)*Len);
    jj=(double *)malloc(1050*sizeof(double));
    
    
    
    
    for(k=0;k<1050;k++)
    {
        
        jj[k]=k/5.0; 
        
    }
    
    sofacomplexMex(SoundIn,ss,inputLen,20,0.5,210,0.03,20,SampleRate);
    dbfunction(ss, Len, 210,dbs1);
   
    for(i=0;i<Len;i++)
    {
        for(k=0;k<1045;k++)
        {
            TempInt=(int)jj[k];
            dbs[k+i*1050]=(jj[k]-TempInt)*dbs1[TempInt+1+i*210]+(TempInt+1-jj[k])*dbs1[TempInt+i*210];   
         
        }
     
        for (k=1045;k<1050;k++)
        {
            dbs[k+i*1050]=dbs[1044+i*1050];  
        }
     
    }
      
    OnsetDetection2(dbs,Len,In,3,1.2);
    for (i=0;i<Len;i++)
    {
        outArray2[i]=In[i]; 
         
    }
    
    OnsetN=0;
    count=0;
    for (i=0;i<Len;i++)
    {
        if(In[i]>0)
        {
            OnsetN=OnsetN+1;
            count=count+1;
        }
    }
    Len2=count;
    out2=(double *)malloc(112*Len2*sizeof(double));
    A6A=(double *)malloc(112*Len2*sizeof(double));
    OnsetToArray(In,Len,OutStart,OutEnd); 
    DoMultiPitch(dbs,1050,Len, PitchOut1, PitchOut2);

    
    for (i=0;i<Len;i++)
    {
        for (j=0;j<112;j++)
        {  
            PitchOut3[i*112+j]=PitchOut1[i*112+j];
            if(PitchOut3[i*112+j]>1)
                PitchOut3[i*112+j]=1;
        }
        
    }
    

    for (i=0;i<OnsetN;i++)
    {  
        for(j=0;j<112;j++)
        {
            A1[j]=0;A2[j]=0;A3[j]=0;A4[j]=0;A5[j]=0;A6[j]=0; 
            
        }
        maxV=0;
        start=(int)OutStart[i];
        endd=(int)OutEnd[i];
        if(i>0)
        {
            startb=(int)OutStart[i-1];
        } 
        
        for (j=0;j<112;j++)
        {
            sum=0;
            count=0;
            for (k=(start-1);k<endd;k++)
            {
                sum=sum+PitchOut3[k*112+j];
                count=count+1;
            }
           
            A1[j]=sum;
            A6[j]=sum/count;
            A6A[i*112+j]=sum/count;
              
        }
        
        for (j=0;j<112;j++)
        {
            maxVal=PitchOut2[start*112+j];
            for (k=(start-1);k<endd;k++)
            {
                if(PitchOut2[k*112+j]>maxVal)
                {
                    maxVal=PitchOut2[k*112+j];
                }
        
            }
           
            A3[j]=maxVal;
              
        }
        
        for (j=0;j<112;j++)
        {
            sum=0;
            count=0;
            for (k=(start-1);k<endd;k++)
            {
                if(PitchOut2[k*112+j]>0)
                {
                    sum=sum+PitchOut2[k*112+j];
                    count=count+1;
                }
            }
            if(count>0)
                A4[j]=sum/count;
            else 
                A4[j]=0;
        }
           
          
        for (j=0;j<112;j++)
        {
            sum=0;
            count=0;
            for (k=(start-1);k<endd;k++)
            {
                if(PitchOut1[k*112+j]>0)
                {
                    sum=sum+PitchOut1[k*112+j];
                    count=count+1;
                }
            }
            if(count>0)
                A5[j]=sum/count;
            else 
                A5[j]=0;
        }
        
        maxV=A3[0];
        for (j=0;j<112;j++)
        {
            if(A3[j]>maxV)
                maxV=A3[j];
        }
       
        for (j=0;j<112;j++)
        {
           
            if(A1[j]>0)
            {
                D[j]=A1[j];D2[j]=A1[j];
            }

            else
            {
                D[j]=A1[j];D2[j]=A1[j];
            }
        }    
        
        for (j=0;j<112;j++)
        {
            if(A1[j]<8)
            {
                D[j]=0;D2[j]=0;             
            }
            
        }
        
        for(j=0;j<112;j++)
        {
          
            if ((j>12)&&(D[j]>0)&&(D[j-12]>0))
            {
                D[j]=0;  D2[j]=0;   
                if((A3[j]>45)&&(A3[j]>(A3[j-12]+3)))
                {
                    D[j]=1;    
                } 
            }
            
            
            if ((j>19)&&(D[j]>0)&&(D[j-19]>0))
            {
             
                D[j]=0;    D2[j]=0;            
                if((A3[j]>50))  
                {
                    D[j]=1;    
                }  
            }   
         
            if ((j>24)&&(D[j]>0)&&(D[j-24]>0))
            {
             
                D[j]=0;          D2[j]=0;  
                if((A3[j]>50))
                {
                    D[j]=1;    
                }
            }  
       
            if ((j>28)&&(D[j]>0)&&(D[j-28]>0))
            {
               
                D[j]=0;           D2[j]=0;    
                if((A3[j]>50))
                {
                    D[j]=1;    
                } 
            }     
       
            if ((j>34)&&(fabs(A5[j]-337.0-A5[j-34])<3.0)&&(D[j]>0)&&(D[j-34]>0))
            {
                     
                D[j]=0;           D2[j]=0;         
                if((A4[j]>25)&&(A3[j]>40)&&(A3[j]>(A3[j-34]-3))&&((A1[j]>8)||(A6[j]>0.8)))
                {
                    D[j]=1;    
                }      
            }  
    
            if((j>48)&&(j<59)&&(A3[j]<20))
            {
                D[j]=0; 
            }

            if((j>58)&&(j<69)&&(A3[j]<28))
            {
                D[j]=0; 
            }


            if((j>68)&&(j<79)&&(A3[j]<40))
            {
                D[j]=0; 
            }
         
            if((j>78)&&(A3[j]<50))
            {
                D[j]=0; 
            }
         
            if((j>85)&&(A3[j]<55))
            {
                D[j]=0; 
            }
         
            if((D2[j]>0)&&(A1[j]>15))
            {
                D[j]=1; 
            }
            if(i>1)
            {

                for (k=(startb-1);k<start;k++)
                {
                    tempArray[k-startb+1]=PitchOut3[j+k*112]; 
                  
                }
                temp=Sum(tempArray,start-startb+1);
                if(((maxV-A3[j])>20)&&(temp>3))
                {
                    D[j]=0;
                }
            
            }
                
        }
        
        for(j=0;j<112;j++)
        {    
            out[j+i*112]=D[j]; 
            out2[j+i*112]=D[j];
        }   
    }
   
    for (i=1;i<OnsetN;i++)
    {
        start2=(int)OutStart[i];
        endd2=(int)OutEnd[i];
              
        for (j=0;j<112;j++)
        {
            sum=0;
            count=0;
            for (k=(start2-1);k<endd2;k++)
            {
                sum=sum+PitchOut3[k*112+j];
                count=count+1;
            }
           
            A1[j]=sum;
        }
       
        for (j=0;j<112;j++)
        {               
            if((out2[(i-1)*112+j]>0)&&(out[j+i*112]>0))
            {
                out[j+i*112]=0;
                sum=0;
                for(k=(start2-1);k<endd2;k++)
                {
                    sum=sum+PitchOut1[j+k*112];
                 
                }    
                p=sum/A1[j];
              
                index=(int)(p+0.5)-200; 
              
                if((index>0)&&(i<(OnsetN-1))&&(start2>5))
                {
                  
                    M1=dbs[index+(start2-1)*1050];
                    for (k=(start2-1);k<(start2+10);k++)
                    {
                        if(dbs[index+k*1050]>M1)
                            M1=dbs[index+k*1050];
                      
                    }
                  
                    M2=dbs[index+(start2-5-1)*1050];
                    for (k=(start2-5-1);k<start2;k++)
                    {
                        if(dbs[index+k*1050]<M2)
                            M2=dbs[index+k*1050];
                      
                    }
                  
                    if((M1-M2)>10)
                    {
                        out[j+i*112]=1;
                    }
                }
            }
        }
    }
    
    count=0;
    for (i=0;i<OnsetN;i++)
    {  
        
        start=(int)OutStart[i];
        endd=(int)OutEnd[i];
        
        for(j=0;j<112;j++)
        {
            if(out[j+i*112]>0)
            {
                outArray3[count*3+0]=j+1-21;//exp((log(2.0))*(j+1-69)/12)*440;
                outArray3[count*3+1]=start*0.01;
            
                if(i==(OnsetN-1))
                {
                    outArray3[count*3+2]=0.01*OutEnd[i];
                }  
                else
                {
            
                    for(k=(i+1);k<OnsetN;k++)
                    {
                
                        if(k==(OnsetN-1))
                        {
                            outArray3[count*3+2]=0.01*OutEnd[k];
                        }  
				   
                        if(out[j+k*112]>0)
                        {
                            outArray3[count*3+2]=0.01*OutStart[k];
                            break;  
                        }
                 
                        if(A6A[k*112+j]<0.5)
                        {
                            outArray3[count*3+2]=0.01*OutStart[k];
                            break;   
                     
                        }
                
                    }
               
                }
   
                count=count+1;
            }
            
        }
        
    }
    outArray3[count*3+0]=0;
    outArray3[count*3+1]=0;
    outArray3[count*3+2]=0;
       
    free(tempArray);
    free(OutStart);
    free(OutEnd);
    free(A1);
    free(A2);
    free(A3);
    free(A4);
    free(A5);
    free(A6);
    free(A6A);
    free(D);
    free(D2);
    free(out2);
    free(PitchOut1);
    free(PitchOut2);
    free(PitchOut3);
    free(In);
    free(dbs);
    free(dbs1);
    free(ss);
    free(jj);
}

