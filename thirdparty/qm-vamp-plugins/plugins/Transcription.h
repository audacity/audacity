/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

#ifndef _TRANSCRIPTION_PLUGIN_H_
#define _TRANSCRIPTION_PLUGIN_H_

#include <vamp-sdk/Plugin.h>

class Transcription : public Vamp::Plugin
{
public:
    Transcription(float inputSampleRate);
    virtual ~Transcription();

    bool initialise(size_t channels, size_t stepSize, size_t blockSize);
    void reset();

    InputDomain getInputDomain() const { return TimeDomain; }

    std::string getIdentifier() const;
    std::string getName() const;
    std::string getDescription() const;
    std::string getMaker() const;
    int getPluginVersion() const;
    std::string getCopyright() const;
	  size_t getPreferredStepSize() const;
    size_t getPreferredBlockSize() const;

    OutputList getOutputDescriptors() const;

    FeatureSet process(const float *const *inputBuffers,
                       Vamp::RealTime timestamp);

    FeatureSet getRemainingFeatures();

protected:
    int m_stepSize;
    int m_blockSize;
    double * m_SoundIn;
    int m_SampleN;
    int m_AllocN;
    bool m_Excess;
    Vamp::RealTime m_Base;
/*
 void sofacomplexMex(double *y, double *z, int ncols,double StartNote,double NoteInterval1,double NoteNum,double C,double D);
 void FindMaxN( double *InputArray, int InputLen,int MaxOrder);
 double SumF(double *InputArray,int Start, int End);
 int round10(int x) ;
 void ConToPitch1250(double *In, int InLen);
 void Norm1(double *In, int InLen);
 void Smooth(double *In, int InLen,int smoothLen);
 void FindPeaks(double *In, int InLen,double *Out1,double *Out2, int db, int db2, int db3);
 void ConFrom1050To960(double *In, double *out, int InputLen);
 void Move( double *InputArray, int InputLen,int m);
 double SumArray( double *InputArray, int InputHLen, int InputVLen);
 double Sum( double *InputArray, int InputHLen);
 void MeanV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
 void SumV( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
 void SumV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
 void MaxV( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
 void MaxV2( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
 void MinArray( double *InputArray, int InputHLen, int InputVLen, double MinValue);
  void MaxArray( double *InputArray, int InputHLen, int InputVLen, double MaxValue);
 double  GetMaxValue( double *InputArray, int InputHLen, int InputVLen);
 void RemoveNoise( double *InputArray, int InputHLen, int InputVLen);
 double MeanArray( double *InputArray, int InputHLen, int InputVLen);
 void Mydiff( double *InputArray, int InputHLen, int InputVLen,int n);
 void PeakDetect(double *In, int InLen);
 void MeanV( double *InputArray, int InputHLen, int InputVLen, double *OutArray);
void Edetect(double *InputArray, int InputHLen, int InputVLen, double MinT, double db1,double *OutOne);
void OnsetDetection2(double *In,int InputLen,double *OutOne,double a,double b);
void PitchEstimation(double *In, int InLen, double *OutArray,double *OutArray2);
void DoMultiPitch(double *In, int RLen,int CLen, double *Out1, double *Out2);
int OnsetToArray(double *In, int Len, double *OutStart,double *OutEnd);
void dbfunction( double *InputArray, int InputHLen, int InputVLen,double *OutArray);

void Transcribe(int Len,int inputLen,double *SoundIn,double *out,double *outArray2,double *outArray3);*/

};


#endif
