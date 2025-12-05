//
//  TF_Calculator.h
//  Audacity
//
//  Created by Klaus Gram-Hansen on 01/12/2025.
//

#ifndef TF_Calculator_h
#define TF_Calculator_h

#include "TimeFrequencyCalculator.h"
#include "SampleCount.h"
#include <memory>
using namespace std;

class audacityTimeFrequencyCalculator
{
public:
   static unique_ptr<audacityTimeFrequencyCalculator> createWaveletCalculator(double q, double tf_overlap, double fmax, unsigned int nb_octaves);
   static unique_ptr<audacityTimeFrequencyCalculator> createStftCalculator(size_t windowLength, size_t transformLength, const float * pWindow);

   void prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost);
   void transform(const float * pSamples, unsigned int nSamples, unsigned int nPre, unsigned int nPost);
   size_t getResult(std::vector<sampleCount>::const_iterator from, std::vector<sampleCount>::const_iterator to, size_t numPixelsY, float* out);

protected:
   audacityTimeFrequencyCalculator(ITimeFrequencyCalculator *);
private:
   unique_ptr<ITimeFrequencyCalculator> ptfCalc;
};
#endif /* TF_Calculator_h */
