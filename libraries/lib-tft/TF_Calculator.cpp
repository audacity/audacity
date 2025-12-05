//
//  TF_Calculator.h
//  Audacity
//
//  Created by Klaus Gram-Hansen on 01/12/2025.
//

#include "TF_Calculator.h"
#include "WaveletCalculator.h"
#include "StftCalculator.h"
#include <assert.h>
#include <vector>

unique_ptr<audacityTimeFrequencyCalculator> audacityTimeFrequencyCalculator::createWaveletCalculator(double q, double tf_overlap, double fmax, unsigned int nb_octaves)
{
   assert(q > 0);
   assert (tf_overlap < 100);
   assert (fmax <= 0.5);
   assert(nb_octaves > 0);
   
   audacityTimeFrequencyCalculator * obj = new audacityTimeFrequencyCalculator( new WaveletCalculator(nb_octaves, fmax, q, tf_overlap));
   return unique_ptr<audacityTimeFrequencyCalculator>(obj);
}

unique_ptr<audacityTimeFrequencyCalculator> audacityTimeFrequencyCalculator::createStftCalculator(size_t windowLength, size_t transformLength, const float * pWindow)
{
   assert(windowLength > 0);
   assert(transformLength >= windowLength);
   assert(pWindow);
   
   audacityTimeFrequencyCalculator * obj = new audacityTimeFrequencyCalculator(new StftCalculator(windowLength, transformLength, pWindow));
   return unique_ptr<audacityTimeFrequencyCalculator>(obj);
}

audacityTimeFrequencyCalculator::audacityTimeFrequencyCalculator(ITimeFrequencyCalculator * calc)
{
   ptfCalc.reset(calc);
}

void audacityTimeFrequencyCalculator::prepare(unsigned int nSamples, unsigned int resolution, unsigned int & nPre, unsigned int & nPost)
{
   assert(ptfCalc);
   ptfCalc->prepare(nSamples, resolution, nPre, nPost);
}

void audacityTimeFrequencyCalculator::transform(const float * pSamples, unsigned int nSamples, unsigned int nPre, unsigned int nPost)
{
   unsigned int nb = ptfCalc->doTransform(pSamples, nSamples, nPre, nPost);
}

size_t audacityTimeFrequencyCalculator::getResult(std::vector<sampleCount>::const_iterator from, std::vector<sampleCount>::const_iterator to, size_t numPixelsY, float* out)
{
   // Lambda to sampleCount int to double
   auto transformer = [](sampleCount s, sampleCount b) { return (s - b).as_double(); };

    // Create a transforming iterator. This requires C++17 for std::transform_iterator
    std::vector<double> timestamps;
    for (auto it = from; it != to; it++) {
       timestamps.push_back(transformer(*it, *from));
    }
   
   unsigned int nb = ptfCalc->extractFrequencySlices(timestamps, 0.0, 0.5 / numPixelsY, numPixelsY, out, timestamps.size() * numPixelsY, true);
   assert (nb == timestamps.size() * numPixelsY);
   return nb;
}
