/*
 This file is part of the SimpleCompressor project.
 https://github.com/DanielRudrich/SimpleCompressor
 Copyright (c) 2019 Daniel Rudrich

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, version 3.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

#include "LookAheadGainReduction.h"
#include <cmath>
#include <algorithm>

namespace DanielRudrich {
void LookAheadGainReduction::setDelayTime(float delayTimeInSeconds)
{
    if (delayTimeInSeconds <= 0.0f) {
        delay = 0.0f;
    } else {
        delay = delayTimeInSeconds;
    }

    if (sampleRate != 0.0) {
        prepare(sampleRate, blockSize);
    }
}

void LookAheadGainReduction::prepare(const double newSampleRate, const int newBlockSize)
{
    sampleRate = newSampleRate;
    blockSize = newBlockSize;

    delayInSamples = static_cast<int>(delay * sampleRate);

    buffer.resize(blockSize + delayInSamples);
    std::fill(buffer.begin(), buffer.end(), 0.0f);
    writePosition = 0;
}

void LookAheadGainReduction::pushSamples(const float* src, const int numSamples)
{
    int startIndex, blockSize1, blockSize2;

    // write in delay line
    getWritePositions(numSamples, startIndex, blockSize1, blockSize2);

    for (int i = 0; i < blockSize1; ++i) {
        buffer[startIndex + i] = src[i];
    }

    if (blockSize2 > 0) {
        for (int i = 0; i < blockSize2; ++i) {
            buffer[i] = src[blockSize1 + i];
        }
    }

    writePosition += numSamples;
    writePosition = writePosition % buffer.size();

    lastPushedSamples = numSamples;
}

void LookAheadGainReduction::process()
{
    /** The basic idea here is to look for high gain-reduction values in the signal, and apply a fade which starts exactly  `delayInSamples` many samples before that value appears. Depending on the value itself, the slope of the fade will vary.

     Some things to note:
        - as the samples are gain-reduction values in decibel, we actually look for negative peaks or local minima.
        - it's easier for us to time-reverse our search, so we start with the last sample
        - once we find a minimum, we calculate the slope, which will be called `step`
        - with that slope, we can calculate the next value of our fade-in `nextGainReductionValue`
        - once a value in the buffer is below our fade-in value, we found a new minimum, which might not be as deep as the previous one, but as it comes in earlier, it needs more attention, so we update our fade-in slope
        - our buffer is a ring-buffer which makes things a little bit messy
     */

    // As we don't know any samples of the future, yet, we assume we don't have to apply a fade-in right now, and initialize both `nextGainReductionValue` and step (slope of the fade-in) with zero.
    float nextGainReductionValue = 0.0f;
    float step = 0.0f;

    // Get the position of the last sample in the buffer, which is the sample right before our new write position.
    int index = writePosition - 1;
    if (index < 0) { // in case it's negative...
        index += static_cast<int>(buffer.size());  // ... add the buffersize so we wrap around.
    }
    // == FIRST STEP: Process all recently pushed samples.

    // We want to process all `lastPushedSamples` many samples, so let's find out, how many we can process in a first run before we have to wrap around our `index` variable (ring-buffer).
    int size1, size2;
    getProcessPositions(index, lastPushedSamples, size1, size2);

    // first run
    for (int i = 0; i < size1; ++i) {
        const float smpl = buffer[index];

        if (smpl > nextGainReductionValue) { // in case the sample is above our ramp...
            buffer[index] = nextGainReductionValue; // ... replace it with the current ramp value
            nextGainReductionValue += step; // and update the next ramp value
        } else { // otherwise... (new peak)
            step = -smpl / delayInSamples;  // calculate the new slope
            nextGainReductionValue = smpl + step; // and also the new ramp value
        }
        --index;
    }

    // second run
    if (size2 > 0) { // in case we have some samples left for the second run
        index = static_cast<int>(buffer.size()) - 1;  // wrap around: start from the last sample of the buffer

        // exactly the same procedure as before... I guess I could have written that better...
        for (int i = 0; i < size2; ++i) {
            const float smpl = buffer[index];

            if (smpl > nextGainReductionValue) {
                buffer[index] = nextGainReductionValue;
                nextGainReductionValue += step;
            } else {
                step = -smpl / delayInSamples;
                nextGainReductionValue = smpl + step;
            }
            --index;
        }
    }

    /*
     At this point, we have processed all the new gain-reduction values. For this, we actually don't need a delay/lookahead at all.
     !! However, we are not finished, yet !!
     What if the first pushed sample has such a high gain-reduction value, that itself needs a fade-in? So we have to apply a gain-ramp even further into the past. And that is exactly the reason why we need lookahead, why we need to buffer our signal for a short amount of time: so we can apply that gain ramp for the first handful of gain-reduction samples.
     */

    if (index < 0) { // it's possible the index is exactly -1
        index = static_cast<int>(buffer.size()) - 1;  // so let's take care of that
    }
    /*
     This time we only need to check `delayInSamples` many samples.
     And there's another cool thing!
        We know that the samples have been processed already, so in case one of the samples is below our ramp value, that's the new minimum, which has been faded-in already! So what we do is hit the break, and call it a day!
     */
    getProcessPositions(index, delayInSamples, size1, size2);
    bool breakWasUsed = false;

    // first run
    for (int i = 0; i < size1; ++i) { // we iterate over the first size1 samples
        const float smpl = buffer[index];

        if (smpl > nextGainReductionValue) { // in case the sample is above our ramp...
            buffer[index] = nextGainReductionValue; // ... replace it with the current ramp value
            nextGainReductionValue += step; // and update the next ramp value
        } else { // otherwise... JACKPOT! Nothing left to do here!
            breakWasUsed = true; // let the guys know we are finished
            break;
        }
        --index;
    }

    // second run
    if (!breakWasUsed && size2 > 0) { // is there still some work to do?
        index = static_cast<int>(buffer.size()) - 1;  // wrap around (ring-buffer)
        for (int i = 0; i < size2; ++i) {
            const float smpl = buffer[index];

            // same as before
            if (smpl > nextGainReductionValue) { // in case the sample is above our ramp...
                buffer[index] = nextGainReductionValue; // ... replace it with the current ramp value
                nextGainReductionValue += step; // and update the next ramp value
            } else { // otherwise... already processed -> byebye!
                break;
            }
            --index;
        }
    }
}

void LookAheadGainReduction::readSamples(float* dest, int numSamples)
{
    int startIndex, blockSize1, blockSize2;

    // read from delay line
    getReadPositions(numSamples, startIndex, blockSize1, blockSize2);

    for (int i = 0; i < blockSize1; ++i) {
        dest[i] = buffer[startIndex + i];
    }

    if (blockSize2 > 0) {
        for (int i = 0; i < blockSize2; ++i) {
            dest[blockSize1 + i] = buffer[i];
        }
    }
}

inline void LookAheadGainReduction::getProcessPositions(int startIndex, int numSamples, int& blockSize1, int& blockSize2)
{
    if (numSamples <= 0) {
        blockSize1 = 0;
        blockSize2 = 0;
    } else {
        blockSize1 = std::min(startIndex + 1, numSamples);
        numSamples -= blockSize1;
        blockSize2 = numSamples <= 0 ? 0 : numSamples;
    }
}

inline void LookAheadGainReduction::getWritePositions(int numSamples, int& startIndex, int& blockSize1, int& blockSize2)
{
    const int L = static_cast<int>(buffer.size());
    int pos = writePosition;

    if (pos < 0) {
        pos = pos + L;
    }
    pos = pos % L;

    if (numSamples <= 0) {
        startIndex = 0;
        blockSize1 = 0;
        blockSize2 = 0;
    } else {
        startIndex = pos;
        blockSize1 = std::min(L - pos, numSamples);
        numSamples -= blockSize1;
        blockSize2 = numSamples <= 0 ? 0 : numSamples;
    }
}

inline void LookAheadGainReduction::getReadPositions(int numSamples, int& startIndex, int& blockSize1, int& blockSize2)
{
    const int L = static_cast<int>(buffer.size());
    int pos = writePosition - lastPushedSamples - delayInSamples;

    if (pos < 0) {
        pos = pos + L;
    }
    pos = pos % L;

    if (numSamples <= 0) {
        startIndex = 0;
        blockSize1 = 0;
        blockSize2 = 0;
    } else {
        startIndex = pos;
        blockSize1 = std::min(L - pos, numSamples);
        numSamples -= blockSize1;
        blockSize2 = numSamples <= 0 ? 0 : numSamples;
    }
}
} // namespace DanielRudrich
