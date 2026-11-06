/*
* Audacity: A Digital Audio Editor
*/
#pragma once

namespace au::au4juce {
//! What a MuteSoloNode needs besides its track
class IMuteSoloContext
{
public:
    virtual ~IMuteSoloContext() = default;

    //! Whether any track solos, in the current block
    virtual bool anySoloing() const = 0;
};
}
