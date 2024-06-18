/*
* Audacity: A Digital Audio Editor
*/

#ifndef AU_AU3WRAP_AU3TYPES_H
#define AU_AU3WRAP_AU3TYPES_H

namespace au::au3 {
inline int au3VolumeToLocal(float volume)
{
    //! convert from range 0-1 to -60-0
    float old_max = 1;
    float old_min = 0;
    int old_range = old_max - old_min;

    int new_max = 0;
    int new_min = -60;
    int new_range = new_max - new_min;

    return (((volume - old_min) * new_range) / old_range) + new_min;
}

inline float localVolumeToAu3(int volume)
{
    //! convert from range -60-0 to 0-1
    float old_max = 0;
    float old_min = -60;
    int old_range = old_max - old_min;

    int new_max = 1;
    int new_min = 0;
    int new_range = new_max - new_min;

    return (((volume - old_min) * new_range) / old_range) + new_min;
}
}

#endif // AU_AU3WRAP_AU3TYPES_H
