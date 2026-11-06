/*
 * Audacity: A Digital Audio Editor
 */
// tracktion_graph's unity source, tracktion_graph.cpp, without its tests: their utilities need tracktion_engine, which
// isn't built. Same order as there.

#include <juce_audio_formats/juce_audio_formats.h>

#include <tracktion_graph/tracktion_graph.h>

#ifdef __GNUC__
 #pragma GCC diagnostic push
 #pragma GCC diagnostic ignored "-Wfloat-equal"
#endif

#include <tracktion_graph/tracktion_graph/tracktion_PlayHeadState.cpp>
#include <tracktion_graph/tracktion_graph/tracktion_PlayHead.cpp>
#include <tracktion_graph/tracktion_graph/tracktion_Utility.cpp>
#include <tracktion_graph/tracktion_graph/tracktion_MultiThreadedNodePlayer.cpp>
#include <tracktion_graph/tracktion_graph/tracktion_LockFreeMultiThreadedNodePlayer.cpp>
#include <tracktion_graph/tracktion_graph/tracktion_NodePlayerThreadPools.cpp>
#include <tracktion_graph/utilities/tracktion_Semaphore.cpp>
#include <tracktion_graph/utilities/tracktion_Threads.cpp>
#include <3rd_party/rpmalloc/rpallocator.cpp>

#ifdef __GNUC__
 #pragma GCC diagnostic pop
#endif
