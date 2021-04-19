/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistAPI.cpp

  Roger B. Dannenberg
  Apr 2021

  Interface for Nyquist access to tracks and more

**********************************************************************/

#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"
#include "Nyquist.h"
#include "NyquistAPI.h"
#include "../../WaveClip.h"
#include "../../LabelTrack.h"

// To access project information, effect runner finds and shares project here:
AudacityProject *audacityProject = NULL;
void setNyquistProject(AudacityProject *p)
{
    audacityProject = p;
}

LVAL properties(const Track *track)
{
    LVAL props, last, allClips, clips, lastClips, cl;
    xlstkcheck(4);
    xlsave(props);
    xlsave(allClips);
    xlsave(clips);
    xlsave(lastClips);
    if (track) {
        wxString wname = track->GetName();
        const char *name = wname.mb_str();
        last = props = consa(xlenter("NAME"));
        last = rplacd(last, consa(name ? cvstring(name) : NULL));
        last = rplacd(last, consa(xlenter("TYPE")));
        const char* kindString = NULL;
        track->TypeSwitch([&](const WaveTrack* t) { kindString = "WAVE"; },
                          [&](const NoteTrack* t) { kindString = "NOTE"; },
                          [&](const LabelTrack* t) { kindString = "LABEL"; });
        last = rplacd(last, consa(kindString ? xlenter(kindString) : NULL));
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack); // track->GetOwner();

            int nchans = channels.size();  // channels->size();
            last = rplacd(last, consa(xlenter("CHANNELS")));
            last = rplacd(last, consa(cvfixnum(nchans)));

            // compute the clips list as property "CLIPS":
            if (nchans > 1) {
                allClips = newvector(nchans);
            }

            int channel_index = 0;
            for (auto channel : channels) {
                clips = NULL;
                lastClips = NULL;

                auto ca = channel->SortedClipArray();
                for (size_t j = 0; j < ca.size(); j++) {
                    if (j < 1000) {
                        auto caj = ca[j]->GetStartTime();
                        cl = cons(cvflonum(ca[j]->GetStartTime()),
                            consa(cvflonum(ca[j]->GetEndTime())));
                        cl = consa(cl); // ((start end))
                        if (!clips) {
                            clips = cl;
                        }
                    }
                    else if (j == 1000) {
                        cl = consa(NULL); // (nil)
                    }
                    else {
                        break;
                    }
                    if (lastClips) {
                        lastClips = rplacd(lastClips, cl);
                    }
                    else {
                        lastClips = clips = cl;
                    }
                }
                if (allClips) {
                    setelement(allClips, channel_index++, clips);
                }
            }
            if (!allClips) {  // only one channel
                allClips = clips;
            }
            last = rplacd(last, consa(xlenter("CLIPS")));
            last = rplacd(last, consa(allClips));
            });
    }   
    xlpopn(4);
    return props;
}

const Track *getTrackByNumber(FIXTYPE n)
{
    const TrackList &trackList = TrackList::Get(*theNyquistProject);
    auto range = trackList.Leaders();

    if (n < 0 || n >= trackList.size()) {
        return NULL;
    }
    TrackList::const_iterator iter = range.begin();
    for (int i = 0; i < n; i++) iter++;  // advance to nth track
     return *iter;
}

const Track *getTrackByName(const char *name)
{
    const TrackList& trackList = TrackList::Get(*theNyquistProject);
    auto range = trackList.Leaders();

    for (auto ptrack : range) {
        wxString wname = ptrack->GetName();
        const char* trackName = wname.mb_str();
        if (strcmp(name, trackName) == 0) {
            return ptrack;
        }
    }
    return NULL;
}


const Track *getTrack(LVAL nameOrNumber)
{
    if (fixp(nameOrNumber)) {
        return getTrackByNumber(getfixnum(nameOrNumber));
    } else if (stringp(nameOrNumber)) {
        return getTrackByName((const char *)getstring(nameOrNumber));
    }
    return NULL;
}


LVAL getTrackInfo(LVAL nameOrNumber)
{
    const Track* track = getTrack(nameOrNumber);
    return properties(track);
}


// getAudio() creates a nyx_susp object to a source of samples
// This object only has a pointer to denote the state, but we
// need a buffer and other state, so we create a new class.
//
// A WaveTrackReader is allocated for each channel. When the effect
// completes, we need to delete the WaveTrackReaders, so keep them
// on a vector. If a sound terminates, we'll keep the WaveTrackReader
// anyway until the end of the effect.
//
// TODO: I think Nyquist effects can retain state and therefore sounds
// from one run to the next. Nyx should tie into the GC and keep
// backpointers to nyx_susp objects, changing their callbacks to 
// return zero if they are invoked after the end of an effect. There is
// no problem retaining a sound as long as it is no longer "lazy" and
// will not call back into Audacity looking for samples.

class WaveTrackReader;
static std::vector<WaveTrackReader*> waveTrackReaders;


class WaveTrackReader
{
public:
    SampleBuffer mCurBuffer;
    sampleCount  mCurBufferStart;
    size_t       mCurBufferLen;
    
    const WaveTrack *mCurTrack;

    WaveTrackReader(const WaveTrack *track) {
        mCurTrack = track;
        mCurBufferStart = 0;
        mCurBufferLen = 0;
        // save all created objects and free them with NyquistAPICleanup()
        // after the effect has completed:
        waveTrackReaders.push_back(this);
    }

    static int StaticGetCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen, void *userdata) {
        WaveTrackReader *wtr = (WaveTrackReader *)userdata;
        return wtr->GetCallback(buffer, channel, start, len, totlen);
    }

    // note: duplicate code from Nyquist.cpp (NyquistEffect)
    int GetCallback(float *buffer, int WXUNUSED(ch),
        int64_t start, int64_t len, int64_t WXUNUSED(totlen))
    {
        if (mCurBuffer.ptr()) {
            if (start < mCurBufferStart || 
                start + len > mCurBufferStart + mCurBufferLen) {
                mCurBuffer.Free();
            }
        }

        if (!mCurBuffer.ptr()) {
            mCurBufferStart = start;
            mCurBufferLen = mCurTrack->GetBestBlockSize(mCurBufferStart);

            if (mCurBufferLen < (size_t)len) {
                mCurBufferLen = ((WaveTrack *)mCurTrack)->GetIdealBlockSize();
            }
            
            // We don't have an end time (length) for the track
            // mCurBufferLen =
            //    limitSampleBufferSize(mCurBufferLen, mCurLen - mCurBufferStart);

            mCurBuffer.Allocate(mCurBufferLen, floatSample);

            // We're inside the Effects's try block, so none here?
            // here's the action: pull samples from track
            // try {
                mCurTrack->Get(
                    mCurBuffer.ptr(), floatSample,
                    mCurBufferStart, mCurBufferLen);
            /* }
            catch (...) {
                // Save the exception object for re-throw when out of the library
                mpException = std::current_exception();
                return -1;
            } */
        }

        // We have guaranteed above that this is nonnegative and bounded by
        // mCurBufferLen:
        auto offset = (start - mCurBufferStart).as_size_t();
        CopySamples(mCurBuffer.ptr() + offset * SAMPLE_SIZE(floatSample), floatSample,
            (samplePtr)buffer, floatSample,
            len);

        return 0;
    }
};


int64_t getTrackLen(const WaveTrack *waveTrack)
{
    auto channels = TrackList::Channels(waveTrack); // track->GetOwner();
    double srate = waveTrack->GetRate();
    int64_t len = 0;
    for (auto channel : channels) {
        auto ca = channel->SortedClipArray();
        int n = ca.size();
        if (n > 0) {
            auto clip = ca[n - 1];  // last clip
            int64_t clipLen = (int64_t)(clip->GetEndTime() * srate + 0.5);
            if (clipLen > len) len = clipLen;
        }
    }
    return len;
}


LVAL getAudio(LVAL nameOrNumber, double start, double dur)
{
    LVAL snd;
    xlsave1(snd);
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack); // track->GetOwner();
            int nchans = channels.size();  // channels->size();
            double srate = waveTrack->GetRate();
            int64_t len = (int64_t)(dur * srate + 0.5);
            if (nchans > 1) {
                snd = newvector(nchans);
                int ch = 0;
                for (auto channel : channels) {
                    WaveTrackReader* wtr = new WaveTrackReader(channel);
                    setelement(snd, ch, nyx_make_input_audio(
                        WaveTrackReader::StaticGetCallback, wtr, 1,
                        len, waveTrack->GetRate()));
                }
            }
            else {
                WaveTrackReader* wtr = new WaveTrackReader(waveTrack);
                snd = nyx_make_input_audio(
                    WaveTrackReader::StaticGetCallback, wtr, 1,
                    len, waveTrack->GetRate());
            }
        });
    }
    // if not a WaveTrack, fall through, return NULL
    xlpop();
    return snd;
}


class WaveTrackWriter
{
public:
    const char *error;
    WaveTrack* mCurTrack[2];
    WaveTrack* mOutputTrack[2];
    double mOutputTime;
    double srate;  // all channels must match

    // trkchans is the number of channels in the output track
    // chans in the number of channels in snd
    WaveTrackWriter(TrackIterRange<const WaveTrack>& channels, LVAL snd,
                    double rate, int trkchans, int chans,
                    double start, double maxdur) {
        error = NULL;
        mCurTrack[0] = mCurTrack[1] = nullptr;
        mOutputTrack[0] = mOutputTrack[1] = nullptr;
        mOutputTime = 0;
        srate = rate;
        std::shared_ptr<WaveTrack> outputTrack[2];

        // for now, require every output channel to have the same sample rate
        // the first channel already matches the Nyquist sound sample rate
        for (auto track : channels) {
            if (track->GetRate() != srate) {
                error = "Error: mismatched sample rates.";
                return;
            }
        }
        int i = 0;
        for (auto track : channels) {
            // this must be wrong. Also, I'm not sure how to write to a track.
            // I generally copied code from Nyquist.cpp (nyquist effects), but
            // I don't understand why it makes copies or how deep the copies 
            // go (are these virtual copies of whole tracks, or just empty
            // containers for new audio?)
            mCurTrack[i] = (WaveTrack *) (const WaveTrack *) track;
            outputTrack[i] = track->EmptyCopy();
            outputTrack[i]->SetRate(srate);
            i++;
        }
        {   // I suppose this is some kind of trick to clean up and restore
            // tracks if track writing raises an exception. 
            auto vr0 = valueRestorer(mOutputTrack[0], outputTrack[0].get());
            auto vr1 = valueRestorer(mOutputTrack[1], outputTrack[1].get());
            if (!nyx_pump_audio(snd, (int64_t)(maxdur / srate + 0.5),
                                &StaticPutCallback, (void*)this)) {
                error = "Error: Something went wrong in evaluating expression.";
            }
        }
        for (int i = 0; i < trkchans; i++) {
            outputTrack[i]->Flush();
            mOutputTime = outputTrack[i]->GetEndTime();

            if (mOutputTime <= 0) {
                error = "Nyquist returned nil audio.\n";
                return;
            }
        }

        for (size_t i = 0; i < trkchans; i++) {
            WaveTrack* out;

            if (chans == trkchans) {
                out = outputTrack[i].get();
            } else {
                out = outputTrack[0].get();
            }

            // See Nyquist.cpp for some code to compute mRestoreSplits and 
            // bMergeClips options.  For now, we ignore options. Let's see 
            // if the behavior is reasonable, and if not we'll try something
            // else.
            mCurTrack[i]->ClearAndPaste(start, start + maxdur, out, 
                                   true, true); // mRestoreSplits, bMergeClips
            // See Nyquist.cpp for code dealing with SyncLockGroups here
        }
        // Well, we wrote to the project, so probably something should be 
        // notified. Nyquist.cpp sets mProjectChanged = true; and we're in
        // an effect, but how can we find it? Should this go through globals
        // in nyx.c? Which already uses some globals to negotiate between
        // Audacity and Nyquist?
        // mProjectChanged = true;
    }

    static int StaticPutCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen, void* userdata) {
        WaveTrackWriter* wtw = (WaveTrackWriter*)userdata;
        return wtw->PutCallback(buffer, channel, start, len, totlen);
    }

    int PutCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen) {
        // Don't let C++ exceptions propagate through the Nyquist library
        return GuardedCall<int>([&] {
             mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len);

            return 0; // success
            }, MakeSimpleGuard(-1)); // translate all exceptions into failure
    }
};

LVAL putAudio(LVAL nameOrNumber, LVAL expr, double start, double maxdur)
{
    LVAL snd;
    double srate;
    const Track* track;
    xlsave1(snd);
    snd = xleval(expr);
    int nchans = nyx_get_num_channels(snd);
    if (nchans < 1) {
        snd = cvstring("AUD_PUT_AUDIO did not get a sound.");
        goto exit;
    }
    srate = nyx_get_sample_rate(snd);

    track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack);
            int trkchans = channels.size();  // channels->size();
            double trkrate = waveTrack->GetRate();
            // int64_t len = (int64_t)(maxdur * trkrate + 0.5);
            // allow mono expanded to N trkchans, otherwise require a match
            if (nchans > 1 && trkchans != nchans) {
                snd = cvstring("Error: channel count mismatch.");
            } else if (nchans == 0) {
                snd = cvstring(
                    "Error: sound expression did not return a sound.");
            } else if (nchans == -1) {
                snd = cvstring(
                    "Error: sound expression returned array with one sound.");
            } else if (trkrate != srate) {
                snd = cvstring("Error: sample rate mismatch.");
            } else {
                WaveTrackWriter waveTrackWriter(channels, snd, trkrate, 
                                       trkchans, nchans, start, maxdur);
                if (waveTrackWriter.error) {
                    snd = cvstring(waveTrackWriter.error);
                } else {
                    snd = s_true;
                }
            }
        });
    }
  exit:
    xlpop();
    return snd;
}



LVAL getLabels(LVAL nameOrNumber, double start, double dur)
{
    LVAL labelList, elem;
    xlsave1(labelList);
    xlsave1(elem);  // each sublist ((T0 T1 title))
    LVAL last = NULL;  // last element in labelList
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const LabelTrack* labelTrack) {
            // int n = labelTrack->GetNumLabels();
            for (const auto& label : labelTrack->GetLabels()) {
                if (label.getT0() >= start &&
                    label.getT0() < start + dur) {
                    elem = consa(cvstring(label.title));
                    elem = cons(cvflonum(label.getT1()), elem);
                    elem = cons(cvflonum(label.getT0()), elem);
                    elem = consa(elem);  // ((T0 T1 title))
                    if (last) {
                        rplacd(last, elem);
                    }
                    else {
                        labelList = last = elem;
                    }
                }
            }
        });
    }
    // if not a LabelTrack, fall through, return NULL
    xlpopn(2);
    return labelList;

}

// WARNING: FUNCTIONS BELOW NEED IMPLEMENTATIONS AFTER WE GET AUDIO WORKING!

LVAL putLabels(LVAL nameOrNumber, LVAL labels, LVAL merge_flag)
{
    return NULL;
}


LVAL getNotes(LVAL nameOrNumber, double start, double dur)
{
    return NULL;
}

LVAL putNotes(LVAL nameOrNumber, LVAL notes, LVAL merge_flag)
{
    return NULL;
}


LVAL getTimes(LVAL nameOrNumber, double start, double dur)
{
    return NULL;
}


LVAL putTimes(LVAL nameOrNumber, LVAL breakpoints)
{
    return NULL;
}


// call this when effect has completed
void NyquistAPICleanup()
{
    waveTrackReaders.clear();
}

