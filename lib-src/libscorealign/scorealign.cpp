#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#ifndef __MACH__
#include <malloc.h>
#endif
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "gen_chroma.h"
#include "comp_chroma.h"
#include "curvefit.h"
#include "mfmidi.h"
#include "regression.h"
#include "sautils.h"

#if (defined (WIN32) || defined (_WIN32)) && _MSC_VER < 1900
#define	snprintf	_snprintf
#endif

#define	LOW_CUTOFF  40
#define HIGH_CUTOFF 2000

// Note: There is a "verbose" flag in Score_align objects that
// enable some printing. The SA_VERBOSE compiler flag causes a
// lot more debugging output, so it could be called VERY_VERBOSE
// as opposed to the quieter verbose flags.

#ifdef SA_VERBOSE
#include "main.h"
#endif

// for presmoothing, how near does a point have to be to be "on the line"
#define NEAR 1.5

// path is file0_frames by file1_frames array, so first index
// (rows) is in [0 .. file0_frames]. Array is sequence of rows.
// columns (j) ranges from [0 .. file1_frames]
#define PATH(i,j) (path[(i) * file1_frames + (j)])

/*===========================================================================*/

#if DEBUG_LOG
FILE *dbf = NULL;
#endif


Scorealign::Scorealign() {
    frame_period = SA_DFT_FRAME_PERIOD;
    window_size = SA_DFT_WINDOW_SIZE;
    force_final_alignment = SA_DFT_FORCE_FINAL_ALIGNMENT;
    ignore_silence = SA_DFT_IGNORE_SILENCE;
    silence_threshold = SA_DFT_SILENCE_THRESHOLD;
    presmooth_time = SA_DFT_PRESMOOTH_TIME;
    line_time = SA_DFT_LINE_TIME;
    smooth_time = SA_DFT_SMOOTH_TIME;
    pathlen = 0;
    path_count = 0;
    pathx = NULL;
    pathy = NULL;
    verbose = false;
    progress = NULL;
#if DEBUG_LOG
    dbf = fopen("debug-log.txt", "w");
    assert(dbf);
#endif
}


Scorealign::~Scorealign() {
    if (pathx) free(pathx);
    if (pathy) free(pathy);
#if DEBUG_LOG
    fclose(dbf);
#endif
}


/*			MAP_TIME  
    lookup time of file0 in smooth_time_map and interpolate
    to get time in file1 
*/

float Scorealign::map_time(float t1)
{
    t1 /= (float) actual_frame_period_0; // convert from seconds to frames
    int i = (int) t1; // round down
    if (i < 0) i = 0;
    if (i >= file0_frames - 1) i = file0_frames - 2;
    // interpolate to get time
    return float(actual_frame_period_1 * 
        interpolate(i, smooth_time_map[i], i+1, smooth_time_map[i+1],
                    t1));
}


/*				FIND_MIDI_DURATION 
    Finds the duration of a midi song where the end
    is defined by where the last note off occurs. Duration
    in seconds is given in DUR, and returns in int the number
    of notes in the song
*/

int find_midi_duration(Alg_seq &seq, float *dur) 
{
    *dur = 0.0F;
    int nnotes = 0;
    int i, j;
    seq.convert_to_seconds();
    for (j = 0; j < seq.track_list.length(); j++) {
        Alg_events &notes = (seq.track_list[j]);
            
        for (i = 0; i < notes.length(); i++) {
            Alg_event_ptr e = notes[i];
            if (e->is_note()) {
                Alg_note_ptr n = (Alg_note_ptr) e;
                float note_end = float(n->time + n->dur);
                if (note_end > *dur) *dur = note_end;
                nnotes++;
            }
        }
    }
    return nnotes; 
}
    
    
    
/* Returns the minimum of three values */
double min3(double x, double y, double z)
{
    return (x < y ?
            (x < z ? x : z) :
            (y < z ? y : z));
}


void save_frames(char *name, int frames, float **chrom_energy)
{
    FILE *outf = fopen(name, "w");
    int i,j;
    for (j=0; j < frames; j++) {
        float *chrom_energy_frame = chrom_energy[j];
        for (i = 0;  i <= CHROMA_BIN_COUNT; i++) {
            fprintf(outf, "%g ", chrom_energy_frame[i]);
        }
        fprintf(outf, "\n");
    }
    fclose(outf);
}


/* steps through the dynamic programming path
*/
void Scorealign::path_step(int i, int j)
{
#if DEBUG_LOG
    fprintf(dbf, "(%i,%i) ", i, j);
    if (++path_count % 5 == 0 ||
        (i == first_x && j == first_y)) 
        fprintf(dbf, "\n");
#endif
    pathx[pathlen] = i; 
    pathy[pathlen] = j;
    pathlen++;
}        


/* path_reverse -- path is computed from last to first, flip it */
/**/
void Scorealign::path_reverse()
{
    int i = 0;
    int j = pathlen - 1;
    while (i < j) {
        short tempx = pathx[i]; short tempy = pathy[i];
        pathx[i] = pathx[j]; pathy[i] = pathy[j];
        pathx[j] = tempx; pathy[j] = tempy;
        i++; j--;
    }
}
 
/*
  Sees if the chroma energy vector is silent (indicated by the 12th element being one)
  Returns true if it is silent.  False if it is not silent 
*/
 bool silent( int i, float *chrom_energy)
 {
     if (AREF2(chrom_energy, i,CHROMA_BIN_COUNT) == 1.0F)
         return true;
     else 
         return false; 
     
}

/*
returns the first index in pathy where the element is bigger than sec 
*/
int Scorealign::sec_to_pathy_index(float sec) 
{
    for (int i = 0 ; i < (file0_frames + file1_frames); i++) {
        if (smooth_time_map[i] * actual_frame_period_1 >= sec) {
            return i; 
        }
        //printf("%i\n" ,pathy[i]);
    }
    return -1; 
}


/*	
given a chrom_energy vector, sees how many 
of the inital frames are designated as silent 
*/

int frames_of_init_silence(float *chrom_energy, int frame_count)
{
    int frames;
    for (frames = 0; frames < frame_count; frames++) {
        if (!silent(frames, chrom_energy)) break;
    }
    return frames; 
}

int last_non_silent_frame(float *chrom_energy, int frame_count)
{
    int frames;
    for (frames = frame_count - 1; frames > 0; frames--) {
        if (!silent(frames, chrom_energy)) break;
    }
    return frames; 
}


/*		COMPARE_CHROMA
Perform Dynamic Programming to find optimal alignment
*/
int Scorealign::compare_chroma()
{
    float *path;
    
    /* Allocate the distance matrix */
    path = (float *) calloc(file0_frames * file1_frames, sizeof(float));
    
    /* skip over initial silence in signals */
    if (ignore_silence) {
        first_x = frames_of_init_silence(chrom_energy0, file0_frames);
        last_x = last_non_silent_frame(chrom_energy0, file0_frames);
        first_y = frames_of_init_silence(chrom_energy1, file1_frames);
        last_y = last_non_silent_frame(chrom_energy1, file1_frames);
    } else {
        first_x = 0;
        last_x = file0_frames - 1;
        first_y = 0;
        last_y = file1_frames - 1;
    }

    if (last_x - first_x <= 0 || last_y - first_y <= 0) {
        return SA_TOOSHORT;
    }

    /* Initialize first row and column */
    if (verbose) printf("Performing DP\n"); 
    PATH(first_x, first_y) = gen_dist(first_x, first_y);
    for (int x = first_x + 1; x <= last_x; x++)
        PATH(x, first_y) = gen_dist(x, first_y) + PATH(x - 1, first_y);
    for (int y = 1; y <= last_y; y++)
        PATH(first_x, y) = gen_dist(first_x, y) + PATH(first_x, y - 1);

#if DEBUG_LOG
    fprintf(dbf, "DISTANCE MATRIX ***************************\n");
#endif
    /* Perform DP for the rest of the matrix */
    for (int x = first_x + 1; x <= last_x; x++) {
        for (int y = first_y + 1; y <= last_y; y++) {
            PATH(x, y) = gen_dist(x, y) +
                    float(min3(PATH(x-1, y-1), PATH(x-1, y), PATH(x, y-1))); 
#if DEBUG_LOG
            fprintf(dbf, "(%d %d %g) ", x, y, gen_dist(x, y), PATH(x, y));
#endif
        }
#if DEBUG_LOG
        fprintf(dbf, "\n");
#endif
        // report progress for each file0_frame (column)
        // This is not quite right if we are ignoring silence because
        // then only a sub-matrix is computed.
        if (progress && !progress->set_matrix_progress(file1_frames)) 
            return SA_CANCEL;
    }
#if DEBUG_LOG
    fprintf(dbf, "END OF DISTANCE MATRIX ********************\n");
#endif

    if (verbose) printf("Completed Dynamic Programming.\n");
    
    
    //x and y are the ending points, it can end at either the end of midi, 
    // or end of audio or both
    pathx = ALLOC(short, (file0_frames + file1_frames));
    pathy = ALLOC(short, (file0_frames + file1_frames));
	
    assert(pathx != NULL);
    assert(pathy != NULL);
	 
    // map from file0 time to file1 time
    time_map = ALLOC(float, file0_frames);
    smooth_time_map = ALLOC(float, file0_frames);
	
    int x = last_x;
    int y = last_y;

    if (!force_final_alignment) {
#if DEBUG_LOG
        fprintf(dbf, "\nOptimal Path: ");
#endif
        // find end point, the lowest cost matrix value at one of the
        // sequence endings
        float min_cost = 1.0E10;
        for (int i = first_x; i <= last_x; i++) {
            if (PATH(i, last_y) <= min_cost) {
                min_cost = PATH(i, last_y);
                x = i;
                y = last_y;
            }
        }
        for (int j = first_y; j <= last_y; j++) {
            if (PATH(last_x, j) <= min_cost) {
                min_cost = PATH(last_x, j);
                x = last_x;
                y = j;
            }
        }
#if DEBUG_LOG
        fprintf(dbf, "Min cost at %d %d\n\nPATH:\n", x, y);
#endif
    }

    while ((x != first_x) || (y != first_y)) {
        path_step(x, y);

        /* Check for the optimal path backwards*/
        if (x > first_x && y > first_y && PATH(x-1, y-1) <= PATH(x-1, y) &&
            PATH(x-1, y-1) <= PATH(x, y-1)) {
            x--;
            y--;
        } else if (x > first_x && y > first_y && PATH(x-1, y) <= PATH(x, y-1)) {
            x--;
        } else if (y > first_y) {
            y--;
        } else if (x > first_x) {
            x--;
        }
    }
    path_step(x, y);
    path_reverse();
    free(path);
    return SA_SUCCESS; // success
}


void Scorealign::linear_regression(int n, int width, float &a, float &b)
{
    int hw = (width - 1) / 2; // a more convenient form: 1/2 width
    // compute average of x = avg of time_map[i]
    float xsum = 0;
    float ysum = 0;
    float xavg, yavg;
    int i;
    for (i = n - hw; i <= n + hw; i++) {
        xsum += i;
        ysum += time_map[i];
    }
    xavg = xsum / width;
    yavg = ysum / width;
    float num = 0;
    float den = 0;
    for (i = n - hw; i <= n + hw; i++) {
        num += (i - xavg) * (time_map[i] - yavg);
        den += (i - xavg) * (i - xavg);
    }
    b = num / den;
    a = yavg - b * xavg;
}


/*			COMPUTE_SMOOTH_TIME_MAP 
	 compute regression line and estimate point at i
 
	 Number of points in regression is smooth (an odd number). First
	 index to compute is (smooth-1)/2. Use that line for the first
	 (smooth+1)/2 points. The last index to compute is 
	 (file0_frames - (smooth+1)/2). Use that line for the last 
	 (smooth+1)/2 points.
*/
void Scorealign::compute_smooth_time_map()
{
    int i;
    int hw = (smooth - 1) / 2; // half width of smoothing window

    // find the first point
    for (i = 0; i < first_x; i++) {
        smooth_time_map[i] = NOT_MAPPED;
    }

    // do the first points:
    float a, b;
    linear_regression(first_x + hw, smooth, a, b);
    for (i = first_x; i <= first_x + hw; i++) {
        smooth_time_map[i] = a + b * i;
    }
    
    // do the middle points:
    for (i = first_x + hw + 1; i < last_x - hw; i++) {
        linear_regression(i, smooth, a, b);
        smooth_time_map[i] = a + b * i;
        
#if DEBUG_LOG
        fprintf(dbf, "time_map[%d] = %g, smooth_time_map[%d] = %g\n", 
                i, time_map[i], i, a + b*i);
#endif
        
    }

    // do the last points
    linear_regression(last_x - hw, smooth, a, b);
    for (i = last_x - hw; i <= last_x; i++) {
        smooth_time_map[i] = a + b * i;
    }
    // finally, fill with NOT_MAPPED
    for (i = last_x + 1; i < file0_frames; i++) 
        smooth_time_map[i] = NOT_MAPPED;
}
 

/* near_line -- see if point is near line */
/**/
bool near_line(float x1, float y1, float x2, float y2, float x, float y)
{
    float exact_y;
    if (x1 == x) {
        exact_y = y1;
    } else {
        assert(x1 != x2);
        exact_y = y1 + (y2 - y1) * ((x - x1) / (x2 - x1));
    }
    y = y - exact_y;
    return y < NEAR && y > -NEAR;
}


// path_copy -- copy a path for debugging
short *path_copy(short *path, int len)
{
    short *new_path = ALLOC(short, len);
    memcpy(new_path, path, len * sizeof(path[0]));
    return new_path;
}


/* presmooth -- try to remove typical dynamic programming errors
 * 
 * A common problem is that the best path wanders off track a ways
 * and then comes back. The idea of presmoothing is to see if the
 * path is mostly a straight line. If so, adjust the points off of
 * the line to fall along the line. The variable presmooth_time is
 * the duration of the line. It is drawn between every pair of 
 * points presmooth_time apart. If 25% of the first half of the line
 * falls within one frame of the path, and 25% of the second half of
 * the line falls within one frame of the path, then find the best
 * fit of the line to the points within 1 frame. Then adjust the middle
 * part of the line (from 25% to 75%) to fall along the line.
 * Note that all this curve fitting is done on integer coordinates.
 */
void Scorealign::presmooth()
{
    int n = ROUND(presmooth_time / actual_frame_period_1);
    n = (n + 3) & ~3; // round up to multiple of 4
	if (n < 4) {
		SA_V(printf("presmooth time %g rounded to zero %gs frame periods.\n",
			        presmooth_time, actual_frame_period_1););
		return;
	}
    int i = 0;
    while (i < pathlen - n && pathx[i] + n <= last_x) {
        /* line goes from i to i+n-1 */
        int x1 = pathx[i];
        int xmid = x1 + n/2;
        int x2 = x1 + n;
        int y1 = pathy[i];
        int y2 = pathy[i + 1]; // make sure it has a value. y2 should be
                               // set in the loop below.
        int j;
        /* search for y2 = pathy[j] s.t. pathx[j] == x2 */
        for (j = i + n; j < pathlen; j++) {
            if (pathx[j] == x2) {
                y2 = pathy[j];
                break;
            }
        }
		// this should not happen, but this guarantees that we found
		// y2 and it is within the path:
		if (j >= pathlen) break;

        Regression regr;
        /* see if line fits the data */
        int k = i;
        int count = 0;
        while (pathx[k] < xmid) { // search first half
            if (near_line(float(x1), float(y1), float(x2), float(y2), 
                          pathx[k], pathy[k])) {
                count++;
                regr.point(pathx[k], pathy[k]);
            }
            k++;
        }
        /* see if points were close to line */
        if (count < n/4) {
            i++;
            continue;
        }
        /* see if line fits top half of the data */
        while (pathx[k] < x2) {
            if (near_line(float(x1), float(y1), float(x2), float(y2), 
                pathx[k], pathy[k])) {
                count++;
                regr.point(pathx[k], pathy[k]);
            }
            k++;
        }
        /* see if points were close to line */
        if (count < n/4) {
            i++;
            continue;
        }
        /* debug: */
        SA_V(printf("presmoothing path from %d to %d:\n", i, j);)
        SA_V(print_path_range(pathx, pathy, i, j);)
        /* fit line to nearby points */
        regr.regress();
        /* adjust points to fall along line */
        // basically reconstruct pathx and pathy from i to j
        short x = pathx[i];
        short y = pathy[i];
        k = i + 1;
        SA_V(printf("start loop: j %d, pathx %d, pathy %d\n",
                 j, pathx[j], pathy[j]);)
        while (x < pathx[j] || y < pathy[j]) {
            SA_V(printf("top of loop: x %d, y %d\n", x, y);)
            // iteratively make an optional move in the +y direction
            // then make a move in the x direction
            // check y direction: want to move to y+1 if either we are below
            // the desired y coordinate or we are below the maximum slope
            // line (if y is too low, we'll have to go at sharper than 2:1
            // slope to get to pathx[j], pathy[j], which is bad
            int target_y = ROUND(regr.f(x));
            SA_V(printf("target_y@%d %d, r %g, ", x, target_y, regr.f(x));)
            // but what if the line goes way below the last point?
            // we don't want to go below a diagonal through the last point
            int dist_to_last_point = pathx[j] - x;
            int minimum_y = pathy[j] - 2 * dist_to_last_point;
            if (target_y < minimum_y) {
                target_y = minimum_y;
                SA_V(printf("minimum_y %d, ", minimum_y);)
            }
            // alternatively, if line goes too high:
            int maximum_y = pathy[j] - dist_to_last_point / 2;
            if (target_y > maximum_y) {
                target_y = maximum_y;
                SA_V(printf("maximum y %d, ", maximum_y);)
            }
            // now advance to target_y
            if (target_y > y) {
                pathx[k] = x;
                pathy[k] = y + 1;
                SA_V(printf("up: pathx[%d] %d, pathy[%d] %d\n", 
                         k, pathx[k], k, pathy[k]);)
                k++;
                y++;
            }
            if (x < pathx[j]) {
                // now advance x
                x++;
                // y can either go horizontal or diagonal, i.e. y either
                // stays the same or increments by one
                target_y = ROUND(regr.f(x));
                SA_V(printf("target_y@%d %d, r %g, ", x, target_y, regr.f(x));)
                if (target_y > y) y++;
                pathx[k] = x;
                pathy[k] = y;
                SA_V(printf("pathx[%d] %d, pathy[%d] %d\n", 
                         k, pathx[k], k, pathy[k]);)
                k++;
            }
        }
        // make sure new path is no longer than original path
        // the last point we wrote was k - 1
        k = k - 1; // the last point we wrote is now k
        assert(k <= j);
        // if new path is shorter than original, then fix up path
        if (k < j) {
            memmove(&pathx[k], &pathx[j], sizeof(pathx[0]) * (pathlen - j));
            memmove(&pathy[k], &pathy[j], sizeof(pathy[0]) * (pathlen - j));
            pathlen -= (j - k);
        }
        /* debug */
        SA_V(printf("after presmoothing:\n");)
        SA_V(print_path_range(pathx, pathy, i, k);)
        /* since we adjusted the path, skip by 3/4 of n */
        i = i + 3 * n/4;
    }
}


/*				COMPUTE_REGRESSION_LINES
	computes the smooth time map from the path computed
	by dynamic programming

*/
void Scorealign::compute_regression_lines()
{
    int i;
    // fill in time_map with NOT_MAPPED until the first point 
    // of the path
    for (i = 0; i < pathx[0]; i++) {
        time_map[i] = NOT_MAPPED;
    }
    // now, compute the y value of the path at
    // each x value. If the path has multiple values
    // on x, take the average.
    int p = 0;
    int upper, lower;
    for (i = pathx[0]; p < pathlen; i++) {
        lower = pathy[p];
        while (p < pathlen && pathx[p] == i) {
            upper = pathy[p];
            p = p + 1;
        }
        time_map[i] = (lower + upper) * 0.5F;
    }
    // fill in rest of time_map with NOT_MAPPED
    for (i = pathx[pathlen - 1] + 1; i <= last_x; i++) {
        time_map[i] = NOT_MAPPED;
    }
    // now fit a line to the nearest WINDOW points and record the 
    // line's y value for each x.
    compute_smooth_time_map();
}


void Scorealign::midi_tempo_align(Alg_seq &seq)
{
    // We create a new time map out of the alignment, and replace
    // the original time map in the Alg_seq sequence
    Alg_seq new_time_map_seq;

    /** align at all integer beats **/
    // totalbeats = lastbeat + 1 and round up the beat
    int totalbeats = (int) seq.get_beat_dur() + 2;
    if (verbose) {
        double dur_in_sec = seq.get_real_dur(); 
        printf("midi duration = %f, totalbeats=%i \n", dur_in_sec, totalbeats);   
    }
#ifdef DEBUG_LOG
    fprintf(dbf, "***************** CONSTRUCTING TIME MAP ***************\n");
#endif
    // turn off last tempo flag so last tempo will extrapolate
    new_time_map_seq.get_time_map()->last_tempo_flag = false;
    int first_beat = -1;
    for (int i = 0; i < totalbeats; i++) {
        double newtime = map_time(float(seq.get_time_map()->beat_to_time(i)));
        if (newtime > 0) {
            new_time_map_seq.insert_beat(newtime, (double) i);
            // remember where the new time map begins
            if (first_beat < 0) first_beat = i;
#ifdef DEBUG_LOG
            fprintf(dbf, "map beat %d to time %g\n", i, newtime);
#endif
        }
    }
    seq.convert_to_beats();
    double end_beat = seq.get_dur();
    Alg_time_map_ptr map = new_time_map_seq.get_time_map();
    seq.set_time_map(map);
    // the new time map begins where the alignment began, but due to
    // smoothing and rounding, there may be some edge effects.
    // Try to set the tempo before the first_beat to match the tempo
    // at the first beat by introducing another time map point at least
    // one beat before the first_beat. To do this, we need at least
    // 2 beats before first_beat and at least 2 beats in the time map 
    // (needed to compute initial tempo). Furthermore, the tempo at 
    // first_beat could be so slow that we do not have enough time 
    // before first_beat to anticipate the tempo.
    if (first_beat >= 2 && totalbeats > first_beat + 1) {
        int new_beat = first_beat / 2;
        // compute initial tempo from first_beat and first_beat + 1
        int i = map->locate_beat(first_beat);
        double t1 = map->beats[i].time;
        double t2 = map->beats[i + 1].time;
        double spb = (t2 - t1); // seconds per beat, beat period
        double new_time = t1 - (first_beat - new_beat) * spb;
        if (new_time <= 0.2) {
            // not enough time to start at new_time, new_beat
            // let's try using half the time rather than half the beats
            new_time = t1 / 2.0;
            // this will round down, so new_beat < first_beat
            new_beat = int(first_beat - (t1 / 2) / spb);
            new_time = t1 - (first_beat - new_beat) * spb;
        }
        // need to check again if new_beat would be too early
        if (new_time > 0.2) {
            map->insert_beat(new_time, new_beat);
        }
    }
    // Note: final tempo is extrapolated, so no need to insert new
    // time map points beyond the last one
    seq.set_dur(end_beat);
#ifdef DEBUG_LOG
    fprintf(dbf, "\nend_beat %g end time %g\n", 
            seq.get_beat_dur(), seq.get_real_dur());
#endif
}


// this routine performs an alignment by adjusting midi to match audio
//
int Scorealign::align_midi_to_audio(Alg_seq &seq, Audio_reader &reader)
{
    float dur = 0.0F;
    int nnotes = find_midi_duration(seq, &dur);
    if (progress) {
        progress->set_frame_period(frame_period);
        progress->set_smoothing(line_time > 0.0);
        progress->set_duration(0, false, dur);
        progress->set_duration(1, true, float(reader.actual_frame_period * 
                                              reader.frame_count));
        progress->set_phase(0);
    }
    /* Generate the chroma for file 0 
     * This will always be the MIDI File when aligning midi with audio.
     */
    file0_frames = gen_chroma_midi(seq, dur, nnotes, HIGH_CUTOFF, LOW_CUTOFF, 
                                   &chrom_energy0, &actual_frame_period_0, 0);

    /* Generate the chroma for file 1 */
    if (progress) progress->set_phase(1);
    file1_frames = gen_chroma_audio(reader, HIGH_CUTOFF, LOW_CUTOFF, 
                                    &chrom_energy1, &actual_frame_period_1, 1);
    return align_chromagrams();
}

int Scorealign::align_audio_to_audio(Audio_reader &reader0,
        Audio_reader &reader1)
{
    if (progress) {
        progress->set_frame_period(frame_period);
        progress->set_duration(0, true, float(reader0.actual_frame_period * 
                                              reader0.frame_count));
        progress->set_duration(1, true, float(reader1.actual_frame_period * 
                                              reader1.frame_count));

        progress->set_phase(0);
        progress->set_smoothing(line_time > 0.0);
    }
    file0_frames = gen_chroma_audio(reader0, HIGH_CUTOFF, LOW_CUTOFF, 
                                    &chrom_energy0, &actual_frame_period_0, 0);

    if (progress) progress->set_phase(1);
    file1_frames = gen_chroma_audio(reader1, HIGH_CUTOFF, LOW_CUTOFF, 
                                    &chrom_energy1, &actual_frame_period_1, 1);

    return align_chromagrams();
}


int Scorealign::align_midi_to_midi(Alg_seq &seq0, Alg_seq &seq1)
{
    float dur0 = 0.0F;
    int nnotes0 = find_midi_duration(seq0, &dur0);
    float dur1 = 0.0F;
    int nnotes1 = find_midi_duration(seq1, &dur1);
    if (progress) {
        progress->set_frame_period(frame_period);
        progress->set_smoothing(line_time > 0.0);
        progress->set_duration(0, false, dur0);
        progress->set_duration(1, false, dur1);

        progress->set_phase(0);
    }
    file0_frames = gen_chroma_midi(seq0, dur0, nnotes0, 
            HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy0, &actual_frame_period_0, 0);

    if (progress) progress->set_phase(1);
    file1_frames = gen_chroma_midi(seq1, dur1, nnotes1, 
            HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy1, &actual_frame_period_1, 1);

    return align_chromagrams();
}

int Scorealign::align_chromagrams()
{
    if (progress) progress->set_phase(2);
    if (verbose)
        printf("\nGenerated Chroma.\n");
    /* now that we have actual_frame_period_1, we can compute smooth */
    // smooth is an odd number of frames that spans about smooth_time
    smooth = ROUND(smooth_time / actual_frame_period_1);
    if (smooth < 3) smooth = 3;
    if (!(smooth & 1)) smooth++; // must be odd
    if (verbose) {
        printf("smoothing time is %g\n", smooth_time);
        printf("smooth count is %d\n", smooth);
    }
    SA_V(printf("Chromagram data for file 0:\n");)
    SA_V(print_chroma_table(chrom_energy0, file0_frames);)
    SA_V(printf("Chromagram data for file 1:\n");)
    SA_V(print_chroma_table(chrom_energy1, file1_frames);)

    /* Compare the chroma frames */
    int result = compare_chroma();
    if (result != SA_SUCCESS) {
        return result;
    }
    if (progress) progress->set_phase(3);
    /* Compute the smooth time map now for use by curve-fitting */	
    compute_regression_lines();
    /* if presmooth_time is set, do presmoothing */
    if (presmooth_time > 0.0) {
        presmooth();
        /* Redo the smooth time map after curve fitting or smoothing */	
        compute_regression_lines();
    }
    /* if line_time is set, do curve-fitting */
    if (line_time > 0.0) {
        curve_fitting(this, verbose);
        /* Redo the smooth time map after curve fitting or smoothing */	
        compute_regression_lines();
    }
    if (progress) progress->set_phase(4);
    return SA_SUCCESS;
}
