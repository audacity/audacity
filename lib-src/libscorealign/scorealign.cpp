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

#if (defined (WIN32) || defined (_WIN32))
#define	snprintf	_snprintf
#endif

#define	LOW_CUTOFF  40
#define HIGH_CUTOFF 2000

// Note: There are "verbose" flags passed as parameters that 
// enable some printing. The SA_VERBOSE compiler flag causes a
// lot more debugging output, so it could be called VERY_VERBOSE
// as opposed to the quieter verbose flags.

#ifdef SA_VERBOSE
#include "main.h"
#endif

// for presmoothing, how near does a point have to be to be "on the line"
#define NEAR 1.5

// path is file1_frames by file2_frames array, so first index
// (rows) is in [0 .. file1_frames]. Array is sequence of rows.
// columns (j) ranges from [0 .. file2_frames]
#define PATH(i,j) (path[(i) * file2_frames + (j)])

/*===========================================================================*/

#if DEBUG_LOG
FILE *dbf = NULL;
#endif


/*			MAP_TIME  
    lookup time of file1 in smooth_time_map and interpolate
    to get time in file2 
*/

float Scorealign::map_time(float t1)
{
    t1 /= actual_frame_period_1; // convert from seconds to frames
    int i = (int) t1; // round down
    if (i < 0) i = 0;
    if (i >= file1_frames - 1) i = file1_frames - 2;
    // interpolate to get time
    return actual_frame_period_2 * 
        interpolate(i, smooth_time_map[i], i+1, smooth_time_map[i+1],
                    t1);
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
                float note_end = n->time + n->dur;
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
		(i == 0 && j == 0)) 
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
    for (int i = 0 ; i < (file1_frames + file2_frames); i++) {
        if (smooth_time_map[i] * actual_frame_period_2 >= sec) {
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

int frames_of_init_silence( float *chrom_energy, int frame_count)
{
    bool silence = true;
    int frames=0; 
    while (silence) {
        if (silent(frames, chrom_energy)) 
            frames++; 
        else
            silence=false; 
    }
    
    return frames; 
}


/*		COMPARE_CHROMA
Perform Dynamic Programming to find optimal alignment
*/
void Scorealign::compare_chroma(bool verbose)
{
    float *path;
    int x = 0;
    int y = 0;
    
    /* Allocate the distance matrix */
    path = (float *) calloc(file1_frames * file2_frames, sizeof(float));
    
    /* Initialize first row and column */

    /* allow free skip over initial silence in either signal, but not both */
    /* silence is indicated by a run of zeros along the first row and or 
     * column, starting at the origin (0,0). After computing these runs, we
     * put the proper value at (0,0)
     */
    if (verbose) printf("Performing silent skip DP \n"); 
    PATH(0, 0) = (silent(0, chrom_energy1) ? 0 :
                  gen_dist(0, 0, chrom_energy1, chrom_energy2));
    for (int i = 1; i < file1_frames; i++)
        PATH(i, 0) = (PATH(i-1, 0) == 0 && silent(i, chrom_energy1) ? 0 :
                      gen_dist(i, 0, chrom_energy1, chrom_energy2) + 
                      PATH(i-1, 0));
    PATH(0, 0) = (silent(0, chrom_energy2) ? 0 :
                  gen_dist(0, 0, chrom_energy1, chrom_energy2));
    for (int j = 1; j < file2_frames; j++)
        PATH(0, j) = (PATH(0, j-1) == 0 && silent(j, chrom_energy2) ? 0 :
                      gen_dist(0, j, chrom_energy1, chrom_energy2) + 
                      PATH(0, j-1));
    /* first row and first column are done, put proper value at (0,0) */
    PATH(0, 0) = (!silent(0, chrom_energy1) || !silent(0, chrom_energy2) ?
                  gen_dist(0, 0, chrom_energy1, chrom_energy2) : 0);
    
    /* Perform DP for the rest of the matrix */
    for (int i = 1; i < file1_frames; i++)
        for (int j = 1; j < file2_frames; j++)
            PATH(i, j) = gen_dist(i, j, chrom_energy1, chrom_energy2) +
                min3(PATH(i-1, j-1), PATH(i-1, j), PATH(i, j-1)); 
    
    if (verbose) printf("Completed Dynamic Programming.\n");
    
    
    x = file1_frames - 1;
    y = file2_frames - 1;

    //x and y are the ending points, it can end at either the end of midi, 
    // or end of audio but not both
    pathx = ALLOC(short, (x + y + 2));
    pathy = ALLOC(short, (x + y + 2));
	
    assert(pathx != NULL);
    assert(pathy != NULL);
	 
    // map from file1 time to file2 time
    time_map = ALLOC(float, file1_frames);
    smooth_time_map = ALLOC(float, file1_frames);
	
#if DEBUG_LOG
    fprintf(dbf, "\nOptimal Path: ");
#endif
    while (1) {
        /* Check for stopping */
        if (x ==  0 & y == 0) {
            path_step(0, 0);
            path_reverse();
            break;
        }
		
        /* Print the current coordinate in the path*/
        path_step(x, y);

        /* Check for the optimal path backwards*/
        if (x > 0 && y > 0 && PATH(x-1, y-1) <= PATH(x-1, y) &&
            PATH(x-1, y-1) <= PATH(x, y-1)) {
            x--;
            y--;
        } else if (x > 0 && y > 0 && PATH(x-1, y) <= PATH(x, y-1)) {
            x--;
        } else if (y > 0) {
            y--;
        } else if (x > 0) {
            x--;
        }
    }
    free(path);
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
	 (file1_frames - (smooth+1)/2). Use that line for the last 
	 (smooth+1)/2 points.
*/
void Scorealign::compute_smooth_time_map()
{
    // do the first points:
    float a, b;
    linear_regression((smooth - 1) / 2, smooth, a, b);
    int i;
    for (i = 0; i < (smooth + 1) / 2; i++) {
        smooth_time_map[i] = a + b*i;
    }
    
    // do the middle points:
    for (i = (smooth + 1) / 2; i < file1_frames - (smooth + 1) / 2; i++) {
        linear_regression(i, smooth, a, b);
        smooth_time_map[i] = a + b*i;
        
#if DEBUG_LOG
        fprintf(dbf, "time_map[%d] = %g, smooth_time_map[%d] = %g\n", 
                i, time_map[i], i, a + b*i);
#endif
        
    }
    
    // do the last points
    linear_regression(file1_frames - (smooth + 1) / 2, smooth, a, b);
    for (i = file1_frames - (smooth + 1) / 2; i < file1_frames; i++) {
        smooth_time_map[i] = a + b*i;
    }
    
    
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
    int n = ROUND(presmooth_time / actual_frame_period_2);
    n = (n + 3) & ~3; // round up to multiple of 4
    int i = 0;
    while (pathx[i] + n < file2_frames) {
        /* line goes from i to i+n-1 */
        int x1 = pathx[i];
        int xmid = x1 + n/2;
        int x2 = x1 + n;
        int y1 = pathy[i];
        int y2;
        int j;
        /* search for y2 = pathy[j] s.t. pathx[j] == x2 */
        for (j = i + n; j < pathlen; j++) {
            if (pathx[j] == x2) {
                y2 = pathy[j];
                break;
            }
        }
        Regression regr;
        /* see if line fits the data */
        int k = i;
        int count = 0;
        while (pathx[k] < xmid) { // search first half
            if (near_line(x1, y1, x2, y2, pathx[k], pathy[k])) {
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
            if (near_line(x1, y1, x2, y2, pathx[k], pathy[k])) {
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
        // DEBUG
        if (k > j) {
            printf("oops: k %d, j %d\n", k, j);
            SA_V(print_path_range(pathx, pathy, i, k);)
        }
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
    // first, compute the y value of the path at
    // each x value. If the path has multiple values
    // on x, take the average.
    int p = 0;
    int i;
    int upper, lower;
    for (i = 0; i < file1_frames; i++) {
        lower = pathy[p];
        while (p < pathlen && pathx[p] == i) {
            upper = pathy[p];
            p = p + 1;
        }
        time_map[i] = (lower + upper) * 0.5;
    }
    // now fit a line to the nearest WINDOW points and record the 
    // line's y value for each x.
    compute_smooth_time_map();
}


void Scorealign::midi_tempo_align(Alg_seq &seq, bool verbose)
{
    // We create a new time map out of the alignment, and replace
    // the original time map in the Alg_seq sequence
    Alg_seq new_time_map_seq;

    /** align at all integer beats **/
    int totalbeats; 
    float dur_in_sec; 
    // probably alignment should respect the real_dur encoded into the seq
    // rather than computing real_dur based on note off times -- the 
    // caller should be required to set real_dur to a good value, and
    // the find_midi_duration() function should be available to the caller
    // if necessary -RBD
    find_midi_duration(seq, &dur_in_sec); 
    // 
    // totalbeat = lastbeat + 1 and round up the beat
    totalbeats = (int) (seq.get_time_map()->time_to_beat(dur_in_sec) + 2);
    if (verbose)
        printf("midi duration = %f, totalbeats=%i \n", dur_in_sec, totalbeats);   
    
    for (int i = 0; i < totalbeats; i++) {
        double newtime = map_time(seq.get_time_map()->beat_to_time(i));
        if (newtime > 0) 
            new_time_map_seq.insert_beat(newtime, (double) i);
    }
    seq.convert_to_beats();
    seq.set_time_map(new_time_map_seq.get_time_map());
}


// this routine performs an alignment by adjusting midi to match audio
//
void Scorealign::align_midi_to_audio(Alg_seq &seq, Audio_reader &reader, 
                                    bool verbose)
{
    /* Generate the chroma for file 1 
     * This will always be the MIDI File when aligning midi with audio.
     */
    file1_frames = gen_chroma_midi(seq, HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy1, &actual_frame_period_1, 1, verbose);

    /* Generate the chroma for file 2 */
    file2_frames = gen_chroma_audio(reader, HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy2, &actual_frame_period_2, 2, verbose);

    align_chromagrams(verbose);
}

void Scorealign::align_audio_to_audio(Audio_reader &reader1, 
                                     Audio_reader &reader2, bool verbose)
{
    file1_frames = gen_chroma_audio(reader1, HIGH_CUTOFF, LOW_CUTOFF, 
                    &chrom_energy1, &actual_frame_period_1, 1, verbose);
    file2_frames = gen_chroma_audio(reader2, HIGH_CUTOFF, LOW_CUTOFF, 
                    &chrom_energy2, &actual_frame_period_2, 2, verbose);
    align_chromagrams(verbose);
}


void Scorealign::align_midi_to_midi(Alg_seq &seq1, Alg_seq &seq2, 
                                   bool verbose)
{
    file1_frames = gen_chroma_midi(seq1, HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy1, &actual_frame_period_1, 1, verbose);

    file2_frames = gen_chroma_midi(seq2, HIGH_CUTOFF, LOW_CUTOFF, 
            &chrom_energy2, &actual_frame_period_2, 2, verbose);

    align_chromagrams(verbose);
}

void Scorealign::align_chromagrams(bool verbose)
{
    if (verbose)
        printf("\nGenerated Chroma.\n");
    /* now that we have actual_frame_period_2, we can compute smooth */
    // smooth is an odd number of frames that spans about smooth_time
    smooth = ROUND(smooth_time / actual_frame_period_2);
    if (smooth < 3) smooth = 3;
    if (!(smooth & 1)) smooth++; // must be odd
    if (verbose) {
        printf("smoothing time is %g\n", smooth_time);
        printf("smooth count is %d\n", smooth);
    }
    /* Normalize the chroma frames */
    norm_chroma(file1_frames, chrom_energy1);
    SA_V(printf("Chromagram data for file 1:\n");)
    SA_V(print_chroma_table(chrom_energy1, file1_frames);)
    norm_chroma(file2_frames, chrom_energy2);
    SA_V(printf("Chromagram data for file 2:\n");)
    SA_V(print_chroma_table(chrom_energy2, file2_frames);)
    if (verbose)
        printf("Normalized Chroma.\n");

    /* Compare the chroma frames */
    compare_chroma(verbose);
    /* Compute the smooth time map now for use by curve-fitting */	
    compute_regression_lines();
    /* if line_time is set, do curve-fitting */
    if (line_time > 0.0) {
        curve_fitting(this, verbose);
        /* Redo the smooth time map after curve fitting or smoothing */	
        compute_regression_lines();
    }
    /* if presmooth_time is set, do presmoothing */
    if (presmooth_time > 0.0) {
        presmooth();
        /* Redo the smooth time map after curve fitting or smoothing */	
        compute_regression_lines();
    }
}
