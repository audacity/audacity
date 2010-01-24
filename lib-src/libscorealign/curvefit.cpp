/*
 *  curvefit.cpp
 *  scorealign
 *
 *  Created by Roger Dannenberg on 10/20/07.
 *  Copyright 2007 __MyCompanyName__. All rights reserved.
 *
 */

#include "assert.h"
#include "comp_chroma.h"
#include "sautils.h"
// the following are needed to get Scorealign
#include <fstream>
#include "allegro.h"
#include "audioreader.h"
#include "scorealign.h"
#include "hillclimb.h"
#include "curvefit.h"

void save_path(char *filename);

/* Curvefit class: do hill-climbing to fit lines to data
 *
 * This class implements the algorithm described above.
 * The problem is partitioned into the general search algorithm
 * (implemented in Hillclimb::optimize) and the evaluation function
 * (implemented in Curvefit::evaluate). A brute-force evaluation
 * would simply recompute the cost of the entire path every time,
 * but note that the search algorithm works by adjusting one parameter
 * at a time. This affects at most two line segments, so the rest
 * contribute a cost that does not need to be recomputed. Thus the
 * total cost can be computed incrementally. It is hard to see how
 * to use this optimization within the general Hillclimb:optimize
 * method, so to avoid making that algorithm very specific and ugly,
 * I decided to hide the incremental nature of evaluate inside
 * the evaluate function itself. The way this works is that evaluate
 * keeps a cache of the coordinates of each line segment and the
 * resulting cost of the segment. Before recomputing any segment,
 * the cache is consulted. If the end points have not moved, the
 * cached value is retrieved. Ideally, there should be a 3-element
 * cache because endpoints are moved and then restored. (The three
 * elements would hold the results of the original, changed left,
 * and changed right endpoints.) The bigger cache would eliminate
 * 1/3 of the computation, but the simple cache already eliminates
 * about (n-2)/n of the work, so that should help a lot.
 */

class Curvefit : public Hillclimb {
public:
    Curvefit(Scorealign *sa_, bool verbose_) { sa = sa_; verbose = verbose_; }
    virtual double evaluate();
    void setup(int n);
    double *get_x() { return x; }
private:
    Scorealign *sa;
    bool verbose;
    double line_dist(int i); // get cost of line segment i
    double compute_dist(int i); // compute cost of line segment i
    double distance_rc(int row, int col);
    double distance_xy(double x, double y);

    double *p1_cache; // left endpoint y values
    double *p2_cache; // right endpoint y values
    double *d_cache; // cached cost of line segment
    double *x;       // the x values of line segment endpoints
        // (the y values are in parameters[])
};


double Curvefit::evaluate()
{
    double sum = 0;
    // why does this loop go to n-2? Because i represents the left endpoint
    // of the line segment. There are n parameters, but only n-1 segments.
    for (int i = 0; i < n-1; i++) {
        sum += line_dist(i); // look up in cache or recompute each segment
    }
    return -sum; // return negative of distance so that bigger will be better
}


double Curvefit::line_dist(int i)
{
    if (p1_cache[i] == parameters[i] &&
        p2_cache[i] == parameters[i+1]) {
        // endpoints have not changed:
        return d_cache[i];
    }
    // otherwise, we need to recompute and save dist in cache
    double d = compute_dist(i);
    p1_cache[i] = parameters[i];
    p2_cache[i] = parameters[i+1];
    d_cache[i] = d;
    return d;
}


void Curvefit::setup(int segments)
{
    // number of parameters is greater than segments because the left
    // col of segment i is parameter i, so the right col of 
    // the last segment == parameter[segments].
    n = segments + 1;
    parameters = ALLOC(double, n);
    p1_cache = ALLOC(double, n);
    p2_cache = ALLOC(double, n);
    d_cache = ALLOC(double, n);
    x = ALLOC(double, n);
    step_size = ALLOC(double, n);
    min_param = ALLOC(double, n);
    max_param = ALLOC(double, n);
    int i;
    // ideal frames per segment
    float seg_length = ((float) (sa->file1_frames - 1)) / segments;
    for (i = 0; i < n; i++) { // initialize cache keys to garbage
        p1_cache[i] = p2_cache[i] = -999999.99;
        // initialize x values
        x[i] = ROUND(i * seg_length);
        // now initialize parameters based on pathx/pathy/time_map
        // time_map has y values for each x
        parameters[i] = sa->time_map[(int) x[i]];
        if (verbose)
            printf("initial x[%d] = %g, parameters[%d] = %g\n", 
                   i, x[i], i, parameters[i]);
        step_size[i] = 0.5;
        min_param[i] = 0;
        max_param[i] = sa->file2_frames - 1;
    }
}


// distance_rc -- look up or compute distance between chroma vectors
//     at row, col in similarity matrix
//
// Note: in current implementation, there is no stored representation
// of the matrix, so we have to recompute every time. It would be
// possible to store the whole matrix, but it's large and it would
// double the memory requirements (we already allocate the large
// PATH array in compare_chroma to compute the optimal path.
// 
// Since distance can be computed relatively quickly, a better plan
// would be to cache values along the path. Here's a brief design
// (for the future, assuming this routine is actually a hot spot):
// Allocate a matrix that is, say, 20 x file1_frames to contain distances
// that are +/- 10 frames from the path. Initialize cells to -1.
// Allocate an array of integer offsets of size file1_frames.
// Fill in the integer offsets with the column number (pathy) value of
// the path. 
// Now, to get distance_rc(row, col):
//    offset = offsets[row]
//    i = 10 + col - offset;
//    if (i < 0 || i > 20) /* not in cache */ return compute_distance(...);
//    dist = distances[20 * row + i];
//    if (dist == -1) { return distances[20 * row + i] = compute_distance...}
//    return dist;
//
double Curvefit::distance_rc(int row, int col)
{
    return gen_dist(row, col, sa->chrom_energy1, sa->chrom_energy2);
}


// compute distance from distance matrix using interpolation. A least
// one of x, y should be an integer value so interpolation is only 2-way
double Curvefit::distance_xy(double x, double y)
{
    int xi = (int) x;
    int yi = (int) y;
    if (xi == x) { // x is integer, interpolate along y axis
        double d1 = distance_rc(xi, yi);
        double d2 = distance_rc(xi, yi + 1);
        return interpolate(yi, d1, yi + 1, d2, y);
    } else if (yi == y) { // y is integer, interpolate along x axis
        double d1 = distance_rc(xi, yi);
        double d2 = distance_rc(xi + 1, yi);
        return interpolate(xi, d1, xi + 1, d2, x);
    } else {
        printf("FATAL INTERNAL ERROR IN distance_xy: neither x nor y is "
               "an integer\n");
        assert(false);
    }
}


double Curvefit::compute_dist(int i)
{
    double x1 = x[i], x2 = x[i+1];
    double y1 = parameters[i], y2 = parameters[i+1];
    double dx = x2 - x1, dy = y2 - y1;
    double sum = 0;
    int n;
    if (dx > dy) { // evauate at each x
        n = (int) dx;
        for (int x = (int) x1; x < x2; x++) {
            double y = interpolate(x1, y1, x2, y2, x);
            sum += distance_xy(x, y);
        }
    } else { // evaluate at each y
        n = (int) dy;
        for (int y = (int) y1; y < y2; y++) {
            double x = interpolate(y1, x1, y2, x2, y);
            sum += distance_xy(x, y);
        }
    }
    // normalize using line length: sum/n is average distance. Multiply
    // avg. distance (cost per unit length) by length to get total cost:
    double rslt = sqrt(dx*dx + dy*dy) * sum / n;
    // printf("compute_dist %d: x1 %g y1 %g x2 %g y2 %g sum %g rslt %g\n",
    //        i, x1, y1, x2, y2, sum, rslt);
    return rslt;
}

    
void curve_fitting(Scorealign *sa, bool verbose)
{
    if (verbose)
        printf("Performing line-segment approximation with %gs segments.\n", 
               sa->line_time);
    Curvefit curvefit(sa, verbose);
    double *parameters;
    double *x;
    // how many segments? About total time / line_time:
    int segments = 
        (int) (0.5 + (sa->actual_frame_period_1 * sa->file1_frames) /
                     sa->line_time);
    curvefit.setup(segments);
    curvefit.optimize();
    parameters = curvefit.get_parameters();
    x = curvefit.get_x();
    // now, rewrite pathx and pathy according to segments
    // pathx and pathy are generously allocated, so we can change pathlen
    // each segment goes from x[i], parameters[i] to x[i+1], parameters[i+1]
    int i;
    int j = 0; // index into path
    for (i = 0; i < segments; i++) {
        int x1 = (int) x[i];
        int x2 = (int) x[i+1];
        int y1 = (int) parameters[i];
        int y2 = (int) parameters[i+1];
        int dx = x2 - x1;
        int dy = y2 - y1;
        if (dx >= dy) { // output point at each x
            int x;
            for (x = x1; x < x2; x++) {
                sa->pathx[j] = x;
                sa->pathy[j] = (int) (0.5 + interpolate(x1, y1, x2, y2, x));
                j++;
            }
        } else {
            int y;
            for (y = y1; y < y2; y++) {
                sa->pathx[j] = (int) (0.5 + interpolate(y1, x1, y2, x2, y));
                sa->pathy[j] = y;
                j++;
            }
        }
    }
    // output last point
    sa->pathx[j] = (int) x[segments];
    sa->pathy[j] = (int) (0.5 + parameters[segments]);
    j++;
    sa->set_pathlen(j);
}



