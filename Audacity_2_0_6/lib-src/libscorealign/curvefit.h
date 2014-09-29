/*
 *  curvefit.h
 *  scorealign
 *
 *  Created by Roger B. Dannenberg on 10/20/07.
 *  Copyright 2007 by Roger B. Dannenberg. All rights reserved.
 *
 * Dynamic programming does a good job of getting a rough alignment
 * that is very good in a global sense, but there are often short-term
 * "digressions" where the optimal path wanders off the "true" path.
 * These digressions are hard to correct with simple smoothing. This
 * module is intended to assert a "steady tempo" constraint to improve
 * the path. It starts with the dynamic programming path, which is likely
 * to be close to the correct path. The DP path (in pathx[] and pathy[])
 * is divided evenly into segments of approximately line_time seconds
 * along the x axis. For a segment from x1 to x2, linear regression is 
 * performed on the DP path from x1 to x2. This specifies an initial
 * line segment. Next, the end-points are joined by averaging: if
 * the segment from x1 to x2 ends at y-end and the segment from x2 to x3
 * starts at y-start, then the end of line x1--x2 and the beginning of
 * line x2--x3 are adjusted to (y-end + y-start)/2. Now the fun starts:
 * the endpoints of all the lines are adjusted up and down in order to 
 * minimize a distance function. The distance function estimates the
 * integral of the distance matrix value along the line. Since the line
 * falls between discrete points in the matrix, interpolation is used.
 * The end result is converted back into a discrete path. (Maybe in the 
 * future, the pathx[]/pathy[] representation should be generalized to
 * allow for non-integer path coordinates.) The resulting path will
 * have steady tempo at least within each segment. What I hope will
 * happen is that when there are chord changes or melody changes, there
 * will be "narrow" pathways in the distance matrix. Getting the 
 * alignment wrong at these transitions will cost a lot. Other places
 * are not so critical, which is why I think DP wanders off the true
 * path. The straight-line path will ensure that for the most part, the
 * score alignment is determined by the transitions, and where alignment
 * is not critical, the alignment will avoid any rubato or over-fitting.
 */

void curve_fitting(Scorealign *sa, bool verbose);

