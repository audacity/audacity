/*
 *  sautils.h
 *  scorealign
 *
 *  Created by Roger Dannenberg on 10/20/07.
 *  Copyright 2007 by Roger B. Dannenberg. All rights reserved.
 *
 */

#define ALLOC(t, n) (t *) malloc(sizeof(t) * (n))
#define FREE(p) free(p)

#define ROUND(x) ((int) (0.5 + (x)))

double interpolate(double x1, double y1, double x2, double y2, double x);


