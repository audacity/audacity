/*
 * This file should be included in all files that use
 * the HASHENTRY macro; see hashrout.h for details
 */
/* Copyright 1989 Carnegie Mellon University */

#ifndef    HASHTYPE
#    include    "-- HASHTYPE undefined"
#endif

/*
 * An element really is a HASHTYPE along with a h_next entry,
 * which chains together entries of the same hash value.
 */

typedef struct hashelem {
    HASHTYPE    h_elem;
    struct hashelem     *h_next;
} hashelem;

extern hashelem    hashfirstchunk[];

#define HASHENTRY(i)    (hashfirstchunk[i].h_elem)
