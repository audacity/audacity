
#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sndfile.h>


static void *
get_cues (const char *filename, double *sr)
{
    SNDFILE    *file;
    SF_INFO	sfinfo;

    unsigned int err, size;
    uint32_t count = 0;
    SF_CUES_VAR(0) *info;

    if ((file = sf_open(filename, SFM_READ, &sfinfo)) == NULL)
    {
	printf("can't open file '%s'\n", filename);
	exit(1);
    }

    printf("\n---- get cues of file '%s'\n", filename);

    if ((err = sf_command(file, SFC_GET_CUE_COUNT, &count, sizeof(uint32_t))) == SF_FALSE)
    {
	if (sf_error(file))
	{
	    printf("can't get cue info size for file '%s' (arg size %lu), err %s\n", 
		   filename, sizeof(uint32_t), sf_strerror(file));
	    exit(2);
	}
	else
	    printf("no cue info for file '%s'\n", filename);
	return NULL;
    }
	
    size = sizeof(*info) + count * sizeof(SF_CUE_POINT);
    printf("number of cues %d  size %d\n", count, size);

    if (!(info = malloc(size)))
	return NULL;

    if (sf_command(file, SFC_GET_CUE, info, size) == SF_FALSE)
    {
	printf("can't get cue info of size %d for file '%s' error %s\n", 
	       size, filename, sf_strerror(file));
	exit(3);
    }

    *sr = sfinfo.samplerate;
    sf_close(file);

    return info;
}


static void
test_cues (const char *filename)
{
    unsigned int i;
    double sr;
    SF_CUES_VAR(0) *info = get_cues(filename, &sr);

    if (info == NULL)
	exit(1);
    
    for (i = 0; i < info->cue_count; i++)
    {
	int    pos = info->cue_points[i].position;
	double t   = (double) pos / sr;
	double expected = i < 8  ?  (double) i / 3.  :  10. / 3.;
	double error = (double) fabs(t - expected);

	printf("cue %02d: markerID %02d  position %6d  offset %6d (time %.3f  expected %.3f  diff %f)  label '%s'\n",
	       i, info->cue_points[i].indx, pos, info->cue_points[i].sample_offset, t, expected, error, info->cue_points[i].name);

	if (error > 0.025)
	    exit(4);
    }

    free(info);
}

static void
print_cues (const char *filename)
{
    unsigned int i;
    double sr;
    SF_CUES_VAR(0) *info = get_cues(filename, &sr);

    if (info == NULL)
	exit(1);
    
    for (i = 0; i < info->cue_count; i++)
    {
	int    pos    = info->cue_points[i].position;
	int    indx   = info->cue_points[i].indx;
	int    cstart = info->cue_points[i].chunk_start;
	int    bstart = info->cue_points[i].block_start;
	int    offset = info->cue_points[i].sample_offset;
	const char *name = info->cue_points[i].name;
	double t   = (double) pos / sr;

	if (cstart != 0  ||  bstart != 0)
	    printf("cue %02d time %7.3f: markerID %02d  position %8d  chunk_start %d  block_start %d  offset %8d  label '%s'\n",
		   i, t, indx, pos, offset, cstart, bstart, name);
	else
	    printf("cue %02d  time %7.3f: markerID %02d  position %8d  offset %8d  label '%s'\n",
		   i, t, indx, pos, offset, name);
    }

    free(info);
}


int
main (int argc, char **argv)
{
    int i;
    
    if (argc > 1)
	for (i = 1; i < argc; i++)
	    print_cues(argv[i]);
    else
    {
	test_cues("clickpluck24.wav");
	test_cues("clickpluck.wav");
	test_cues("clickpluck.aiff");
    }
    return 0;
}
