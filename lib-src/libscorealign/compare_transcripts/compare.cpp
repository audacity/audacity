#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"

bool read_stuff(FILE *f, int &p, float &start, float &dur)
{
	int n, chan, vel;
	int c;
	while ((c = getc(f)) == '#') {
		while ((c = getc(f)) != '\n' && c != EOF) ;
	}
	ungetc(c, f);
	int fields = fscanf(f, "%d %d %d %d %f %f", &n, &chan, &p, &vel, &start, &dur);
	if (fields == EOF) {
		return false;
	} else if (fields != 6) {
		printf("Error scanning file\n");
		exit(1);
	}
	while ((c = getc(f)) != '\n' && c != EOF) ;
	return true;
}


void print_usage(char *progname)
{
	printf("%s file1 file2\n", progname);
}


int main(int argc, char *argv[])
{
	char *file1;
	char *file2;
	char *progname = strrchr(argv[0], '/');
	progname = progname ? progname + 1 : argv[0];
	if (argc < 3) {
		print_usage(progname);
		return 1;
	}
	file1 = argv[1];
	file2 = argv[2];
	FILE *f1 = fopen(file1, "r");
	FILE *f2 = fopen(file2, "r");
	int count = 0;
	float sum = 0.0;
	float sumsqr = 0.0;
	while (true) {
		int p1, p2;
		float start1, start2;
		float dur1, dur2;
		bool ok1 = read_stuff(f1, p1, start1, dur1);
		bool ok2 = read_stuff(f2, p2, start2, dur2);
		if (ok1 != ok2 || p1 != p2) {
			printf("Transcripts are not compatible\n");
			exit(1);
		}
		if (!ok1) break;
		count++;
		float diff = start2 - start1;
		sum += diff;
		sumsqr += diff * diff;
	}
	float avg = sum / count;
	float stddev = sqrt((sumsqr / count) - (avg * avg));
	printf("average error = %g\nstandard deviation = %g\n", avg, stddev);

	return 0;
}