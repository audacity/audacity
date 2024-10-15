/* main.h -- declarations of some command-line functions
 *
 * If VERBOSE is on in some files, some print functions are called.
 * Since these are only appropriate for the command-line interface,
 * there are some print functions declared in main.cpp. main.h
 * declares these functions for use in scorealign.cpp (and maybe others)
 *
 * 14-Jul-08  RBD
 */

void print_path_range(const short *pathx, const short *pathy, int i, int j);
void print_chroma_table(const float *chrom_energy, int frames);
