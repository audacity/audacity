#define CHROMA_BIN_COUNT 12

bool is_midi_file(char *filename);

// index into matrix to extract chroma vector
#define AREF1(chrom_energy, row) \
    ((chrom_energy) + (row) * (CHROMA_BIN_COUNT + 1))

// index into matrix to extract element of chroma vector
#define AREF2(chrom_energy, row, column) AREF1(chrom_energy, row)[column]

