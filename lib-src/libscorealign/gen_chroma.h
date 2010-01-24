#define CHROMA_BIN_COUNT 12

bool is_midi_file(char *filename);

#define AREF2(chrom_energy, row, column) \
    (chrom_energy[row * (CHROMA_BIN_COUNT + 1) + column])
