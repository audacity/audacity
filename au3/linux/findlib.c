// SPDX-License-Identifier: MIT
/* findlib.c
* Dynamically load a shared object file and print its full path.
* Part of library-utls - https://github.com/shoogle/library-utils
* Copyright (c) 2020 Peter Jonas
*/
#define _GNU_SOURCE
#include <link.h>
#include <dlfcn.h>
#include <libgen.h>
#include <string.h>
#include <stdio.h>

char* help
    ="Usage: %s LIBRARY\n"
     "Print the full path to LIBRARY if it is dynamically loadable by this program.\n"
     "\n"
     "Exit status codes:\n"
     "  0 - Success!\n"
     "  1 - Failed to load LIBRARY (or one of its dependencies).\n"
     "  2 - Other error(s) occurred.\n"
     "\n"
     "LIBRARY will fail to load if any of the following are true:\n"
     "  - The file is corrupt.\n"
     "  - It is not found in any of the library search paths (see `man ld.so`).\n"
     "  - It was compiled for a different architecture to that of this program.\n"
     "  - It depends (via DT_NEEDED) on another library that fails to load.\n"
     "\n"
     "Hint: You can use the `file` command to show the processor architecture that\n"
     "any ELF library or executable (including this program) was compiled for.\n";

int main(int argc, char* argv[])
{
    if (argc != 2) {
        fprintf(stderr, help, basename(argv[0]));
        return 2;
    }

    if ((strcmp(argv[1], "-h") == 0) || (strcmp(argv[1], "--help") == 0)) {
        printf(help, basename(argv[0]));
        return 0;
    }

    // dlopen enables us to load shared objects during execution
    void* lib = dlopen(argv[1], RTLD_LAZY);

    if (lib == NULL) {
        fprintf(stderr, "%s: %s\n", basename(argv[0]), dlerror());
        return 1;
    }

    struct link_map* map;

    // dlinfo can return information about objects loaded with dlopen
    if (dlinfo(lib, RTLD_DI_LINKMAP, &map) == -1) {
        fprintf(lib, "%s: %s\n", basename(argv[0]), dlerror());
        return 2;
    }

    printf("%s\n", map->l_name);
    return 0;
}
