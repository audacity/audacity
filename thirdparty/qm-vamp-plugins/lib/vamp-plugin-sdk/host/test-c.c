
#include <vamp-hostsdk/host-c.h>

#include <stdio.h>

int main(int argc, char **argv)
{
    int i;
    int libcount = vhGetLibraryCount();

    printf("Vamp plugin libraries found:\n");
    for (i = 0; i < libcount; ++i) {
	printf("%d: %s\n", i, vhGetLibraryName(i));
    }

    printf("Going to try loading qm-vamp-plugins...\n");
    int libindex = vhGetLibraryIndex("qm-vamp-plugins");
    vhLibrary lib = vhLoadLibrary(libindex);
    if (!lib) {
	printf("Failure!\n");
	return 1;
    }

    int plugincount = vhGetPluginCount(lib);
    printf("Success: it contains %d plugins; they are:\n", plugincount);

    for (i = 0; i < plugincount; ++i) {
	const VampPluginDescriptor *descriptor = vhGetPluginDescriptor(lib, i);
	if (!descriptor) {
	    printf("<unknown! failed to load>\n");
	} else {
	    printf("%s\n", descriptor->identifier);
	}
    }

    vhUnloadLibrary(lib);
    
    return 0;
}

