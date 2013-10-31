# gawk script to convert CMake-generated Visual Studio projects into
# stand-alone project files
#
# Roger Dannenberg, October 2009
#
# the result uses relative path names (so you can install the project on 
# any machine and use it)
#
# NOTE: to run this, you must assign base_relative to the relative path
# from the vcproj file to portmidi, e.g. base_relative=.. or base_relative=.

BEGIN {
    state = "normal";
    #=================IMPORTANT====================
    # change the following path to the path in which
    # the CMakeLists.txt file resides:
    base_path = "C:\\\\Users\\\\rbd\\\\portmedia\\\\portmidi";
    #==============================================

    base_path_2 = base_path;
    gsub("\\\\\\\\", "/", base_path_2)
    cmake_stuff = 0; # flag to print <file> ... </file> text
}
# this case removes CMake phases from project
state == "cmakelists" {
    # print "IN CMAKELISTS " 
    file_text = file_text "\n" $0 # collect the <file> ... </file> section
    if (index($0, "CMakeLists.txt") > 0) { 
        cmake_stuff = 1 # remember to delete this <file> ... </file> section
    }

    if (index($0, "</File>") > 0) { 
        state = "normal";
        if (cmake_stuff == 0) {
            gsub(base_path, base_relative, file_text)
            gsub(base_path_2, base_relative, file_text)
            print file_text;
        }
        cmake_stuff = 0;
    };
    next
}

# this is the normal case, not in buildPhases list
state == "normal" {
    # print "NOT IN BUILD PHASES"
    # take out all the absolute paths
    gsub(base_path, base_relative, $0); 
    gsub(base_path_2, base_relative, $0); 
    # special processing for <file> ... </file> text:
    if ($0 ~ "<File$") {
        file_text = $0;
        cmake_stuff = 0; # innocent (keep text) until proven guilty
        state = "cmakelists";
        next # do not print this line
    };
    # THIS CODE WOULD ALLOW portmidi-static and portmidi-dynamic IN
    # pm_commmon. I DECIDED TO TRY PUTTING THEM IN SEPARATE DIRECTORIES
    # INSTEAD.
    # Use static libraries for everything except portmidi-dynamic
    #if (($0 ~ "RuntimeLibrary=") && (base_relative ~ "dynamic")) {
    #    if ($0 ~ 2) {
    #        $0 = "\t\t\t\tRuntimeLibrary=\"0\"";
    #    } else if ($0 ~ 3) {
    #        $0 = "\t\t\t\tRuntimeLibrary=\"1\"";
    #    }
    print $0;
    next
}

