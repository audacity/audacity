# docimages_all.py
# Sends commands to get images for the manual.
# Execs all the docimage scripts.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

import time


# records time, name of file and returns contents.
def inner( name ) :
    global old_name
    global start_time
    global results
    result = old_name + ' took ' + str( time.time() - start_time )
    results.append( result )
    print( result )
    start_time = time.time()
    if not name :
        return ""
    old_name = name
    return open("docimages_" + name + ".py" ).read()

#initialise timing
start_time = time.time()
old_name = 'startup'
results = []

#do the different files...
exec( inner( 'tracks' ) )
exec( inner( 'labels' ) )
exec( inner( 'spectro' ) )
exec( inner( 'after' ) )
exec( inner( 'envelopes' ) )
exec( inner( 'cut_n_paste' ) )
exec( inner( 'clip_boundaries' ) )
exec( inner( 'oddments' ) )
exec( inner( 'named_tracks' ) )
exec( inner( 'arrange' ) )

#report on timing.
inner( "" )

print( "\n\nSummary:" )
print( "\n".join( results ) )


