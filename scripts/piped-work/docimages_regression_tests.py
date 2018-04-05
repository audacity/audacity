# docimages_regression_tests.py
# Sends commands to get images for the manual.
# Tests against bug numbers to confirm that they are still fixed.

# Make sure Audacity is running first and that mod-script-pipe is enabled
# before running this script.

#load and run the common core.
exec( open("docimages_core.py" ).read() )


def bug_test_1844():
    makeWayForTracks()
    do( 'Select: Start=0 End=30' )
    do( 'Silence' )
    do( 'Select: Start=0 End=40' )
    do( 'ZoomSel' )
    do( 'Select: Start=0 End=30' )
    do( 'Tone' )
    do( 'EnvelopeTool' )
    do( 'SetEnvelope: Time=10 Value=1.0' )
    do( 'SetEnvelope: Time=12 Value=0.5' )
    do( 'Select: Start=0 End=4' )
    do( 'AdjustableFade: curve="0" gain0="0" gain1="100" preset="None" type="Up" units="% of Original" ' )
    capture( "Bug1844.png", "AllTracks" )
    do( 'SelectTool' )
        
# Should have envelop points at 0s, 2x4s, 10s and 12s and no others.
bug_test_1844()

