#include <Drag.h>
#include <Errors.h>
#include <TextEdit.h>
#include <QuickDraw.h>

extern TEHandle hTERec;
// Handle drag from newswatcher -EAD

/*----------------------------------------------------------------------------
    DragText
    
    Drag selected text.
    
    Entry:	ev = pointer to mouse down event record.
            where = location of mouse down event in local coords.
            theTE = handle to TextEdit record.
    
    Exit:	function result = error code.
            *dragged = 
                true if text was dragged.
                false if mouse down was not over text selection, or
                    user did not move the mouse before releasing the
                    mouse button.
            *trashed = true if text was dragged to trash.
----------------------------------------------------------------------------*/
extern RgnHandle rgn;
//extern EventRecord theEvent;

Boolean DragText (EventRecord *ev)
{
    DragReference dragRef;
    OSErr err = noErr;
    Boolean haveDragRef = false;
    Handle hText;
    RgnHandle dragRgn, tempRgn;
    short selStart, selEnd;
    char state;
    Point theLoc;
    GrafPtr curPort;
    
//	if (!PtInTEHiliteRgn(where, hTERec)) return noErr;
    if (!WaitMouseMoved(ev->where)) return noErr;
    
    GetPort(&curPort);
    
    CopyRgn(rgn, dragRgn = NewRgn());
    SetPt(&theLoc, 0, 0);
    LocalToGlobal(&theLoc);
    OffsetRgn(dragRgn, theLoc.h, theLoc.v);

    hText = (**hTERec).hText;
    selStart = (**hTERec).selStart;
    selEnd = (**hTERec).selEnd;
    
    err = NewDrag(&dragRef);
    if (err != noErr) goto exit;
    haveDragRef = true;
    state = HGetState(hText);
    HLock(hText);
    err = AddDragItemFlavor(dragRef, 1, 'TEXT', *hText + selStart, selEnd - selStart, 0);
    HSetState(hText, state);
    if (err != noErr) goto exit;
//	dragRgn = NewRgn();
//	err = TEGetHiliteRgn(dragRgn, hTERec);
//	if (err != noErr) goto exit;
//	LocalToGlobalRgn(dragRgn);
//	OutlineRegion(dragRgn);
    SetDragItemBounds(dragRef, 1, &(**dragRgn).rgnBBox);
    tempRgn = NewRgn();
    CopyRgn(dragRgn, tempRgn);
    InsetRgn(tempRgn, 1, 1);
    DiffRgn(dragRgn, tempRgn, dragRgn);
    DisposeRgn(tempRgn);

    err = TrackDrag(dragRef, ev, dragRgn);
    if (err != noErr && err != userCanceledErr) goto exit;
    //*trashed = DragTargetWasTrash(dragRef);
//	DisposeRgn(dragRgn);

    DisposeDrag(dragRef);
    return true;
    
exit:

    if (haveDragRef) DisposeDrag(dragRef);
//	if (dragRgn != nil) DisposeRgn(dragRgn);
    return false;
}



/*----------------------------------------------------------------------------
    LocalToGlobalRgn 
    
    Convert a region from local to global coordinates.
            
    Entry:	rgn = handle to region.
----------------------------------------------------------------------------*/

void LocalToGlobalRgn (RgnHandle rgn)
{
    Point where;
    
    SetPt(&where, 0, 0);
    LocalToGlobal(&where);
    OffsetRgn(rgn, where.h, where.v);
}

/*----------------------------------------------------------------------------
    OutlineRegion 
    
    Change a region into a tracing of its border which is appropriate 
    for normal dragging.
            
    Entry:	theRgn = handle to region.
    
    Exit:	Region changed to outline of region.
    
    From Apple "HFS Drag Sample" sample code.
----------------------------------------------------------------------------*/

void OutlineRegion (RgnHandle theRgn)
{
    RgnHandle tempRgn;
    
    tempRgn = NewRgn();
    CopyRgn(theRgn, tempRgn);
    InsetRgn(tempRgn, 1, 1);
    DiffRgn(theRgn, tempRgn, theRgn);
    DisposeRgn(tempRgn);
}

/*----------------------------------------------------------------------------
    PtInTEHiliteRgn 
    
    Determine whether or not a point is in the current TextEdit hilite
    region.
            
    Entry:	where = point in local coords.
            theTE = handle to TextEdit record.
            
    Exit:	function result = true if point is in the hilite region.
----------------------------------------------------------------------------*/

Boolean PtInTEHiliteRgn (Point where, TEHandle theTE)
{
    Boolean result = false;
    RgnHandle rgn = nil;
    OSErr err = noErr;
    
    //if (!HaveTEGetHiliteRgn()) return false;
    rgn = NewRgn();
    err = TEGetHiliteRgn(rgn, theTE);
    if (err != noErr) goto exit;
    result = PtInRgn(where, rgn);
    
exit:

    if (rgn != nil) DisposeRgn(rgn);
    return result;
}
