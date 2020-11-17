# decorate_images.py
# Crops and drop-shadows images for the manual

from PIL import Image
from PIL import ImageFilter
import glob
import os

# Edit these to your image source and image destination paths.
src = "C:/OpenSourceGit/AudacityTeamTools/wit-html/trim_me"
dest = "C:/OpenSourceGit/AudacityTeamTools/wit-html/processed"

# from https://en.wikibooks.org/wiki/Python_Imaging_Library/Drop_Shadows
def makeShadow(image, iterations, border, offset, backgroundColour, shadowColour):
    # image: base image to give a drop shadow
    # iterations: number of times to apply the blur filter to the shadow
    # border: border to give the image to leave space for the shadow
    # offset: offset of the shadow as [x,y]
    # backgroundCOlour: colour of the background
    # shadowColour: colour of the drop shadow
    
    #Calculate the size of the shadow's image
    fullWidth  = image.size[0] + abs(offset[0]) + 2*border
    fullHeight = image.size[1] + abs(offset[1]) + 2*border
    
    #Create the shadow's image. Match the parent image's mode.
    shadow = Image.new(image.mode, (fullWidth, fullHeight), backgroundColour)
    
    # Place the shadow, with the required offset
    shadowLeft = border + max(offset[0], 0) #if <0, push the rest of the image right
    shadowTop  = border + max(offset[1], 0) #if <0, push the rest of the image down
    #Paste in the constant colour
    shadow.paste(shadowColour, 
                [shadowLeft, shadowTop,
                 shadowLeft + image.size[0],
                 shadowTop  + image.size[1] ])
    
    # Apply the BLUR filter repeatedly
    for i in range(iterations):
        shadow = shadow.filter(ImageFilter.BLUR)

    # Paste the original image on top of the shadow 
    imgLeft = border - min(offset[0], 0) #if the shadow offset was <0, push right
    imgTop  = border - min(offset[1], 0) #if the shadow offset was <0, push down
    shadow.paste(image, (imgLeft, imgTop))

    return shadow    

# crops on 3 sides.    
def crop3_one( image, d ) :            
    w,h = image.size
    image = image.crop( (d, 0, w-d, h-d) )
    return image

#crops on 4 sides
def crop4_one( image, d ) :            
    w,h = image.size
    image = image.crop( (d, d, w-d, h-d) )
    return image

# Take this one full path name, process it, output to dest directory.
def process_one( name ):
    global dest
    name = os.path.realpath( name )
    image = Image.open( name )

    #---- Start of image processing steps
    image = crop4_one( image, 4 )
    image = makeShadow( image, 3, 2, [2,2], (255,255,255), (80,80,80) )
    #---- End of image processing steps
    
    path, fname = os.path.split( name )
    print( "Name:", fname, " Size:", image.size )
    image.save( "/".join( (dest, fname )) )


# Process all .png images in directory
for filename in glob.iglob(src + '/*.png'):
    name = os.path.realpath( filename );
    process_one(name)


#process_one( 'C:/OpenSourceGit/AudacityTeamTools/wit-html/trim_3/AfterChirp.png' )
