iconsdatadir = $(datarootdir)/icons/hicolor
icons16dir = $(iconsdatadir)/16x16/apps
icons22dir = $(iconsdatadir)/22x22/apps
icons24dir = $(iconsdatadir)/24x24/apps
icons32dir = $(iconsdatadir)/32x32/apps
icons48dir = $(iconsdatadir)/48x48/apps
iconsscalabledir = $(iconsdatadir)/scalable/apps

dist_icons16_DATA = icons/16x16/audacity.png
dist_icons22_DATA = icons/22x22/audacity.png
dist_icons24_DATA = icons/24x24/audacity.png
dist_icons32_DATA = icons/32x32/audacity.png
dist_icons48_DATA = icons/48x48/audacity.png
dist_iconsscalable_DATA = audacity.svg

pixmapdir = $(datarootdir)/pixmaps
dist_pixmap_DATA = \
   gnome-mime-application-x-audacity-project.xpm \
   icons/16x16/audacity16.xpm \
   icons/32x32/audacity32.xpm \
   $(NULL)

pixmap_DATA = icons/48x48/audacity.xpm

# Copy AudacityLogo48x48.xpm to icons/48x48/audacity.xpm
# We could rename this file instead and adjust all locations.
icons/48x48/audacity.xpm: AudacityLogo48x48.xpm
	cp AudacityLogo48x48.xpm icons/48x48/audacity.xpm
EXTRA_DIST = AudacityLogo48x48.xpm
CLEANFILES = icons/48x48/audacity.xpm
