sizes = [
  '16x16',
  '22x22',
  '24x24',
  '32x32',
  '48x48',
]

foreach size : sizes
  install_data(join_paths('icons', size, 'audacity.png'),
    install_dir: join_paths(get_option('datadir'), 'icons', 'hicolor', size, 'apps'),
  )
endforeach

install_data('audacity.svg',
  install_dir: join_paths(get_option('datadir'), 'icons', 'hicolor', 'scalable', 'apps'),
)

install_data([
    'icons/16x16/audacity16.xpm',
    'icons/32x32/audacity32.xpm',
    'icons/48x48/audacity.xpm',
    'gnome-mime-application-x-audacity-project.xpm',
  ],
  install_dir: join_paths(get_option('datadir'), 'pixmaps'),
)
