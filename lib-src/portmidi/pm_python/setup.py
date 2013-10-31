import sys
import os
import logging

from distutils.core import setup, Command
from distutils.extension import Extension
try:
    from Cython.Distutils import build_ext
except ImportError:
    logging.warn("Cython is preferred over pyrex for python3 compatibility.")
    from Pyrex.Distutils import build_ext





DESCRIPTION = open('README_PYTHON.txt').read()
CHANGES = open('CHANGES.txt').read()
TODO = open('TODO.txt').read()

EXTRAS = {}

long_description = DESCRIPTION + CHANGES + TODO
#import sys
#if "checkdocs" in sys.argv:
#    print long_description


METADATA = {
    'name':             'pyportmidi',
    'version':          '0.0.7',
    'license':          'MIT License',
    'url':              'http://pypi.python.org/pyportmidi/',
    'author':           'John Harrison, Roger B. Dannenberg, Rene Dudfield, others...',
    'author_email':     'renesd@gmail.com',
    'maintainer':       'Rene Dudfield',
    'maintainer_email': 'renesd@gmail.com',
    'description':      'Python Wrappings for PortMidi #python.  CHANGES: new package layout.',
    'long_description': long_description,
    'classifiers':      [
            'Development Status :: 2 - Pre-Alpha',
            'Intended Audience :: Developers',
            'Intended Audience :: Information Technology',
            'License :: OSI Approved :: BSD License',
            'Operating System :: MacOS :: MacOS X',
            'Operating System :: Microsoft :: Windows',
            'Operating System :: POSIX :: Linux',
            'Programming Language :: Cython',
            'Programming Language :: C',
            'Programming Language :: Python :: 2',
            'Programming Language :: Python :: 2.5',
            'Programming Language :: Python :: 2.6',
            'Programming Language :: Python :: 2.7',
            'Programming Language :: Python :: 3',
            'Programming Language :: Python :: 3.0',
            'Programming Language :: Python :: 3.1',
            'Programming Language :: Python :: 3.2',
            'Topic :: Multimedia :: Sound/Audio :: MIDI',
            'Topic :: Software Development :: Libraries',
    ],
}


if "bdist_msi" in sys.argv:
    # hack the version name to a format msi doesn't have trouble with
    METADATA["version"] = METADATA["version"].replace("pre", "a0")
    METADATA["version"] = METADATA["version"].replace("rc", "b0")
    METADATA["version"] = METADATA["version"].replace("release", "")





# allow optionally using setuptools for bdist_egg.
using_setuptools = False

if "-setuptools" in sys.argv:
    using_setuptools = True

    from setuptools import setup, Command
    sys.argv.remove ("-setuptools")

    EXTRAS.update({'include_package_data': True,
                   'install_requires': [],
                   'zip_safe': False,
                   'test_suite' : 'pyportmidi.tests',
                   }
    )


# test command.  For doing 'python setup.py test'
class TestCommand(Command):
    user_options = [ ]

    def initialize_options(self):
        self._dir = os.getcwd()

    def finalize_options(self):
        pass

    def run(self):
        '''
        runs the tests with default options.
        '''
        import pyportmidi.tests
        pyportmidi.tests.main()

        #import subprocess
        #return subprocess.call([sys.executable, "run_tests.py"])


cmdclass = {'build_ext': build_ext}

# we use our test command.
if not using_setuptools:
    import os
    cmdclass['test'] = TestCommand



scripts = []

PACKAGEDATA = {
    'cmdclass':    cmdclass,

    'package_dir': {'pyportmidi': 'pyportmidi',
                    #'pyportmidi.tests': 'test',
                    #'pyportmidi.docs': 'docs',
                    #'pyportmidi.examples': 'examples',

                   },
    'packages': ['pyportmidi',
                 'pyportmidi.tests', 
                ],
    'scripts': scripts,
}


PACKAGEDATA.update(METADATA)
PACKAGEDATA.update(EXTRAS)



if sys.platform == 'win32':
    print "Found Win32 platform"
    EXTENSION = dict(
        ext_modules=[ 
            Extension("pyportmidi._pyportmidi", [os.path.join("pyportmidi", "_pyportmidi.pyx")],
                      library_dirs = ["../Release"],
                      libraries = ["portmidi", "winmm"],
                      include_dirs = ["../porttime"],
#                  define_macros = [("_WIN32_", None)]) # needed by portmidi.h
                      extra_compile_args = ["/DWIN32"]) # needed by portmidi.h
        ]
    )
elif sys.platform == 'darwin':
    print "Found darwin (OS X) platform"
    library_dirs = ["/usr/local/lib"]
    include_dirs = ["/usr/local/include"]
    EXTENSION = dict(
        ext_modules=[ 
            Extension("pyportmidi._pyportmidi", [os.path.join("pyportmidi", "_pyportmidi.pyx")],
                      library_dirs = library_dirs,
                      include_dirs = include_dirs,
                      libraries = ["portmidi"],
                      extra_link_args=["-framework", "CoreFoundation",
                                       "-framework", "CoreMIDI",
                                       "-framework", "CoreAudio"])
        ]
    )
else:
    print "Assuming Linux platform"
    EXTENSION = dict(
        ext_modules=[ 
            Extension("pyportmidi._pyportmidi", [os.path.join("pyportmidi", "_pyportmidi.pyx")],
                      library_dirs=["./linux"],
                      libraries = ["portmidi", "asound", "pthread"]
                      )
        ]
       
    )

PACKAGEDATA.update(EXTENSION)

setup(**PACKAGEDATA)
