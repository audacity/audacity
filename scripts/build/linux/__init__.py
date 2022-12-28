__all__ = [
    "objdump",
]

from . import objdump

def collect_dependencies(binaries, additional_paths):
    return objdump.collect_dependencies(binaries, additional_paths)
