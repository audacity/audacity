import os
import shutil
import time

def safe_rm_tree(path):
    if not os.path.isdir(path):
        return

    def onerror(func, path, exc_info):
        import stat

        # Is the error an access error?
        if not os.access(path, os.W_OK):
            print(f"Failed to remove `{path}`. Trying to change permissions...")
            os.chmod(path, stat.S_IWUSR)
            func(path)
        else:
            raise OSError("Cannot change permissions for {}! Exception info: {}".format(path, exc_info))

    for i in range(20):
        try:
            shutil.rmtree(path, onerror=onerror)
            return
        except Exception as e:
            delay = 0.5 * float(i + 1)
            print(f"Failed to remove `{path}`: `{e}`. Retrying in {delay} seconds...")
            time.sleep(delay)
