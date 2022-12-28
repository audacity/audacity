import os
import shutil

def copy_file(source_file, destination_file):
    dest_exists = os.path.exists(destination_file)
    if not dest_exists or os.path.getmtime(source_file) > os.path.getmtime(destination_file):
        if dest_exists:
            os.remove(destination_file)
        print(f'Copying {source_file} to {destination_file}')
        shutil.copy2(source_file, destination_file)

def copy_files(files, destination):
    for filename, prefix in files:
        dest_dir = os.path.join(destination, prefix)
        dest_path = os.path.join(dest_dir, os.path.basename(filename))

        if not os.path.exists(dest_dir):
            os.makedirs(dest_dir)

        copy_file(filename, dest_path)
