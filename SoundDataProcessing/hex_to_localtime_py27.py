import time
import os
import shutil
import fnmatch


def main():

    from_dir = raw_input('Directory to copy from: ')
    site = raw_input('Site name (no spaces): ')
    os.mkdir(from_dir + '_local_time')
    to_dir = from_dir + '_local_time'
    original_dir = os.listdir(from_dir)

# copy files to new directory
    for item in original_dir:
        for root, dirs, files in os.walk(from_dir):
            for basename in files:
                if fnmatch.fnmatch(basename, item):
                    full_path_filename = os.path.join(root, basename).replace('\\', '/')
                    try:
                        shutil.copy2(full_path_filename, to_dir)
                    except shutil.SameFileError:
                        pass

# find and copy files in new directory
    new_dir = os.listdir(to_dir)
    for item in new_dir:
        for root, dirs, files in os.walk(to_dir):
            for basename in files:
                if fnmatch.fnmatch(basename, item):
                    absolute_path = os.path.join(root, basename).replace('\\', '/')
    # change file name
        file = os.path.splitext(absolute_path)[0]
        am_format = os.path.split(file)[1]
        py_hex = '0x' + am_format
        local_time = time.strftime('%Y-%m-%d_%H-%M', time.localtime(int(py_hex, 16)))
        new_file_name = site + '-' + local_time + '.WAV'
        dst = to_dir + '\\' + new_file_name
        src = to_dir + '\\' + item
        os.rename(src, dst)


main()
