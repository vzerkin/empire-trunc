import os

if __name__ == "__main__":
    if os.uname()[0] == "Linux":    os.symlink( "zvv2-1.020-lin.exe", "zvview.exe" )
    elif os.uname()[0] == "Darwin": os.symlink( "zvv2-1.020-mac.exe", "zvview.exe" )
    else:                           os.symlink( "zvv2-1.005-win.exe", "zvview.exe" )
