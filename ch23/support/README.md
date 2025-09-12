# Support for Chapter 23: GUI programming with gtk2hs

This directory contains all that is needed to compile and run the application
from the chapter 23.

The starting point was that I wasn't able to install the `glade` hackage package
in a recent Haskell environment, so I picked the most recent older GHC version
the `glade` library was tested with (version 8.0.1) and aligned everything else
to it.

It was created and tested on Slackware64 15.0 Linux distribution, but it should
be possible to use it as a guide for running the chapter 23 on other Linux
distributions or maybe even other operating systems. For full reproducibility,
Slackware64 15.0 is assumed.

There was still a need to do minor changes to the code of the chapter. [The
patch](ch23.patch) is included in this directory.

## The result

In the terminal, you can see patching the original code of the chapter 23,
activating the environment, compiling and running the `PodLocalMain`
application. There is also a GTK 2 Glade editor running.

![Running PodLocalMain and GTK 2 Glade designer](ch23.png?raw=true)

## The main.sh script

The main script automates installation of the environment. It
* downloads all binary and source archives needed
* verifies SHA256 sum of every downloaded archive
* installs the GHC compiler
* installs libraries related to GTK 2
* installs the Haskell packages
* installs the Glade designer for GTK 2 for the full experience :-)
* creates a bash script for activating the installed environment.

The script installs everything in a single directory. It doesn't modify anything
outside of it. Uninstallation is just removing that directory.

## Steps

The steps for getting the chapter 23 to run are
1. Installing the environment
2. Activating the environment
3. Patching and compiling the application

See the terminal in the screenshot above for the activation, patching, and compiling.

### Installation

1. Install Slackware64 15.0. The DVD image is at [the Slackware ISO
mirrors](https://mirrors.slackware.com/slackware/slackware-iso/slackware64-15.0-iso/). It
contains the software needed. I installed the system in a virtual machine. For
simplicity I did full installation. It needs about 15 GB, so give the virtual
system at least 25 GB to have enough space for the downloaded and installed
files. If you are not a Slackware user, for booting into the graphical environment
   1. log in as root
   2. run `adduser` to add an unprivileged user
   3. edit `/etc/inittab` (e.g., by `nano` editor) and change the
   `id:3:initdefault:` line to `id:4:initdefault:`
   4. reboot the system

2.  Get this directory. You can copy it into the virtual machine via a directory
shared with the host OS or git-clone it from inside.
4.  Run the `main.sh` script inside this directory. It creates a root directory
in the current directory and installs all files there.

### Activation

The activation script `env` is in the root directory created by the `main.sh` in
`DESTDIR`. Source it in your bash session. It sets environment variables (PATH,
LD_LIBRARY_PATH) to include and prefer the installed GHC version, libraries, and
executables.

This is done every time you want to compile and run the chapter 23. Running
other programs may not work because of the overriden paths to the libraries. Run
those in a normal shell session without the environment activated.

### Patching the code of chapter 23

Go to the directory with code for the chapter and run `patch -p1 < /path/to/the/ch23.patch`.

### Compiling and running the application

With the environment activated, run
```
> ghc PodLocalMain.hs
> ./PodLocalMain
```

## GTK 2 Glade designer

The designer is run by `glade-3` command when the environment is activated. Note
that running just `glade` command runs the GTK 3 Glade designer included in
Slackware64 15.0 which cannot edit GTK 2 glade files.
