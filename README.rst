eztool -- Control and communicate with a Cypress EZ-USB
=======================================================

**eztool** is a command line tool to control and to communicate with Cypress
EZ-USB AN2131 microcontrollers via USB. More precisely, it is a tool to
communicate with *USB devices*, which have an EZ-USB microcontroller. It can
also communicate to any USB device in a custom user mode.

License
-------

    Copyright (C) 2012 Johann Glaser <Johann.Glaser@gmx.at>

    This program is free software; you can redistribute it and/or modify  
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or  
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

References
----------

**pas-readline**
  Object Oriented wrapper for GNU Readline for Pascal
  https://github.com/hansiglaser/pas-readline

**pas-tcl**
  Object Oriented wrapper for TCL for Pascal
  https://github.com/hansiglaser/pas-tcl

**pas-libusb**
  Object Oriented wrapper for LibUSB
  https://github.com/hansiglaser/pas-libusb

**ezusb-firmware**
  Firmware skeleton for the Cypress EZ-USB microcontroller
  https://github.com/hansiglaser/ezusb-firmware.git

Directory Structure
-------------------

  ``host/``
    Host application base directory

  ``host/src/``
    Host applicatione source code. This directory has a ``Makefile``.

  ``host/pas-readline``
    Git Submodule: Object Oriented wrapper for GNU Readline for Pascal

  ``host/pas-tcl``
    Git Submodule: Object Oriented wrapper for TCL for Pascal

  ``host/pas-libusb``
    Git Submodule: Object Oriented wrapper for LibUSB

  ``firmware``
    EZ-USB device firmware. This directory has a ``Makefile``.

Build
-----

The host application is written in Pascal using `FreePascal
<http://www.freepascal.org/>`_ 2.6.0 on Linux.

To compile the firmware, the `SDCC compiler package
<http://sdcc.sourceforge.net/>`_ is required. Most Linux
distributions include SDCC in their official package repositories. The SDCC
source code can be found at http://sdcc.sourceforge.net/
Simply type "make hex" in the firmware directory to compile.
"make clean" will remove all generated files except the Intel HEX file required
for downloading the firmware to the EZ-USB device.

::

  $ cd firmware/
  $ make

This builds the device firmware which is used by the host application in the
"EZTool" mode.

::

  $ cd ../host/src
  $ make

This builds the host application as well as the man-pages for each command and
the host application itself.

Usage
-----

Execute ``eztool`` and then type ``man eztool`` for more information.
Alternatively you can directly read its man page using

::

  $ man host/src/man/man1/eztool.1


Git Submodules
--------------

The projects ``pas-readline``, ``pas-tcl`` and ``pas-libusb`` are referenced using `Git
Submodules <http://git-scm.com/book/en/Git-Tools-Submodules>`_. To clone
this project, you have to add the submodules too.

::

  git clone https://github.com/hansiglaser/eztool.git
  cd eztool
  git submodule init
  git submodule update

To update a submodule with its most up-to-date state, two steps are necessary.

::

  cd ./host/pas-tcl
  #git merge origin/master    # to get changes from a branch
  git pull                   # to get the newest revision
  cd ../..
  git add host/pas-tcl       # to stage the update
  git commit                 # to update the pointer

To change a submodule within this main project and then commit and push to
GitHub, a few things must be `considered <http://longweekendmobile.com/2010/11/05/making-changes-in-a-git-submodule-made-simple/>`_.
A submodule by default is a 'Detached Head' this means it isn't on a branch.

::

  git branch libusb-1.0      # switch to a branch
  # make changes
  git add ...                # stage changes
  git commit                 # commit
  git push                   # and push to GitHub
  cd ../..
  git submodule              # shows that submodules are at a newer state than
                             # referenced by the main project
  git add host/pas-libusb    # tell git to use the most current revision of
                             # this submodule
  git commit                 # commit
  git push                   # and push to GitHub


If changes in a submodule within this main project were made and committed,
some hand crafting is necessary, becaus the commit created a revision
detached from "master".

::

  git submodule              # shows a list of submodules including their SHA-1
  cd host/pas-tcl
  git checkout master        # switch to master branch
  git cherry-pick 8ec7179    # and get all the changes from the "wild" commit
  git push                   # now they can be pushed to GitHub
  cd ../..
  git submodule              # shows that submodules are at a newer state than
                             # referenced by the main project
  git add host/pas-tcl       # tell git to use the most current revision of
                             # this submodule
  git commit                 # commit
  git push                   # and push to GitHub

See als
 - http://blog.jacius.info/2009/08/09/your-git-submodule-and-you/
 - http://longweekendmobile.com/2010/11/05/making-changes-in-a-git-submodule-made-simple/
 - https://git.wiki.kernel.org/index.php/GitSubmoduleTutorial

EZUSB Firmware
--------------

The firmware can either be used with the `git subtree merge
<http://git-scm.com/book/en/Git-Tools-Subtree-Merging>`_ or simply by
downloading the current release from GitHub.

::

  git clone https://github.com/hansiglaser/ezusb-firmware.git firmware
  cd firmware/
  rm -rf .git
  cd ..
  git add firmware

