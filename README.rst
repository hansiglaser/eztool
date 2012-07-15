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

These projects are referenced using `Git Submodules
<http://git-scm.com/book/en/Git-Tools-Submodules>`_. To clone this project,
you have to add the submodules too.

::

  git clone https://github.com/hansiglaser/eztool.git
  cd eztool
  git submodule init
  git submodule update

To update a submodule with its most up-to-date state, two steps are necessary.

::

  cd ./host/pas-tcl
  git merge origin/master    # to get teh newest revision
  cd ../..
  git add host/pas-tcl       # to stage the update
  git commit                 # to update the pointer



