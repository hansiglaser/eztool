(*ronn
eztool(1) -- Control and communicate with a Cypress EZ-USB
===========================================================

## SYNOPSYS

`eztool` [`-h`|`--help`] [`-b`] [`-f` <filename>] [`-c` <string>] [`-n`]

## DESCRIPTION

`eztool` is a command line tool to control and to communicate with Cypress
EZ-USB AN2131 microcontrollers via USB. More precisely, it is a tool to
communicate with _USB devices_, which have an EZ-USB microcontroller. It can
also communicate to any USB device in a custom user mode.

## APPLICATION MODES

The operation of `eztool` is organized in **application modes**:

 - Disconnected
 - Empty
 - EZTool
 - User

In `Disconnected Mode`, general information on USB devices (`lsusb`(1ez)) is
available. By `connect`(1ez)ing to a device, the mode is changed, according to
the device type.

`Empty Mode` provides features as offered by the EZ-USB vendor request
Firmware Load. The 8051 CPU can be `reset`(1ez) and its XRAM can be read
(`xread`(1ez)) and written (`xwrite`(1ez)). The command `download`(1ez) offers
to directly download firmware from an Intel Hex file.

The `EZTool Mode` downloads a firmware supplied with this tool. This allows
more control over the features of the EZ-USB chip, which include access to the
IO ports (`iosetup`(1ez), `ioset`(1ez), `ioget`(1ez)), the external I2C EEPROM
(`eeread`(1ez), `eewrite`(1ez)) and the internal XRAM (`xread`(1ez),
`xwrite`(1ez)). Generic I2C routines (`i2cread`(1ez) and `i2cwrite`(1ez)) are
available to communicate with any I2C device.

In the `User Mode`, direct USB communication with the USB device is offered.
This does not require an EZ-USB chip, but allows all kinds of USB transfers
(`controlmsg`(1ez), `bulkin`(1ez), `bulkout`(1ez)). Note that the USB interface
has to be `claim`(1ez)ed before use.

## USER INTERFACE

`eztool` offers a convenient _command line_ interface. It uses _Tcl_ as
scripting language and command interpreter. Actually, every command you enter
is implemented as a Tcl command (but written in Pascal and compiled in the
executable). The Tcl programming language offers unlimited options to use,
customize and automate `eztool`.

`eztool` prints a prompt at the screen which signals the current mode. To
wait for the user input, _GNU Readline_ is used. This offers comforable command
line editing, history and auto-completion.

For each Tcl command and variable a dedicated `man`(1ez)ual page is provided.
The sub-extension `ex` is used.

To quit `eztool` use `exit`(1ez) or type [Ctrl]-[D].

## OPTIONS

  * `-h`, `--help`:
    Print a usage information and exit.

  * `-b`:
    Batch mode. `eztool` will not wait for user input but quit directly after
    the startup scripts, the scripts given with `-f` parameters and the commands
    given with `-c` parameters were executed.

  * `-f` <filename>:
    Execute the Tcl script in _filename_ at program start.

  * `-c` <string>:
    Execute the commands given in _string_.

  * `-n`:
    Do not execute the start scripts (/etc/eztool/... TODO )

## STARTUP SCRIPTS

`eztool` searches and executes startup scripts at program start, which are
_/etc/eztool/eztoolrc_, _/etc/eztool/eztool.d/*.tcl_ (in alphabetical order)
and _~/eztoolrc_ in that order. If the option `-n` is given, none of these
startup scripts are executed.

Next, if the option `-f` _filename_ is given, the Tcl script _filename_ is
executed. The Tcl commands supplied with the option `-c` _string_ are also
executed. These scripts and commands are executed in the order of the options
on command line.

Finally, the user is prompted to input his commands, unless the option `-b`
is given, which immediately quits the program after all scripts and commands
are executed.

## COMMANDS

Note that some commands are only available in certain modes.

**Common commands**
     help [word]
     man [page]
     exit [exitcode]
     history
     lsusb [-a|...]
     disconnect
     devinfo

**Disconnected Mode**
     connect [-empty|-eztool|-user] [idVendor:idProduct]

**Empty Mode**
     reset [-toggle|-keep|-release]
     xread
     xwrite
     download [firmware.hex idVendor:idProduct] [-norelease]

**EZTool Mode**
     iosetup A|B|C PORTxCFG OEx
     ioset A|B|C OUTx
     ioget A|B|C
     eeread addr len
     eewrite addr b0 b1 b2 ...
     xread addr len
     xwrite addr b0 b1 b2 ...
     i2cread addr len
     i2cwrite addr b0 b1 b2 ...

**User Mode**
     claim intf alt
     controlmsg bmRequestType bRequest wValue wIndex [length|b0 b1 b2 ...]
     bulkin  ep length
     bulkout ep b0 b1 b2 ...

## VARIABLES

     $timeout
     $usbid      # currently open device, "" if in mode "disconnected"

## FILES

  * _/etc/eztool/eztoolrc_:
    System-wide startup script

  * _/etc/eztool/eztoolrc.d/*.tcl_:
    Base directory for more system-wide startup scripts

  * _~/.eztoolrc_:
    Personal startup script

  * _~/.eztool_history_:
    stores the history of the commands entered at the prompt

  * _firmware.ihx_:
    EZ-Tools firmware for the EZ-USB device. This file is searched in the
    current directory, in ~/.eztool/, in the path given by the environment
    variable $EZTOOLFIRMWAREPATH, in the directory of the executable, in the
    system $PATH and in /etc/eztool/.

## TODO

 - implement variable $timeout
 - object type "data"
 - `man`: auto-complete man-pages (simpler: all TCL commands, but the user might
   be fooled)
 - add a `man` parameter to print to stdout without a pager
   "man -Tutf8 page" works, but switches off highlighting,
   interesting, bold font is emulated with `x^Hx`, underline with `_^Hx`
   "less -X" does not switch to an alternate screen and does not clear the
   screen afterwards ->
   "man -P"less -X" page"
   "LESS=-X man page"
 - handle searching for firmware properly
 - handle searching for man-pages properly
 - implement interrupt and isochronous transfers
 - offer to set USB configuration

## REFERENCES

`eztool` uses the [GNU
Readline](http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html) library with
[Pascal bindings and an object-oriented
wrapper](https://github.com/hansiglaser/pas-readline/).

[Tcl (Tool Command Language)](http://www.tcl.tk/) is also used with [Pascal
bindings and an object-oriented
wrapper](https://github.com/hansiglaser/pas-tcl/).

For the USB communication, [libusb](http://www.libusb.org/) is used.

## SEE ALSO

help(1ez), man(1ez), fxload(1)

## AUTHOR

Johann Glaser <Johann.Glaser@gmx.at>

*)
Program EZTool;

{$mode objfpc}{$H+}

Uses
  Classes, SysUtils, Math, LibUSB, LibUsbOop, LibUsbUtil, EZUSB, Device, Utils, ReadlineOOP, Tcl, TclOOP, BaseUnix, Unix, TclApp, USBDeviceDebug;

Type

  TMode = (mdDisconnected,mdEmpty,mdEZTool,mdUser);
  TModeSet = set of TMode;

  { TEZTool }

  TEZTool = class(TTclApp)
  private
    FMode         : TMode;
    FContext      : TLibUsbContext;
    FEmptyDevice  : TLibUsbDeviceEZUSB;
    FEZToolDevice : TEZToolDevice;
    FUserDevice   : TUSBDeviceDebug;
    Procedure SetMode(AMode:TMode;DoEqual:Boolean=true);
    Procedure CheckMode(AModes:TModeSet);
    // internal functions
    Procedure DisconnectAll;
    Procedure ConnectEmpty(AidVendor:Word;AidProduct:Word);
    Procedure ConnectEZTool(AidVendorEmpty,AidProductEmpty:Word;AidVendorEztool:Word;AidProductEztool:Word);
    Procedure ConnectUser(AidVendor:Word;AidProduct:Word);
    Procedure NotifyConnected(AidVendor : Word; AidProduct : Word);
    // common commands
    Procedure Help      (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure LsUsb     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure DevInfo   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure Disconnect(ObjC:Integer;ObjV:PPTcl_Object);
    // Mode: Disconnected
    Procedure Connect   (ObjC:Integer;ObjV:PPTcl_Object);
    // Mode: Empty
    Procedure Reset     (ObjC:Integer;ObjV:PPTcl_Object);
           // XRead
           // XWrite
    Procedure Download  (ObjC:Integer;ObjV:PPTcl_Object);
    // Mode: EZTool
    Procedure IOSetup   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure IOSet     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure IOGet     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure EERead    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure EEWrite   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure XRead     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure XWrite    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure I2CRead   (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure I2CWrite  (ObjC:Integer;ObjV:PPTcl_Object);
    // Mode: User
    Procedure Claim     (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure ControlMsg(ObjC:Integer;ObjV:PPTcl_Object);
    Procedure BulkIn    (ObjC:Integer;ObjV:PPTcl_Object);
    Procedure BulkOut   (ObjC:Integer;ObjV:PPTcl_Object);
  public
    Constructor Create;
    Destructor  Destroy; override;
  End;

{ TEZTool }

Constructor TEZTool.Create;
Begin
  inherited Create('.eztool_history','EZTool');

  // Register/override in the Tcl engine our new functions
  // common commands
  FCmdLine.CreateCommandExit   ('exit');
  FCmdLine.CreateCommandHistory('history');
  FCmdLine.CreateCommandMan    ('man',FpGetCwd + '/man');
  FTCL.CreateObjCommand('help',      @Self.Help,      nil);
  FTCL.CreateObjCommand('lsusb',     @Self.LsUsb,     nil);
  FTCL.CreateObjCommand('devinfo',   @Self.DevInfo,   nil);
  FTCL.CreateObjCommand('disconnect',@Self.Disconnect,nil);
  // Mode: Disconnected
  FTCL.CreateObjCommand('connect',   @Self.Connect,   nil);
  // Mode: Empty
  FTCL.CreateObjCommand('reset',     @Self.Reset,     nil);
         // XRead
         // XWrite
  FTCL.CreateObjCommand('download',  @Self.Download,  nil);
  // Mode: EZTool
  FTCL.CreateObjCommand('iosetup',   @Self.IOSetup,   nil);
  FTCL.CreateObjCommand('ioset',     @Self.IOSet,     nil);
  FTCL.CreateObjCommand('ioget',     @Self.IOGet,     nil);
  FTCL.CreateObjCommand('eeread',    @Self.EERead,    nil);
  FTCL.CreateObjCommand('eewrite',   @Self.EEWrite,   nil);
  FTCL.CreateObjCommand('xread',     @Self.XRead,     nil);
  FTCL.CreateObjCommand('xwrite',    @Self.XWrite,    nil);
  FTCL.CreateObjCommand('i2cread',   @Self.I2CRead,   nil);
  FTCL.CreateObjCommand('i2cwrite',  @Self.I2CWrite,  nil);
  // Mode: User
  FTCL.CreateObjCommand('claim',     @Self.Claim,     nil);
  FTCL.CreateObjCommand('controlmsg',@Self.ControlMsg,nil);
  FTCL.CreateObjCommand('bulkin',    @Self.BulkIn,    nil);
  FTCL.CreateObjCommand('bulkout',   @Self.BulkOut,   nil);

  FTCL.SetVar('usbid_empty','0547:2131');
  FTCL.SetVar('usbid_eztool',IntToHex(USBVendConf,4)+':'+IntToHex(USBProdConf,4));

  // constants (unfortunately, there is no such thing, so we make variables)
  FTCL.SetVar('LIBUSB_ENDPOINT_IN',               '0x'+IntToHex(LIBUSB_ENDPOINT_IN,               2));
  FTCL.SetVar('LIBUSB_ENDPOINT_OUT',              '0x'+IntToHex(LIBUSB_ENDPOINT_OUT,              2));
  FTCL.SetVar('LIBUSB_REQUEST_GET_STATUS',        '0x'+IntToHex(LIBUSB_REQUEST_GET_STATUS,        2));
  FTCL.SetVar('LIBUSB_REQUEST_CLEAR_FEATURE',     '0x'+IntToHex(LIBUSB_REQUEST_CLEAR_FEATURE,     2));
  FTCL.SetVar('LIBUSB_REQUEST_SET_FEATURE',       '0x'+IntToHex(LIBUSB_REQUEST_SET_FEATURE,       2));
  FTCL.SetVar('LIBUSB_REQUEST_SET_ADDRESS',       '0x'+IntToHex(LIBUSB_REQUEST_SET_ADDRESS,       2));
  FTCL.SetVar('LIBUSB_REQUEST_GET_DESCRIPTOR',    '0x'+IntToHex(LIBUSB_REQUEST_GET_DESCRIPTOR,    2));
  FTCL.SetVar('LIBUSB_REQUEST_SET_DESCRIPTOR',    '0x'+IntToHex(LIBUSB_REQUEST_SET_DESCRIPTOR,    2));
  FTCL.SetVar('LIBUSB_REQUEST_GET_CONFIGURATION', '0x'+IntToHex(LIBUSB_REQUEST_GET_CONFIGURATION, 2));
  FTCL.SetVar('LIBUSB_REQUEST_SET_CONFIGURATION', '0x'+IntToHex(LIBUSB_REQUEST_SET_CONFIGURATION, 2));
  FTCL.SetVar('LIBUSB_REQUEST_GET_INTERFACE',     '0x'+IntToHex(LIBUSB_REQUEST_GET_INTERFACE,     2));
  FTCL.SetVar('LIBUSB_REQUEST_SET_INTERFACE',     '0x'+IntToHex(LIBUSB_REQUEST_SET_INTERFACE,     2));
  FTCL.SetVar('LIBUSB_REQUEST_SYNCH_FRAME',       '0x'+IntToHex(LIBUSB_REQUEST_SYNCH_FRAME,       2));
  FTCL.SetVar('LIBUSB_REQUEST_TYPE_STANDARD',     '0x'+IntToHex(LIBUSB_REQUEST_TYPE_STANDARD,     2));
  FTCL.SetVar('LIBUSB_REQUEST_TYPE_CLASS',        '0x'+IntToHex(LIBUSB_REQUEST_TYPE_CLASS,        2));
  FTCL.SetVar('LIBUSB_REQUEST_TYPE_VENDOR',       '0x'+IntToHex(LIBUSB_REQUEST_TYPE_VENDOR,       2));
  FTCL.SetVar('LIBUSB_REQUEST_TYPE_RESERVED',     '0x'+IntToHex(LIBUSB_REQUEST_TYPE_RESERVED,     2));
  FTCL.SetVar('LIBUSB_RECIPIENT_DEVICE',          '0x'+IntToHex(LIBUSB_RECIPIENT_DEVICE,          2));
  FTCL.SetVar('LIBUSB_RECIPIENT_INTERFACE',       '0x'+IntToHex(LIBUSB_RECIPIENT_INTERFACE,       2));
  FTCL.SetVar('LIBUSB_RECIPIENT_ENDPOINT',        '0x'+IntToHex(LIBUSB_RECIPIENT_ENDPOINT,        2));
  FTCL.SetVar('LIBUSB_RECIPIENT_OTHER',           '0x'+IntToHex(LIBUSB_RECIPIENT_OTHER,           2));

  SetMode(mdDisconnected);

  FContext := TLibUsbContext.Create;
End;

Destructor TEZTool.Destroy;
Begin
  FEmptyDevice.Free;
  FEZToolDevice.Free;
  FUserDevice.Free;
  FContext.Free;
  inherited Destroy;
End;

Procedure TEZTool.SetMode(AMode : TMode;DoEqual:Boolean);
Begin
  if (FMode = AMode) and not DoEqual then
    Exit;
  FMode := AMode;
  Case AMode of
    mdDisconnected : Begin
      FCmdLine.Readline.Prompt := 'Discon> ';
    End;
    mdEmpty        : Begin
      FCmdLine.Readline.Prompt := 'Empty> ';
    End;
    mdEZTool       : Begin
      FCmdLine.Readline.Prompt := 'EZTool> ';
    End;
    mdUser         : Begin
      FCmdLine.Readline.Prompt := 'User> ';
    End;
  End;
End;

Procedure TEZTool.CheckMode(AModes : TModeSet);
Begin
  if not (FMode in AModes) then
    raise Exception.Create('This command is not available in this mode.');
End;

Procedure TEZTool.DisconnectAll;
Begin
  // free all devices
  FreeAndNil(FEmptyDevice);
  FreeAndNil(FEZToolDevice);
  FreeAndNil(FUserDevice);
End;

Procedure TEZTool.ConnectEmpty(AidVendor:Word;AidProduct:Word);
Begin
  DisconnectAll;
  FEmptyDevice := TLibUsbDeviceEZUSB.Create(FContext,AidVendor,AidProduct);
End;

Procedure TEZTool.ConnectEZTool(AidVendorEmpty,AidProductEmpty:Word;AidVendorEztool:Word;AidProductEztool:Word);
Var MatchEmpty : TLibUsbDeviceMatchVidPid;
Begin
  DisconnectAll;
  try
    if (AidVendorEmpty <> 0) or (AidProductEmpty <> 0) then
      // unconfigured (=empty) device given -> use a matcher
      MatchEmpty := TLibUsbDeviceMatchVidPid.Create(FContext,AidVendorEmpty,AidProductEmpty)
    else
      // no unconfigured device given -> don't use a matcher, and don't even search for it
      MatchEmpty := Nil;
    FEZToolDevice := TEZToolDevice.Create(
      FContext,
      MatchEmpty,
      TEZToolDevice.FindFirmware(Device.FirmwareName,'eztool'),
      TLibUsbDeviceMatchVidPid.Create(FContext,AidVendorEztool,AidProductEztool));
    // the two matcher classes are .Free()ed inside the constructor
    WriteLn('Successfully connected to USB device ',IntToHex(AidVendorEztool,4),':',IntToHex(AidProductEztool,4),': ',FEZToolDevice.GetVersion);
  except
    on E : Exception do
      Begin
        WriteLn('Couldn''t connect to device: ',E.Message);
        raise;  // re-raise
      End;
  End;
End;

Procedure TEZTool.ConnectUser(AidVendor:Word;AidProduct:Word);
Begin
  DisconnectAll;
  FUserDevice := TUSBDeviceDebug.Create(FContext,AidVendor,AidProduct);
End;

Procedure TEZTool.NotifyConnected(AidVendor:Word;AidProduct:Word);
Var UsbID : String;
Begin
  UsbID := IntToHex(AidVendor,4)+':'+IntToHex(AidProduct,4);
  FTCL.SetVar('usbid',UsbID);
  WriteLn('Connected to device ',UsbID);
End;

(*****************************************************************************)
(***  TCL Functions: Common Commands  ****************************************)
(*****************************************************************************)

(*ronn
help(1ez) -- Provide a list of all commands and variables
=========================================================

## SYNOPSYS

`help` [<word>]

## DESCRIPTION

The command `help` prints a list of all commands with their parameters and of
all variables.

If `help` is supplied with the parameter <word>, it returns a list of all
commands which contain <word> (similar to apropos(1)).

## MODES

This command is available in all modes.

## SEE ALSO

man(1ez)

*)
Procedure TEZTool.Help(ObjC : Integer; ObjV: PPTcl_Object);
Begin
  if ObjC = 2 then  // one argument -> "apropos"
    Begin
      FCmdLine.Apropos(ObjV^[1].AsPChar);
      Exit;
    End;
  WriteLn('EZ-Tool');
  WriteLn('Common commands');
  WriteLn('  help [word]');
  WriteLn('  man [page]');
  WriteLn('  history');
  WriteLn('  lsusb [-a|...]');
  WriteLn('  disconnect');
  WriteLn('  devinfo');
  WriteLn('  exit [exitcode]');
  WriteLn('Mode: Disconnected ("Discon")');
  WriteLn('  connect [-empty|-eztool|-user] [idVendor:idProduct]');
  WriteLn('Mode: Connected to empty EZ-USB device ("Empty")');
  WriteLn('  reset [-toggle|-keep|-release]');
  WriteLn('  xread');
  WriteLn('  xwrite');
  WriteLn('  download [firmware.hex idVendor:idProduct] [-norelease]  ');
  WriteLn('Mode: Connected to EU-USB device with EZTool firmware ("EZTool")');
  WriteLn('  iosetup A|B|C PORTxCFG OEx');
  WriteLn('  ioset A|B|C OUTx');
  WriteLn('  ioget A|B|C');
  WriteLn('  eeread addr len');
  WriteLn('  eewrite addr b0 b1 b2 ...');
  WriteLn('  xread addr len');
  WriteLn('  xwrite addr b0 b1 b2 ...');
  WriteLn('  i2cread addr len');
  WriteLn('  i2cwrite addr b0 b1 b2 ...');
  WriteLn('Mode: Connected to EU-USB device with user firmware ("User")');
  WriteLn('  claim intf alt');
  WriteLn('  controlmsg bmRequestType bRequest wValue wIndex [length|b0 b1 b2 ...]');
  WriteLn('  bulkin  ep length');
  WriteLn('  bulkout ep b0 b1 b2 ...');
  WriteLn('Variables');
  WriteLn('  $timeout');
  WriteLn('  $usbid');
  WriteLn('  $usbid_empty');
  WriteLn('  $usbid_eztool');
  WriteLn('  $usbid_user');
  WriteLn('Some useful TCL commands: exec, puts, pwd, cd, source, set, after, info');
End;

(*ronn
man(1ez) -- online reference manual
===================================

## SYNOPSYS

`man` [<page>]

## DESCRIPTION

Display the manual page for the given <page>. If no <page> is given, a list of
all man pages and short descriptions is given, similar to `whatis`(1).

## EXAMPLES

  `man help`

## SEE ALSO

help(1ez)

*)

(*ronn
history(1ez) -- display the history list
========================================

## SYNOPSYS

`history`

## DESCRIPTION

Prints all previously entered commands. These are also stored in the file
_~/.eztool_history_ on program exit and loaded on program start.

*)

(*ronn
exit(1ez) -- quit the program
=============================

## SYNOPSYS

`exit` [<exitcode>]

## DESCRIPTION

Quit the program. An optional <exitcode> can be given. If none is supplied, it
defaults to <0>. Note that the exit code is an unsigned 8 bit value (0..255).

The [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/exit-status.html) writes
on exit codes:

    [...] A successful command returns a 0, while an unsuccessful one returns a
    non-zero value that usually can be interpreted as an error code.
    Well-behaved UNIX commands, programs, and utilities return a 0 exit code
    upon successful completion, [...]

*)

(*ronn
lsusb(1ez) -- list USB devices
==============================

## SYNOPSYS

`lsusb` [-a|<options>]

## DESCRIPTION

The command `lsusb` invokes the external program `lsusb` with the parameters as
given at the prompt.

In modes `Empty`, `EZTool` and `User`, a special behavior is provided. When
called without parameteres, the external program is invoked with the parameters
`-v -d` _idVendor_`:`_idProduct_. Use the option `-a` to execute `lsusb` without
parameters or provide any custom options.

## MODES

This command is available in all modes but has special behavior in the modes
`Empty`, `EZTool` and `User` (see DESCRIPTION).

## SEE ALSO

devinfo(1ez)
*)
Procedure TEZTool.LsUsb(ObjC : Integer; ObjV : PPTcl_Object);
Var Args     : TDynArrString;
    ExitCode : Integer;

  Procedure SetArgs(AArgs:Array of String);
  Var I : Integer;
  Begin
    SetLength(Args,Length(AArgs));
    For I := 0 to Length(AArgs)-1 do
      Args[I] := AArgs[I];
  End;

  Procedure SetArgs(ObjC : Integer; ObjV : PPTcl_Object);
  Var I : Integer;
  Begin
    SetLength(Args,ObjC-1);
    For I := 0 to ObjC-2 do
      Args[I] := ObjV^[I+1].AsString;
  End;

Begin
  if FMode <> mdDisconnected then
    Begin
      if ObjC = 1 then    // connected, no parameters
        SetArgs(['-v', '-d', FTCL.GetVar('usbid')])
      else if (ObjC = 2) and (ObjV^[1].AsPChar = '-a') then   // connected, called with "-a"
        SetArgs([])
      else // connected, called with custom parameters
        SetArgs(ObjC,ObjV);
    End
  else
    SetArgs(ObjC,ObjV);
  ExitCode := Exec('/usr/bin/lsusb',Args,[],true);
  if ExitCode > 0 then
    raise Exception.CreateFmt('libusb returned %d',[ExitCode])
  else if ExitCode < 0 then
    raise Exception.CreateFmt('Error executing libusb (%d): %s',[-ExitCode,SysErrorMessage(-ExitCode)]);
End;

(*ronn
devinfo(1ez) -- print information on the connected device
=========================================================

## SYNOPSYS

`devinfo`

## DESCRIPTION

The command `devinfo` shows the configurations, interfaces, alternate settings
and end points of the currently connected device.

## MODES

`Empty`, `EZTool` and `User`

## SEE ALSO

lsusb(1ez)
*)
Procedure TEZTool.DevInfo(ObjC : Integer; ObjV : PPTcl_Object);
Var Dev : TLibUsbDevice;
    Config : Plibusb_config_descriptor;
    NumIf,IIf : Integer;
    TheIf : Plibusb_interface_descriptor;
    NumAlt,IAlt : Integer;
    TheAlt : Plibusb_interface_descriptor;
    NumEp,IEp : Integer;
    TheEP : Plibusb_endpoint_descriptor;
Begin
  CheckMode([mdEmpty,mdEZTool,mdUser]);
  case FMode of
    mdEmpty  : Dev := FEmptyDevice;
    mdEZTool : Dev := FEZToolDevice;
    mdUser   : Dev := FUserDevice;
  End;

  Config := FContext.GetActiveConfigDescriptor(Dev.Device);
  // iterate over all interfaces
  For IIf := 0 to Config^.bNumInterfaces-1 do
    With Config^._interface^[IIf] do
      Begin
        // iterate over all alternate settings
        For IAlt := 0 to num_altsetting-1 do
          With altsetting^[IAlt] do
            Begin
              WriteLn('Interface ',bInterfaceNumber,
                      ' (Alternate ',bAlternateSetting,')',
                      ' "',Dev.Control.GetString(iInterface),'":');
              For IEp := 0 to bNumEndpoints-1 do
                With endpoint^[IEp] do
                  Begin
                    WriteLn('  EP ',   bEndpointAddress and LIBUSB_ENDPOINT_ADDRESS_MASK:2,
                            ' ',Select(bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK <> 0,'IN ','OUT'),
                            ' ',Select(bmAttributes     and LIBUSB_TRANSFER_TYPE_MASK,['Control','Isochronous','Bulk','Interrupt']));
                  End;
            End;
      End;
End;

(*ronn

disconnect(1ez) -- disconnect from USB device
=============================================

## SYNOPSYS

`disconnect`

## DESCRIPTION

Disconnect from the USB device.

## MODES

`Empty`, `EZTool`, `User`

Switches to mode `Disconnected`

## SEE ALSO

`connect`(1ez)

*)
Procedure TEZTool.Disconnect(ObjC:Integer;ObjV:PPTcl_Object);
Begin
  CheckMode([mdEmpty,mdEZTool,mdUser]);
  // free all devices
  DisconnectAll;
  SetMode(mdDisconnected);
End;

(*****************************************************************************)
(***  TCL Functions: Mode: Disconnected  *************************************)
(*****************************************************************************)

(*ronn
connect(1ez) -- connect to a USB device
=======================================

## SYNOPSYS

`connect` [`-empty`|`-eztool`|`-user`] [<idVendor>`:`<idProduct>]

## DESCRIPTION

With `connect`, `eztool` connects to a USB device. The options `-empty`,
`eztool` and `-user` specify the mode and therefore the functionality.

If no arguments are given, first a connection in mode `Empty` to the EZ-USB
device is established. Then the eztool firmware is `download`(1ez)ed. After
a view hundret milliseconds, a re-connect in `EZTool` mode is performed.

With the optional second parameter [<idVendor>`:`<idProduct>] the USB device
identification can be supplied. If this parameter is omitted, the values of the
variables `$usbid_empty`, `$usbid_eztool` or `$usbid_user` are used,
respectively.

After successfully connecting to the USB device, the variable `$usbid` is set
with the current USB IDs.

## VARIABLES

 - `$usbid_empty`
 - `$usbid_eztool`
 - `$usbid_user`
 - `$usbid`

## EXAMPLES

To connect to a EZ-USB device without firmware, but to work with its IO ports
and the external I2c EEPROM, the `EZTool` mode is entered after connecting and
downloading the EZTool firmware. This allows, e.g., to program the external
I2C EEPROM with the USB IDs 1234:5678.

    connect
    eewrite 0x0000 0xB0 0x34 0x12 0x78 0x56 0x01 0x00
    eeread 0x0000 16
    disconnect

The following command connects to a EZ-USB device with already loaded firmware
for the `EZTool` mode.

    connect -eztool

Before a custom firmware is `download`(1ez)ed, a connection to the empty
EZ-USB device is necessary. In this example, an external I2C EEPROM with
custom USB IDs is assumed.

    connect -empty 1234:5678
    download ../firmware.hex 1234:abcd
    disconnect

Finally, with the custom firmware loaded, a connection in `User` mode allows
to communicate with the device.

    connect -user 1234:abcd
    controlmsg ...
    bulkin ...
    bulkout ...

## MODES

`Disconnected`

Switches to modes `Empty`, `EZTool` or `User`, depending on the option.

## SEE ALSO

`disconnect`(1ez)

*)
Procedure TEZTool.Connect(ObjC : Integer; ObjV: PPTcl_Object);
Var idVendor,      idProduct       : Word;
    idVendorEmpty, idProductEmpty  : Word;
    idVendorEztool,idProductEztool : Word;

  Procedure GetUSBIDs(VarName:String);
  Begin
    if ObjC = 3 then
      Begin
        if not SplitUsbID(ObjV^[2].AsPChar,idVendor,idProduct) then
          raise Exception.Create('Invalid format of parameter idVendor:idProduct');
        FTCL.SetVar(VarName,IntToHex(idVendor,4)+':'+IntToHex(idProduct,4));
      End
    else
      Begin
        if not SplitUsbID(FTCL.GetVar(VarName),idVendor,idProduct) then
          raise Exception.Create('Invalid format of variable $'+VarName);
      End;
  End;

Begin
  CheckMode([mdDisconnected]);

  if (ObjC < 1) or (ObjC > 3) then
    raise Exception.Create('Invalid paramaters');

  if ObjC = 1 then
    Begin
      // connect
      // this means: connect -empty ; download (including re-connect to eztool-firmware)
      // read defaults from TCL variables
      if not SplitUsbID(FTCL.GetVar('usbid_empty'),idVendorEmpty,idProductEmpty) then
        raise Exception.Create('Invalid format of variable $usbid_empty');
      if not SplitUsbID(FTCL.GetVar('usbid_eztool'),idVendorEztool,idProductEztool) then
        raise Exception.Create('Invalid format of variable $usbid_eztool');
      WriteLn('Searching unconfigured devices with ',IntToHex(idVendorEmpty,4),':',IntToHex(idProductEmpty,4),'...');
      ConnectEZTool(idVendorEmpty,idProductEmpty,idVendorEztool,idProductEztool);
      NotifyConnected(idVendorEztool,idProductEztool);
      SetMode(mdEZTool);
    End
  else if MatchOption(ObjV^[1].AsString,'-empty',3) then
    Begin
      // connect -empty  [idVendor:idProduct]
      GetUSBIDs('usbid_empty');
      ConnectEmpty(idVendor,idProduct);
      NotifyConnected(idVendor,idProduct);
      SetMode(mdEmpty);
    End
  else if MatchOption(ObjV^[1].AsString,'-eztool',3) then
    Begin
      // connect -eztool [idVendor:idProduct]
      GetUSBIDs('usbid_eztool');
      ConnectEZTool($0000,$0000,idVendor,idProduct);
      NotifyConnected(idVendor,idProduct);
      SetMode(mdEZTool);
    End
  else if MatchOption(ObjV^[1].AsString,'-user',2) then
    Begin
      // connect -user   [idVendor:idProduct]
      GetUSBIDs('usbid_user');
      ConnectUser(idVendor,idProduct);
      NotifyConnected(idVendor,idProduct);
      SetMode(mdUser);
    End
  else
    raise Exception.Create('Invalid paramaters');
End;

(*****************************************************************************)
(***  TCL Functions: Mode: Empty  ********************************************)
(*****************************************************************************)

(*ronn
reset(1ez) -- reset the 8051 CPU in the EZ-USB
==============================================

## SYNOPSYS

`reset` [-toggle|-keep|-release]

## DESCRIPTION

The command `reset` asserts the Reset bit for the 8051 CPU in the EZ-USB
microcontroller.

With `-toggle`, the reset is asserted and deasserted again. If no option is
supplied, this is the default behavior.

`-keep` asserts the reset and keeps that state.

The reset can be deasserted with the `-release` option.

## MODES

`Empty`

## SEE ALSO

`xread`(1ez), `xwrite`(1ez)

*)
Procedure TEZTool.Reset(ObjC : Integer; ObjV : PPTcl_Object);
Var DoReset,DoRelease : Boolean;
Begin
  CheckMode([mdEmpty]);
  // reset [-toggle|-keep|-release]
  // default
  DoReset   := true;
  DoRelease := true;
  // parse parameters
  if ObjC = 2 then
    Begin
      if      MatchOption(ObjV^[1].AsString,'-toggle',2) then
        // do nothing, default is already set
      else if MatchOption(ObjV^[1].AsString,'-keep',2) then
        DoRelease := False
      else if MatchOption(ObjV^[1].AsString,'-release',2) then
        DoReset   := False;
    End
  else
    if ObjC <> 1 then
      raise Exception.Create('Invalid paramaters');

  if DoReset then
    if FEmptyDevice.ResetCPU(CPUCS_8051RESET) <> 1 then
      raise Exception.Create('Error resetting EZ-USB');
  if DoRelease then
    if FEmptyDevice.ResetCPU(0) <> 1 then
      raise Exception.Create('Error releasing EZ-USB from reset');
End;

(*ronn
download(1ez) -- download firmware
==================================

## SYNOPSYS

`download` [<firmware>.<hex> <idVendor>`:`<idProduct>] [`-norelease`]

## DESCRIPTION

With `download`, firmware is downloaded to the SRAM of the EZ-USB device. As
first step the 8051 CPU is held in reset. Then the Intel Hex file is read and
downloaded to the SRAM. Finally the CPU is released from reset and starts to
execute the firmware.

EZ-Tools disconnects from the device and waits for 1 sec. Meanwhile the EZ-USB
device should have booted and (re-)enumerated on the USB bus. Then a connection
is made to the device with running firmware.

If `-norelease` is used, the reset state is kept after the download and the
connection is not closed.

The parameter <firmware>.<hex> specifies the firmware file to read. The USB IDs
to connect to the booted device are given with <idVendor>`:`<idProduct>.

If no firmware file and no USB IDs are given, the EZ-Tools firmware is used.

## MODES

`Empty`

Switches the mode to `EZTool` or `User`.

## SEE ALSO

`xwrite`(1ez), `reset`(1ez)

*)
Procedure TEZTool.Download(ObjC : Integer; ObjV : PPTcl_Object);
Var Firmware           : String;
    StartImmediately   : Boolean;
    UserMode           : Boolean;
    idVendor,idProduct : Word;
Begin
  CheckMode([mdEmpty]);
  // Usage:
  //   download [-norelease]                                  # use default EZTool firmware
  //   download firmware.hex idVendor:idProduct [-norelease]
  // implies reset, disconnect and connect -eztool/-user; -norelease keeps the device in reset

  if (ObjC < 1) or (ObjC > 4) then
    raise Exception.Create('Invalid paramaters');

  // set defaults
  Firmware := TEZToolDevice.FindFirmware(Device.FirmwareName,'eztool');
  StartImmediately := true;
  UserMode         := false;
  if not SplitUsbID(FTCL.GetVar('usbid_eztool'),idVendor,idProduct) then
    raise Exception.Create('Invalid format of variable $usbid_eztool');

  // check for '-norelease'
  if (ObjC = 2) or (ObjC = 4) then
    Begin
      // download (...) -norelease
      if not MatchOption(ObjV^[ObjC-1].AsString,'-norelease',2) then
        raise Exception.Create('Invalid parameters');
      StartImmediately := false;
    End;
  // get filename and USB IDs
  if (ObjC = 3) or (ObjC = 4) then
    Begin
      // download firmware.hex idVendor:idProduct [-norelease]
      Firmware := ObjV^[1].AsString;
      if not SplitUsbID(ObjV^[2].AsPChar,idVendor,idProduct) then
        raise Exception.Create('Invalid format of parameter idVendor:idProduct');
      UserMode := true;
    End;

  // download new firmware
  WriteLn('Downloading firmware ',Firmware);
  FEmptyDevice.DownloadFirmware(Firmware,StartImmediately);
  if not StartImmediately then
    Exit;   // device is held in Reset
  // disconnect from device
  FreeAndNil(FEmptyDevice);
  SetMode(mdDisconnected);   // set the mode, in case something goes wrong below
  Sleep(1000);    // wait for device to boot up, TODO: use USBFindDevices
  // connect to new device
  if UserMode then
    Begin
      ConnectUser(idVendor,idProduct);
      NotifyConnected(idVendor,idProduct);
      SetMode(mdUser);
    End
  else
    Begin
      ConnectEZTool($0000,$0000,idVendor,idProduct);
      NotifyConnected(idVendor,idProduct);
      SetMode(mdEZTool);
    End;
End;

(*****************************************************************************)
(***  TCL Functions: Mode: EZTool ********************************************)
(*****************************************************************************)

Function ConvPort(Port:String):TPort;
Begin
  if Length(Port) <> 1 then
    raise Exception.Create('Invalid port qualifier');
  Case Port[1] of
    'A','a' : Result := ptA;
    'B','b' : Result := ptB;
    'C','c' : Result := ptC;
  else
    raise Exception.Create('Invalid port qualifier');
  End;
End;

(*ronn
iosetup(1ez) -- setup IO port special function and output enable
================================================================

## SYNOPSYS

`iosetup` `A`|`B`|`C` <PORTxCFG> <OEx>

## DESCRIPTION

The command `iosetup` sets the IO port configuration and output enable
registers `PORTxCFG` and `OEx` of the EZ-USB device for the Ports `A`, `B` and
`C`.

The bitmask given by <PORTxCFG> is written to the `PORTxCFG` register, where
_x_ is `A`, `B` or `C`. A `1` configures the corresponding pin (e.g. `B`, bit 3
correspond to pin PB3) as a special function pin (PB3 is TXD1). Writing `0` to
a configuration bit sets this pin as a general purpose input/output.

<OEx> specifies the bitmask for the register `OEx`. Writing `1` to a bit sets
the corresponding pin as an output by enabling the push-pull CMOS drivers. The
value `0` disables these drivers and therefore configures the pin as an input.

## EXAMPLES

To configure all eight pins of port A as general purpose outputs, the command

    iosetup A 0x00 0xFF

is used.

## MODES

`EZTool`

## REFERENCES

<EZ-USB Technical Reference Manual> p. 4-5.

## SEE ALSO

`ioset`(1ez), `ioget`(1ez)

*)
Procedure TEZTool.IOSetup(ObjC : Integer; ObjV: PPTcl_Object);
Begin
  CheckMode([mdEZTool]);
  // iosetup A|B|C PORTxCFG OEx
  if ObjC <> 4 then
    raise Exception.Create('Invalid parameters');
  FEZToolDevice.IOSetup(ConvPort(ObjV^[1].AsPChar),ObjV^[2].AsInteger(FTCL),ObjV^[3].AsInteger(FTCL));
End;

(*ronn
ioset(1ez) -- set IO port output
================================

## SYNOPSYS

`ioset` `A`|`B`|`C` <OUTx>

## DESCRIPTION

Pins, which are configured as general purpose outputs can be set with `ioset`.
The bitmask given by <OUTx> is written to the register `OUTx`, where _x_ is
`A`, `B` or `C`. Writing `1` sets the pin to high (i.e., 3.3V, VCC) and writing
`0` sets the pin to low (i.e., 0V, GND).

Note that the pins must be configured as general purpose IOs and as outputs
using `iosetup`(1ez).

## EXAMPLES

To set pin PA5 to high, all other PAx to low, use the command

    ioset A 0x20

## MODES

`EZTool`

## SEE ALSO

`iosetup`(1ez), `ioget`(1ez)

*)
Procedure TEZTool.IOSet(ObjC : Integer; ObjV: PPTcl_Object);
Begin
  CheckMode([mdEZTool]);
  // ioset A|B|C OUTx
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');
  FEZToolDevice.IOSet(ConvPort(ObjV^[1].AsPChar),ObjV^[2].AsInteger(FTCL));
End;

(*ronn
ioget(1ez) -- query IO pin signal
=================================

## SYNOPSYS

`ioget` `A`|`B`|`C`

## DESCRIPTION

To read the current input signal on the port pins, the command `ioget` returns
the value of the `PINSx` register of the EZ-USB, where _x_ is `A`, `B` or `C`.

The pin state can be read of any pin at any time, regardless of its
configuration.

## EXAMPLES

To query the state of the pins of port A, use the command

    ioget A

To set pin PA5 to high and keeping the other pins of port A as is, the state
of port A can be read, modified and written again.

    ioset A [expr [ioget A] | 0x20]

## MODES

`EZTool`

## SEE ALSO

`iosetup`(1ez), `ioget`(1ez)

*)
Procedure TEZTool.IOGet(ObjC : Integer; ObjV: PPTcl_Object);
Var B : Byte;
Begin
  CheckMode([mdEZTool]);
  // ioget A|B|C
  if ObjC <> 2 then
    raise Exception.Create('Invalid parameters');
  B := FEZToolDevice.IOGet(ConvPort(ObjV^[1].AsPChar));
  WriteLn('Port ',ObjV^[1].AsPChar,' = $',IntToHex(B,2),' = ',IntToBin(B,8));
  FTCL.SetObjResult(B);
End;

(*ronn
eeread(1ez) -- get data from I2C EEPROM
=======================================

## SYNOPSYS

`eeread` <addr> <len>

## DESCRIPTION

`eeread` reads data from the I2C EEPROM with I2C address 0x50 connected to the
EZ-USB microcontroller. A total of <len> bytes are read starting at address
<addr>. The maximum value of <len> is 64. <addr> is limited to 255 (i.e. an
8 bit value).

## EXAMPLES

Reading the first 16 bytes of the EEPROM is performed using

    eeread 0x0000 16

## MODES

`EZTool`

## SEE ALSO

`eewrite`(1ez)

*)
Procedure TEZTool.EERead(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : Array[0..63] of Byte;
    Addr : Cardinal;
    Len  : Cardinal;
Begin
  CheckMode([mdEZTool]);
  // eeread addr len
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  Len  := ObjV^[2].AsInteger(FTCL);
  if Addr >= $0100 then
    raise Exception.Create('Maximum start address is 0x00FF');
  if Len > 64 then
    raise Exception.Create('Maximum length is 64 bytes');
  FEZToolDevice.EERead(Addr,Buf,Len);
  HexDump(Addr,Buf,Len);
End;

(*ronn
eewrite(1ez) -- write data to I2C EEPROM
========================================

## SYNOPSYS

`eewrite` <addr> <b0> <b1> <b2> ...

## DESCRIPTION

Use `eewrite` to write to the I2C EEPROM with I2C address 0x50. <addr>
specifies the start address. It must be between 0x00 and 0xFF. The following
arguments <b0>, <b1>, ... are one or more data bytes which are written to the
EEPROM.

**Important:** The EEPROM does not allow to write across page boundares. Each
page is 16 bytes, therefore the maximum amount of data is 16 bytes. If the
start address is not is not divisible by 16, the maximum amount of data is
reduced by _addr mod 16_.

## EXAMPLES

To write the USB IDs to the EEPROM, with which the EZ-USB microcontroller
enumerates itself (e.g. 1234:5678), use the command:

    eewrite 0x0000 0xB0 0x34 0x12 0x78 0x56 0x01 0x00
    eeread 0x0000 16

## MODES

`EZTool`

## SEE ALSO

`eeread`(1ez)

*)
Procedure TEZTool.EEWrite(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : Array[0..63] of Byte;
    Addr : Cardinal;
    I    : Integer;
Begin
  CheckMode([mdEZTool]);
  // eewrite addr b0 b1 b2 ...
  if ObjC < 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  if Addr >= $0100 then
    raise Exception.Create('Maximum start address is 0x00FF');
  if (Addr and $000F) + (ObjC-2) > 16 then
    raise Exception.Create('You can not cross a 16-byte-page boundary');
  For I := 0 to Min(ObjC-3,High(Buf)) do
    Buf[I] := ObjV^[I+2].AsInteger(FTCL);
  HexDump(Addr,Buf,ObjC-2);
  FEZToolDevice.EEWrite(Addr,Buf,ObjC-2);
End;

(*ronn
xread(1ez) -- read from the 8051 XRAM space
===========================================

## SYNOPSYS

`xread` <addr> <len>

## DESCRIPTION

Use `xread` to read data from the 8051 XRAM of the EZ-USB microcontroller. The
start address is specified by <addr> and the number of bytes to read is given
with <len>. The address and the length can range from 0x0000 to 0xFFFF
(i.e. 16 bit).

## ADDRESS MAP

The EZ-USB AN2131 has 8kB SRAM from 0x0000 to 0x1FFF. The range from 0x0000 to
0x1B3F is used as combined code and data RAM. The range from 0x1B40 to
0x1FFF holds the EZ-USB register and the USB bulk endpoint buffers. The latter
range from 0x1B40 to 0x1F3F (16 buffer with 64 bytes each, 1024 bytes). The
EZ-USB registers are placed from 0x1F40 to 0x1FFF (192 bytes). The range from
0x1B40 to 0x1FFF is mirrored at 0x7B40 to 0x7FFF.

An additional 2kB SRAM are available from 0x2000 to 0x27FF if the isochronous
endpoints are disabled.

In mode `EZTool` the whole memory range can be accessed. In mode `Empty`, the
range is limited to 0x0000 to 0x1B40 (i.e., the code and data memory of the
8051 CPU) and 0x7F92 (i.e., the CPUCS register, but only bit 0, the 8051 Reset
signal, is writable).

## EXAMPLES

To read the interrupt vector table, use

    xread 0x0003 112

## MODES

`Empty`, `EZTool`

## REFERENCES

<EZ-USB Technical Reference Manual> p. 3-1ff.

## SEE ALSO

`xwrite`(1ez)

*)
Procedure TEZTool.XRead(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : Pointer;
    Addr : Cardinal;
    Len  : Cardinal;
Begin
  CheckMode([mdEmpty,mdEZTool]);
  // xread addr len
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  Len  := ObjV^[2].AsInteger(FTCL);
  if Len > 1024 then
    raise Exception.Create('Maximum length is 1024');
  GetMem(Buf,Len);
  if FMode = mdEmpty then
    FEmptyDevice.ReadMem(Addr,Buf^,Len)
  else
    FEZToolDevice.XRead(Addr,Buf^,Len);
  HexDump(Addr,Buf^,Len);
  FreeMem(Buf);
End;

(*ronn
xwrite(1ez) -- write to the 8051 XRAM space
===========================================

## SYNOPSYS

`xwrite` <addr> <b0> <b1> <b2> ...

## DESCRIPTION

Use `xwrite` to write to the 8051 XRAM space. <addr> specifies the start
address in a range from 0x0000 0xFFFF. The following arguments <b0>, <b1>, ...
are one or more data bytes which are written to the XRAM.

For a description of the address map and limitations in mode `Empty`, see
`xread`(1ez).

## EXAMPLES

To change the value of an XDATA variable, which is placed at 0x09A3 by the
linker, use

    xwrite 0x09A3 0xC3

## MODES

`Empty`, `EZTool`

## REFERENCES

<EZ-USB Technical Reference Manual> p. 3-1ff.

## SEE ALSO

`xread`(1ez)

*)
Procedure TEZTool.XWrite(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : PByteArray;
    Addr : Cardinal;
    I    : Cardinal;
Begin
  CheckMode([mdEmpty,mdEZTool]);
  // xwrite addr b0 b1 b2 ...
  if ObjC < 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  GetMem(Buf,ObjC-2);
  For I := 0 to ObjC-3 do
    Buf^[I] := ObjV^[I+2].AsInteger(FTCL);
  HexDump(Addr,Buf^,ObjC-2);
  if FMode = mdEmpty then
    FEmptyDevice.WriteMem(Addr,Buf^,ObjC-2)
  else
    FEZToolDevice.XWrite(Addr,Buf^,ObjC-2);
  FreeMem(Buf);
End;

(*ronn
i2cread(1ez) -- get data from I2C EEPROM
=======================================

## SYNOPSYS

`i2cread` <addr> <len>

## DESCRIPTION

`i2cread` issues a read transfer at the I2C bus. The 7-bit address of the I2C
slave is specifyed with <addr>. This value will be left-shifted by 1 and the
LSB is set to 1, denoting an I2C read transfer. A total of <len> bytes are
read. The maximum value of <len> is 64. <addr> is limited to 0x7F.

## EXAMPLES

For an example see `i2cwrite`(1ez).

## MODES

`EZTool`

## SEE ALSO

`i2cwrite`(1ez)

*)
Procedure TEZTool.I2CRead(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : Array[0..63] of Byte;
    Addr : Cardinal;
    Len  : Cardinal;
Begin
  CheckMode([mdEZTool]);
  // eeread addr len
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  Len  := ObjV^[2].AsInteger(FTCL);
  if Addr >= $0100 then
    raise Exception.Create('Maximum start address is 0x00FF');
  if Len > 64 then
    raise Exception.Create('Maximum length is 64 bytes');
  FEZToolDevice.I2CRead(Addr,Buf,Len);
  HexDump(Addr,Buf,Len);
End;

(*ronn
i2cwrite(1ez) -- write data to I2C EEPROM
========================================

## SYNOPSYS

`i2cwrite` <addr> <b0> <b1> <b2> ...

## DESCRIPTION

Use `i2cwrite` to perform a generic write transfer at the I2C bus. <addr>
specifies the 7-bit slave address. It must be between 0x00 and 0x7F. This value
will be left-shifted by 1 and the LSB is set to 0, denoting an I2C write
transfer. The following arguments <b0>, <b1>, ... are one or more data bytes
which are written on the I2C bus

## EXAMPLES

To configure an HMC5883L (Three-Axis Digital Compass IC, 7-bit address 0x1E,
8-bit read address 0x3D, 8-bit write address 0x3C) for continuous measurement
mode (mode register at internal address 0x02 set to 0x00), use the command:

    i2cwrite 0x1E 0x02 0x00

To read the current measurement (data registers at internal addresses 0x03 to
0x08), use the commands

    # set address pointer
    i2cwrite 0x1E 0x03
    # read 6 bytes
    i2cread  0x1E 6

## MODES

`EZTool`

## SEE ALSO

`i2cread`(1ez)

*)
Procedure TEZTool.I2CWrite(ObjC : Integer; ObjV: PPTcl_Object);
Var Buf  : Array[0..63] of Byte;
    Addr : Cardinal;
    I    : Integer;
Begin
  CheckMode([mdEZTool]);
  // eewrite addr b0 b1 b2 ...
  if ObjC < 3 then
    raise Exception.Create('Invalid parameters');
  Addr := ObjV^[1].AsInteger(FTCL);
  if Addr >= $0100 then
    raise Exception.Create('Maximum start address is 0x00FF');
  if (Addr and $000F) + (ObjC-2) > 16 then
    raise Exception.Create('You can not cross a 16-byte-page boundary');
  For I := 0 to Min(ObjC-3,High(Buf)) do
    Buf[I] := ObjV^[I+2].AsInteger(FTCL);
  HexDump(Addr,Buf,ObjC-2);
  FEZToolDevice.I2CWrite(Addr,Buf,ObjC-2);
End;

(*****************************************************************************)
(***  TCL Functions: Mode: User  *********************************************)
(*****************************************************************************)

(*ronn
claim(1ez) -- claim a USB interface
===================================

## SYNOPSYS

`claim` <intf> <alt>

## DESCRIPTION

Before packets to and from a USB device can be transferred, the corresponding
interface must be claimed using `claim`. Specify the according
_bInterfaceNumber_ as <intf> and _bAlternateSetting_ as <alt>.

## EXAMPLES

The EZ-Tools firmware can be used from mode `User` too. For each command, the
USB communication must be performed "by hand". To query the firmware version,
first the command CMD_GET_VERSION is sent as a vendor specific control message.
The resulting version string is read using a bulk in transfer at endpoint 2.
This is provided by interface 0, alternate setting 1.

    set CMD_GET_VERSION 0x80
    connect -user 0547:CFAA
    claim 0 1
    controlmsg [expr $LIBUSB_ENDPOINT_OUT | $LIBUSB_REQUEST_TYPE_VENDOR | $LIBUSB_RECIPIENT_DEVICE ] \\
               $CMD_GET_VERSION 0 0
    bulkin 2 64

## MODES

`User`

## SEE ALSO

`controlmsg`(1ez), `bulkin`(1ez), `bulkout`(1ez)

*)
Procedure TEZTool.Claim(ObjC : Integer; ObjV : PPTcl_Object);
Begin
  CheckMode([mdUser]);
  // claim intf alt
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');
  FUserDevice.Claim(ObjV^[1].AsInteger(FTCL),ObjV^[2].AsInteger(FTCL));
  // this automatically creates TUSBBulk*Endpoint objects according to device
  // descriptor
End;

(*ronn
controlmsg(1ez) -- send control message
=======================================

## SYNOPSYS

`controlmsg` <bmRequestType> <bRequest> <wValue> <wIndex> [<length>|<b0> <b1> <b2> ...]

## DESCRIPTION

Send a control transfer to the USB device. The arguments <bmRequestType>,
<bRequest>, <wValue> and <wIndex> specify the according USB transfer fields.

You can use the following constants to build the value of <bmRequestType>:

    $LIBUSB_ENDPOINT_IN             = 0x80
    $LIBUSB_ENDPOINT_OUT            = 0x00
    $LIBUSB_REQUEST_TYPE_STANDARD   = 0x00 << 5
    $LIBUSB_REQUEST_TYPE_CLASS      = 0x01 << 5
    $LIBUSB_REQUEST_TYPE_VENDOR     = 0x02 << 5
    $LIBUSB_REQUEST_TYPE_RESERVED   = 0x03 << 5
    $LIBUSB_RECIPIENT_DEVICE        = 0x00
    $LIBUSB_RECIPIENT_INTERFACE     = 0x01
    $LIBUSB_RECIPIENT_ENDPOINT      = 0x02
    $LIBUSB_RECIPIENT_OTHER         = 0x03

For the standard device USB requests, the following constants are available
for <bRequest>:

    $LIBUSB_REQUEST_GET_STATUS         = 0x00
    $LIBUSB_REQUEST_CLEAR_FEATURE      = 0x01
    $LIBUSB_REQUEST_SET_FEATURE        = 0x03
    $LIBUSB_REQUEST_SET_ADDRESS        = 0x05
    $LIBUSB_REQUEST_GET_DESCRIPTOR     = 0x06
    $LIBUSB_REQUEST_SET_DESCRIPTOR     = 0x07
    $LIBUSB_REQUEST_GET_CONFIGURATION  = 0x08
    $LIBUSB_REQUEST_SET_CONFIGURATION  = 0x09
    $LIBUSB_REQUEST_GET_INTERFACE      = 0x0A
    $LIBUSB_REQUEST_SET_INTERFACE      = 0x0B
    $LIBUSB_REQUEST_SYNCH_FRAME        = 0x0C

Depending on the value of bit 7 of <bmRequestType>, the transfer is a control
IN transfer ($LIBUSB_ENDPOINT_IN = 0x80) or a control OUT transfer
($LIBUSB_ENDPOINT_OUT = 0x00).

For control IN transfers you can specify the optional argument <length>. Then
up to <length> bytes are received with the control transfer in the data phase.

To send data with a control OUT Transfer you can supply additional data bytes
<b0>, ...

It is not necessary to `claim`(1ez) an interface before using `controlmsg` when
the control message is sent to the device ($LIBUSB_RECIPIENT_DEVICE).

## EXAMPLES

For an example see `claim`(1ez).

## MODES

`User`

## SEE ALSO

`claim`(1ez)

*)
Procedure TEZTool.ControlMsg(ObjC : Integer; ObjV : PPTcl_Object);
Var OutRequest : Boolean;
    RequestType:Byte;
    Request:Byte;
    Value:Word;
    Index:Word;
    Buf : PByteArray;
    Length:LongInt;
    Result :LongInt;
    I : Integer;
Begin
  CheckMode([mdUser]);
  // controlmsg bmRequestType bRequest wValue wIndex [length|data|b0 b1 b2 ...]   # returns data if it is an in-request
  if ObjC < 5 then
    raise Exception.Create('Invalid parameters');

  RequestType := ObjV^[1].AsInteger(FTCL);
  Request     := ObjV^[2].AsInteger(FTCL);
  Value       := ObjV^[3].AsInteger(FTCL);
  Index       := ObjV^[4].AsInteger(FTCL);
  Buf         := Nil;
  Length      := 0;  // default

  OutRequest := (RequestType and LIBUSB_ENDPOINT_DIR_MASK = 0);

  if OutRequest then
    Begin
      // out request: 5 or more prameters are allowed
      Length := ObjC - 5;
      if Length > 0 then
        Begin
          GetMem(Buf,Length);
          For I := 0 to Length-1 do
            Buf^[I] := ObjV^[I+5].AsInteger(FTCL);
          HexDump($0000,Buf^,Length);
        End;
    End
  else
    Begin
      // in request: 5 or 6 parameters are allowed (length is optional)
      if ObjC > 6 then
        raise Exception.Create('Invalid parameters');
      if ObjC = 6 then
        Begin
          Length := ObjV^[5].AsInteger(FTCL);
          GetMem(Buf,Length);
        End;
    End;

  //WriteLn('RequestType = 0x',IntToHex(RequestType,2),', Request = 0x',IntToHex(Request,2),
  //        ', Value = 0x',IntToHex(Value,4),', Index = 0x',IntToHex(Index,4),
  //        ', Length = ',Length);

  // Control Message
  Result := FUserDevice.Control.ControlMsg(RequestType,Request,Value,Index,Buf^,Length,100);

  if Result < 0  then
    Begin
      if Length > 0 then
        FreeMem(Buf);
      raise Exception.CreateFmt('Error during control message (%d): %s',[-Result,SysErrorMessage(-Result)]);
    End;

  if Length > 0 then
    Begin
      if OutRequest then
        WriteLn('Sent ',Result,' bytes')
      else
        Begin
          WriteLn('Received ',Result,' bytes');
          HexDump($0000,Buf^,Result);
        End;
      FreeMem(Buf);
    End;
End;

(*ronn
bulkin(1ez) -- issue a bulk in transfer
=======================================

## SYNOPSYS

`bulkin` <ep> <length>

## DESCRIPTION

Perform a USB bulk IN transfer from endpoint <ep> with up to <length> bytes.

You have to `claim`(1ez) an interface before bulk transfers. Only the endpoints
specified by the interface are available.

## EXAMPLES

For an example see `claim`(1ez).

## MODES

`User`

## SEE ALSO

`claim`(1ez), `bulkout`(1ez)

*)
Procedure TEZTool.BulkIn(ObjC : Integer; ObjV : PPTcl_Object);
Var EP     : Integer;
    Length : Integer;
    Buf    : PByteArray;
    Result : Integer;
Begin
  CheckMode([mdUser]);
  if not FUserDevice.HaveInterface then
    raise Exception.Create('You must first ''claim'' an interface.');

  // bulkin  ep length                 # "connect" automatically creates TUSBBulk*Endpoints according to device descriptor
  if ObjC <> 3 then
    raise Exception.Create('Invalid parameters');

  EP     := ObjV^[1].AsInteger(FTCL);
  Length := ObjV^[2].AsInteger(FTCL);
  if Length > $FFFF then
    raise Exception.Create('maximum length is 0xFFFF');
  GetMem(Buf,Length);

  //WriteLn('EP = ',EP,' IN, Length = ',Length,', Buf = ',IntToHex(PtrUInt(Buf),SizeOf(PtrUInt)*2));
  Result := FUserDevice.BulkIn(EP or LIBUSB_ENDPOINT_IN,Buf^,Length,100);

  if Result < 0  then
    Begin
      FreeMem(Buf);
      raise Exception.CreateFmt('Error during bulk in transfer (%d): %s',[-Result,SysErrorMessage(-Result)]);
    End;

  WriteLn('Received ',Result,' bytes');
  if Result > 0 then
    HexDump($0000,Buf^,Result);

  FreeMem(Buf);
End;

(*ronn
bulkout(1ez) -- issue a bulk out transfer
=========================================

## SYNOPSYS

`bulkout` <ep> <b0> <b1> <b2> ...

## DESCRIPTION

Perform a USB bulk OUT transfer to endpoint <ep> with the data byte <b0>,
<b1>, ...

You have to `claim`(1ez) an interface before bulk transfers. Only the endpoints
specified by the interface are available.

## EXAMPLES

Send data to a bulk OUT endpoint.

    bulkout 2 0x10 0x3F 0x55 0xAA

## MODES

`User`

## SEE ALSO

`claim`(1ez), `bulkin`(1ez)

*)
Procedure TEZTool.BulkOut(ObjC : Integer; ObjV : PPTcl_Object);
Var EP     : Integer;
    Length : Integer;
    I      : Integer;
    Buf    : PByteArray;
    Result : Integer;
Begin
  CheckMode([mdUser]);
  if not FUserDevice.HaveInterface then
    raise Exception.Create('You must first ''claim'' an interface.');

  // bulkout ep data|b0 b1 b2 ...
  if ObjC < 3 then
    raise Exception.Create('Invalid parameters');

  EP     := ObjV^[1].AsInteger(FTCL);
  Length := ObjC - 2;
  if Length > $FFFF then
    raise Exception.Create('maximum length is 0xFFFF');
  GetMem(Buf,Length);

  For I := 0 to Length-1 do
    Buf^[I] := ObjV^[I+2].AsInteger(FTCL);

  HexDump($0000,Buf^,Length);

  //WriteLn('EP = ',EP,' OUT, Length = ',Length,', Buf = ',IntToHex(PtrUInt(Buf),SizeOf(PtrUInt)*2));
  Result := FUserDevice.BulkOut(EP or LIBUSB_ENDPOINT_OUT,Buf^,Length,100);

  if Result < 0  then
    Begin
      FreeMem(Buf);
      raise Exception.CreateFmt('Error during bulk out transfer (%d): %s',[-Result,SysErrorMessage(-Result)]);
    End;

  WriteLn('Sent ',Result,' bytes');

  FreeMem(Buf);
End;

(*****************************************************************************)
(***  Main Program  **********************************************************)
(*****************************************************************************)

Var Dev        : TEZTool;
    RunScripts : Boolean;
    BatchMode  : Boolean;
    ExitStatus : Integer;

Procedure Usage(ExitCode:Byte);
Begin
  WriteLn('EZTool');
  WriteLn;
  WriteLn('Usage: eztool [-h|--help] [-b] [-f filename] [-c string] [-n]');
  WriteLn;
  WriteLn('  -h, --help   Print a usage information and exit.');
  WriteLn;
  WriteLn('  -b           Batch mode. eztool will not wait for user input but quit');
  WriteLn('               directly after the startup scripts, the scripts given with -f');
  WriteLn('               parameters and the commands given with -c parameters were');
  WriteLn('               executed.');
  WriteLn;
  WriteLn('  -f filename  Execute the Tcl script in filename at program start.');
  WriteLn;
  WriteLn('  -c string    Execute the commands given in string.');
  WriteLn;
  WriteLn('  -n           Do not execute the start scripts (/etc/eztool/... TODO )');
  WriteLn;
  Halt(ExitCode);
End;

(**
 * Local helper to execute TCL commands
 *
 * The TCL commands are evaluated by Tcl. Then the result is checked and
 * printed to the screen. If 'exit' was used, an exception is raised. This is
 * caught in the main program.
 *)
Procedure Eval(St:String);
Var Code : Integer;
Begin
  // execute command
  Code := Dev.FTCL.Eval(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := Dev.FTCL.GetStringResult;   // use 'Line' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := Dev.FCmdLine.ExitStatus;
  if (ExitStatus >= 0) or (Code <> TCL_OK) then
    raise Exception.Create(St);
End;

Procedure EvalFile(St:String);
Var Code : Integer;
Begin
  // execute script file
  Code := Dev.FTCL.EvalFile(St);

  // handle result
  if (Code <> TCL_OK) then
    Write('Error: ');
  St := Dev.FTCL.GetStringResult;   // use 'Line' to avoid additional variable
  if St > '' then
    WriteLn(St);

  // handle if 'exit' was called
  ExitStatus := Dev.FCmdLine.ExitStatus;
  if (ExitStatus >= 0) or (Code <> TCL_OK) then
    raise Exception.Create(St);
End;

(**
 * Parse the command line parameters
 *
 * The parameters are parsed in two phases. Phase 0 only sets internal variables
 * which influence the behavior of the program (-b, -n). In phase 1 the scripts
 * and commands specified with parameters -f and -c, respectively, are exeucuted.
 *
 * @param Phase
 *)
Procedure ParseParams(Phase:Integer);
Var I : Integer;
Begin
  I := 1;
  While I <= ParamCount do
    Begin
      if (ParamStr(I) = '-h') or (ParamStr(I) = '--help') then
        // help
        Usage(0)
      else if ParamStr(I) = '-b' then
        // switch to batch mode
        BatchMode := true
      else if ParamStr(I) = '-f' then
        Begin
          // execute script file
          Inc(I);
          if Phase = 1 then
            EvalFile(ParamStr(I));
        End
      else if ParamStr(I) = '-c' then
        Begin
          // execute command
          Inc(I);
          if Phase = 1 then
            Eval(ParamStr(I));
        End
      else if ParamStr(I) = '-n' then
        // don't run startup scripts
        RunScripts := false
      else
        // error
        Usage(1);
      Inc(I);
    End;
End;

Procedure ExecuteStartupScripts;
  Procedure EvalFileIfExists(Filename:String);
  Begin
    if FileExists(Filename) then
      Begin
        //WriteLn('Executing ',Filename);
        EvalFile(Filename);
      End
    else
      //WriteLn('Cannot find ',Filename);
  End;
Var List : TStringList;
    St   : String;
Begin
  // run /etc/eztool/eztoolrc
  EvalFileIfExists('/etc/eztool/eztoolrc');
  // run /etc/eztool/eztoolrc.d/*.tcl
  List := FileSearchGlobList('/etc/eztool/eztoolrc.d/*.tcl');
  List.Sorted := true;  // automatically sorts the list
  for St in List do
    Begin
      //WriteLn('Executing ',St);
      EvalFile(St);
    End;
  // run ~/.eztoolrc
  EvalFileIfExists(fpGetEnv('HOME')+'/.eztoolrc');
End;

Begin
  Dev := TEZTool.Create;
  // initialize default values
  ExitStatus := 0;
  RunScripts := true;
  BatchMode  := false;
  // parse parameters to set variables
  ParseParams(0);
  try
    // execute startup scripts
    if RunScripts then
      ExecuteStartupScripts;
    // parse parameters to execute scripts and commands given as parameters
    ParseParams(1);
    // show help and run command prompt
    if not BatchMode then
      Begin
        Dev.Help(0,Nil);
        ExitStatus := Dev.Run;
      End;
  finally
    // cleanup, finish
    Dev.Free;
    Halt(ExitStatus);
  End;
End.

