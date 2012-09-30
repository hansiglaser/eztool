(***************************************************************************
 *   Copyright (C) 2012 by Johann Glaser <Johann.Glaser@gmx.at>            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************)

Unit Device;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,SysUtils,BaseUnix,Utils,LibUsb,LibUsbOop,LibUsbUtil,EZUSB;

Const
  USBVendConf   = $0547;
  USBProdConf   = $CFAA;
  FirmwareName  = 'firmware.ihx';

Const
  EP_IN    =  2 or LIBUSB_ENDPOINT_IN;
  EP_OUT   =  2 or LIBUSB_ENDPOINT_OUT;

Const
  CMD_GET_VERSION   = $80;
  CMD_GET_STATUS    = $81;
  CMD_SETUP_IOPORT  = $82;    // PORTxCFG and OEx
  CMD_SET_IOPORT    = $83;    // write OUTx
  CMD_GET_IOPORT    = $84;    // read INx
  CMD_READ_EEPROM   = $85;    // read from EEPROM
  CMD_WRITE_EEPROM  = $86;    // write to EEPROM
  CMD_READ_XDATA    = $87;    // read from XDATA
  CMD_WRITE_XDATA   = $88;    // write to XDATA
  CMD_READ_I2C      = $89;    // generic read at I2C bus
  CMD_WRITE_I2C     = $8A;    // generic write at I2C bus


Const
  EZToolUSBConfiguration = 1;
  EZToolUSBInterface     = 0;
  EZToolUSBAltInterface  = 0;

Type

  TPort = (ptA,ptB,ptC);
  TStatus = Byte;  // TODO: define type as you like

  { TEZToolDevice }

  TEZToolDevice = Class(TLibUsbDeviceWithFirmware)
  protected
    FUidVendorEmpty   : Word;   // unconfigured
    FUidProductEmpty  : Word;
    FUidVendorEzTool  : Word;   // configured
    FUidProductEzTool : Word;
    FFirmwareFile     : String;
    { USB variables }
    FInterface       : TLibUsbInterface;
    FEPIn            : TLibUsbBulkInEndpoint;
    FEPOut           : TLibUsbBulkOutEndpoint;
    Procedure Configure(ADev:Plibusb_device); override;
  public
    { class methods }
    Constructor Create(AContext:TLibUsbContext;AMatchUnconfigured:TLibUsbDeviceMatchClass;AFirmwareFile:String;AMatchConfigured:TLibUsbDeviceMatchClass);
    Destructor  Destroy; override;
    Class Function FindFirmware(AName, AProgram : String) : String;
  protected
    Function  SendCommand(Cmd:Byte;Value:Word;Index:Word) : Integer;
  private
    Function  Port2Index(APort:TPort) : Word;
    Function  Index2Port(AIndex:Word) : TPort;
  public
    Function  GetVersion:String;
    Function  GetStatus:TStatus;
    Procedure IOSetup(APort:TPort;AConfig,AOutEnable:Byte);
    Procedure IOSet  (APort:TPort;AValue:Byte);
    Function  IOGet  (APort:TPort) : Byte;
    Function  EERead (Addr:Word;Out   Buf;Len:Byte) : Integer;
    Function  EEWrite(Addr:Word;Const Buf;Len:Byte) : Integer;
    Function  XRead  (Addr:Word;Out   Buf;Len:Word) : Integer;
    Function  XWrite (Addr:Word;Const Buf;Len:Word) : Integer;
    Function  I2CRead (Addr:Byte;Out   Buf;Len:Byte) : Integer;
    Function  I2CWrite(Addr:Byte;Const Buf;Len:Byte) : Integer;
  End;

Implementation

(**
 * Constructor
 *
 * @param AContext            libusb context
 * @param AMatchUnconfigured  device matcher class for the unconfigured device
 * @param AFirmwareFile       filename of the firmware Intel Hex file. e.g. as retured by FindFirmware()
 * @param AMatchConfigured    device matcher class for the configured device
 *)
Constructor TEZToolDevice.Create(AContext:TLibUsbContext;AMatchUnconfigured:TLibUsbDeviceMatchClass;AFirmwareFile:String;AMatchConfigured:TLibUsbDeviceMatchClass);
Begin
  FFirmwareFile     := AFirmwareFile;
  { uses MatchUnconfigured to find an unconfigured device, then Configure to
    do the configuration and finally MatchConfigured to find the configured
    device. }
  inherited Create(AContext,AMatchUnconfigured,AMatchConfigured);
  SetConfiguration(EZToolUSBConfiguration);

  // create handlers for the endpoints (and the interface they belong to)
  FInterface       := TLibUsbInterface.Create(Self,FindInterface(EZToolUSBInterface,EZToolUSBAltInterface));
  FEPIn            := TLibUsbBulkInEndpoint. Create(FInterface,FInterface.FindEndpoint(EP_IN));
  FEPOut           := TLibUsbBulkOutEndpoint.Create(FInterface,FInterface.FindEndpoint(EP_OUT));
End;

(**
 * Destructor
 *)
Destructor  TEZToolDevice.Destroy;
Begin
  { If no configured devices were found in the constructor (or any other
    exception occured there), this destructor is automatically called. In this
    case we don't have the USB stuff setup (and nothing else), so we don't
    do the freeing and finalization stuff. }
  FInterface.Free;
  inherited Destroy;
End;

(**
 * Send a command to the device
 *
 * Commands are sent as control transfers.
 *
 * This function does not use the data phase.
 *)
Function TEZToolDevice.SendCommand(Cmd:Byte;Value:Word;Index:Word):Integer;
Begin
  Result := FControl.ControlMsg(
    { bmRequestType } LIBUSB_ENDPOINT_OUT or LIBUSB_REQUEST_TYPE_VENDOR or LIBUSB_RECIPIENT_DEVICE,
    { bRequest      } Cmd,
    { wValue        } Value,
    { wIndex        } Index,
    { Timeout       } 100);
End;

Function TEZToolDevice.Port2Index(APort:TPort):Word;
Begin
  Case APort of
    ptA : Result := 0;
    ptB : Result := 1;
    ptC : Result := 2;
  else
    raise Exception.Create('Port2Index: Invalid port in enumeration');
  End;
End;

Function TEZToolDevice.Index2Port(AIndex:Word):TPort;
Begin
  Case AIndex of
    0 : Result := ptA;
    1 : Result := ptB;
    2 : Result := ptC;
  else
    raise Exception.Create('Index2Port: Invalid port in word');
  End;
End;

Function TEZToolDevice.GetVersion : String;
Var R   : LongInt;
    Buf : Array[0..63] of Char;
Begin
  R := SendCommand(CMD_GET_VERSION,0,0);
  if R < 0 then
    raise ELibUsb.Create(R,'GetVersion SendCommand');
  R := FEPIn.Recv(Buf,SizeOf(Buf),100);
  if R < 0 then
    raise ELibUsb.Create(R,'GetVersion EP Recv');
  SetLength(Result,R);
  Move(Buf,Result[1],R);
End;
Function TEZToolDevice.GetStatus : TStatus;
Var R : LongInt;
Begin
  R := SendCommand(CMD_GET_STATUS,0,0);
  if R < 0 then
    raise ELibUsb.Create(R,'GetStatus SendCommand');
  R := FEPIn.Recv(Result,Sizeof(Result),100);
  if R <> Sizeof(Result) then
    raise ELibUsb.Create(R,'GetStatus EP Recv');
End;

Procedure TEZToolDevice.IOSetup(APort:TPort;AConfig,AOutEnable:Byte);
Var R : LongInt;
Begin
  R := SendCommand(CMD_SETUP_IOPORT,AConfig or (AOutEnable shl 8),Port2Index(APort));
  if R < 0 then
    raise ELibUsb.Create(R,'IOSetup SendCommand');
End;

Procedure TEZToolDevice.IOSet(APort:TPort;AValue:Byte);
Var R : LongInt;
Begin
  R := SendCommand(CMD_SET_IOPORT,AValue,Port2Index(APort));
  if R < 0 then
    raise ELibUsb.Create(R,'IOSet SendCommand');
End;

Function TEZToolDevice.IOGet(APort:TPort):Byte;
Var R : LongInt;
Begin
  R := SendCommand(CMD_GET_IOPORT,0,Port2Index(APort));
  if R < 0 then
    raise ELibUsb.Create(R,'IOGet SendCommand');
  R := FEPIn.Recv(Result,Sizeof(Result),100);
  if R <> Sizeof(Result) then
    raise ELibUsb.Create(R,'IOGet EP Recv');
End;

Function TEZToolDevice.EERead(Addr:Word;Out Buf;Len:Byte):Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_READ_EEPROM,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'EERead SendCommand');
  R := FEPIn.Recv(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'EERead EP Recv');
End;

Function TEZToolDevice.EEWrite(Addr:Word;Const Buf;Len:Byte):Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_WRITE_EEPROM,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'EEWrite SendCommand');
  R := FEPOut.Send(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'EEWrite EP Send');
End;

Function TEZToolDevice.XRead(Addr:Word;Out Buf;Len:Word):Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_READ_XDATA,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'XRead SendCommand');
  R := FEPIn.Recv(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'XRead EP Recv');
End;

Function TEZToolDevice.XWrite(Addr:Word;Const Buf;Len:Word):Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_WRITE_XDATA,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'XWrite SendCommand');
  R := FEPOut.Send(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'XWrite EP Send');
End;

Function TEZToolDevice.I2CRead(Addr : Byte; Out Buf; Len : Byte) : Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_READ_I2C,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'I2CRead SendCommand');
  R := FEPIn.Recv(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'I2CRead EP Recv');
End;

Function TEZToolDevice.I2CWrite(Addr : Byte; Const Buf; Len : Byte) : Integer;
Var R : LongInt;
Begin
  R := SendCommand(CMD_WRITE_I2C,Len,Addr);
  if R < 0 then
    raise ELibUsb.Create(R,'I2CWrite SendCommand');
  R := FEPOut.Send(Buf,Len,1000);
  if R <> Len then
    raise ELibUsb.Create(R,'I2CWrite EP Send');
End;

Procedure TEZToolDevice.Configure(ADev:Plibusb_device);
Var EZUSB : TLibUsbDeviceEZUSB;
Begin
  WriteLn('Using Firmware file "'+FFirmwareFile+'" to configure devices.');
  // create a temporary TUSBDeviceEZUSB object to download the firmware
  EZUSB := TLibUsbDeviceEZUSB.Create(FContext,ADev);
  EZUSB.SetConfiguration(ANCHOR_USB_CONFIG);
  EZUSB.DownloadFirmware(FFirmwareFile);
  EZUSB.Free;
End;

class Function TEZToolDevice.FindFirmware(AName,AProgram:String):String;
Var Path : String;
Begin
  { search controller firmware file }
  Path := '.:~/.'+AProgram+':'
          +fpGetEnv(UpperCase(AProgram)+'FIRMWAREPATH')+':'
          +ExtractFilePath(ExpandFileName(ParamStr(0)))+':'
          +fpGetEnv('PATH')
          +':/etc/'+AProgram;
  ParamStr(0); { strange, this seems to be necessary to free memory previously reserved by ParamStr }
  Result := FindFileInPath(AName,Path);
  if Result = '' then
    raise Exception.Create('Couldn''t find firmware file "'+AName+'" in search path "'+Path+'". You might try to set the environment variable "'+UpperCase(AProgram)+'FIRMWAREPATH".');
End;

End.

