Unit USBDeviceDebug;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUsb, LibUsbOop, FGL;

Type

  TEPList   = specialize TFPGMap<Byte,TLibUsbEndpoint>;

  { TUSBDeviceDebug }

  TUSBDeviceDebug = class(TLibUsbDevice)
  private
    FInterface  : TLibUsbInterface;
    FEndpoints  : TEPList;
    Procedure FreeInterface;
    Procedure ClaimInterface(AIntf : Plibusb_interface_descriptor);
    Function GetHaveInterface : Boolean;
  public
    Constructor Create(AContext:TLibUsbContext;ADevice:Plibusb_device); override;
    Constructor Create(AContext:TLibUsbContext;AVID,APID:Word);
    Destructor  Destroy; override;
    Procedure Claim(AInterface,AAltSetting:Byte);
    Function BulkIn (EP:Byte;Out      Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    Function BulkOut(EP:Byte;ConstRef Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    property HaveInterface : Boolean read GetHaveInterface;
  End;


Implementation

Uses Utils;

{ TUSBDeviceDebug }

Constructor TUSBDeviceDebug.Create(AContext:TLibUsbContext;ADevice : Plibusb_device);
Begin
  inherited Create(AContext,ADevice);
  FEndpoints  := TEPList.Create;
  FEndpoints.Duplicates := dupError;
End;

Constructor TUSBDeviceDebug.Create(AContext : TLibUsbContext; AVID, APID : Word);
Begin
  inherited;
End;

Destructor TUSBDeviceDebug.Destroy;
Begin
  FreeInterface;
  FEndpoints.Free;
  Inherited Destroy;
End;

Procedure TUSBDeviceDebug.Claim(AInterface,AAltSetting:Byte);
Var Intf : Plibusb_interface_descriptor;
Begin
  // find appropriate interface
  Intf := FindInterface(AInterface,AAltSetting);
  // not found?
  if Intf = Nil then
    raise Exception.CreateFmt('Could not find interface %d alt. setting %d',[AInterface,AAltSetting]);
  // free and release old interface and all associated endpoints
  FreeInterface;
  // create and claim new interface and create endpoint objects
  ClaimInterface(Intf);
End;

Function TUSBDeviceDebug.BulkIn(EP : Byte; Out Buf; Length : LongInt; Timeout : LongInt) : LongInt;
Begin
  if FEndpoints.IndexOf(EP) < 0 then
    raise Exception.CreateFmt('Invalid endpoint number %d',[EP and LIBUSB_ENDPOINT_ADDRESS_MASK]);
  Result := (FEndpoints[EP] as TLibUsbBulkInEndpoint).Recv(Buf,Length,Timeout);
End;

Function TUSBDeviceDebug.BulkOut(EP : Byte; ConstRef Buf; Length : LongInt; Timeout : LongInt) : LongInt;
Begin
  if FEndpoints.IndexOf(EP) < 0 then
    raise Exception.CreateFmt('Invalid endpoint number %d',[EP and LIBUSB_ENDPOINT_ADDRESS_MASK]);
  Result := (FEndpoints[EP] as TLibUsbBulkOutEndpoint).Send(Buf,Length,Timeout);
End;

Procedure TUSBDeviceDebug.FreeInterface;
Var I : Integer;
Begin
  // free all endpoint classes
  For I := 0 to FEndpoints.Count-1 do
    FEndpoints.Data[I].Free;
  // free and release old interface
  FInterface.Free;
End;

Procedure TUSBDeviceDebug.ClaimInterface(AIntf : Plibusb_interface_descriptor);
Var NumEp,IEp : Integer;
    TheEP     : Plibusb_endpoint_descriptor;
    EP        : TLibUsbEndpoint;
Begin
  // create and claim new interface
  FInterface := TLibUsbInterface.Create(Self,AIntf);
  // create objects for all endpoints
  WriteLn('Interface ',AIntf^.bInterfaceNumber,
          ' (Alternate ',AIntf^.bAlternateSetting,')',
          ' "',FControl.GetString(AIntf^.iInterface),'":');
  NumEp := AIntf^.bNumEndpoints;
  For IEp := 0 to NumEP-1 do
    Begin
      TheEp := @AIntf^.endpoint^[IEp];
      Write('  EP ',   TheEP^.bEndpointAddress and LIBUSB_ENDPOINT_ADDRESS_MASK:2,
            ' ',Select(TheEP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK <> 0,'IN ','OUT'),
            ' ',Select(TheEP^.bmAttributes and LIBUSB_TRANSFER_TYPE_MASK,['Control','Isochronous','Bulk','Interrupt']));
      EP := Nil;
      if TheEP^.bEndpointAddress and LIBUSB_ENDPOINT_DIR_MASK <> 0 then
        Begin
          // IN endpoint
          Case TheEP^.bmAttributes and LIBUSB_TRANSFER_TYPE_MASK of
            LIBUSB_TRANSFER_TYPE_CONTROL     : ;
            LIBUSB_TRANSFER_TYPE_ISOCHRONOUS : ;
            LIBUSB_TRANSFER_TYPE_BULK        : EP := TLibUsbBulkInEndpoint     .Create(FInterface,TheEP);
            LIBUSB_TRANSFER_TYPE_INTERRUPT   : EP := TLibUsbInterruptInEndpoint.Create(FInterface,TheEP);
          End;
        End
      else
        Begin
          // OUT endpoint
          Case TheEP^.bmAttributes and LIBUSB_TRANSFER_TYPE_MASK of
            LIBUSB_TRANSFER_TYPE_CONTROL     : ;
            LIBUSB_TRANSFER_TYPE_ISOCHRONOUS : ;
            LIBUSB_TRANSFER_TYPE_BULK        : EP := TLibUsbBulkOutEndpoint     .Create(FInterface,TheEP);
            LIBUSB_TRANSFER_TYPE_INTERRUPT   : EP := TLibUsbInterruptOutEndpoint.Create(FInterface,TheEP);
          End;
        End;
      if assigned(EP) then
        FEndpoints.Add(TheEP^.bEndpointAddress,EP)
      else
        Write(' (unsupported)');
      WriteLn;
    End;
End;

Function TUSBDeviceDebug.GetHaveInterface : Boolean;
Begin
  Result := assigned(FInterface);
End;

End.

