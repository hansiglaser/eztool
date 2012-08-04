Unit USBDeviceDebug;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LibUSB, USB, FGL;

Type

  TEPList   = specialize TFPGMap<Byte,TUSBEndpoint>;

  { TUSBDeviceDebug }

  TUSBDeviceDebug = class(TUSBDevice)
  private
    FInterface  : TUSBInterface;
    FEndpoints  : TEPList;
    Procedure FreeInterface;
    Procedure ClaimInterface(AIntf : PUSBInterfaceDescriptor);
    Function GetHaveInterface : Boolean;
  public
    Constructor Create(ADev:PUSBDevice;AConfig:Integer=-1); override;
    Destructor  Destroy; override;
    Procedure Claim(AInterface,AAltSetting:Byte);
    Function BulkIn (EP:Byte;Out      Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    Function BulkOut(EP:Byte;ConstRef Buf;Length:LongInt;Timeout:LongInt) : LongInt;
    property HaveInterface : Boolean read GetHaveInterface;
    property Control : TUSBDeviceControlEndpoint read FControl;
  End;


Implementation

Uses Utils;

{ TUSBDeviceDebug }

Constructor TUSBDeviceDebug.Create(ADev : PUSBDevice; AConfig : Integer);
Begin
  inherited Create(ADev,AConfig);
  FEndpoints  := TEPList.Create;
  FEndpoints.Duplicates := dupError;
End;

Destructor TUSBDeviceDebug.Destroy;
Begin
  FreeInterface;
  FEndpoints.Free;
  Inherited Destroy;
End;

Procedure TUSBDeviceDebug.Claim(AInterface,AAltSetting:Byte);
Var Intf : PUSBInterfaceDescriptor;
Begin
  // find appropriate interface
  Intf := USBFindInterface(AInterface,AAltSetting,FDevice);
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
    raise Exception.CreateFmt('Invalid endpoint number %d',[EP and USB_ENDPOINT_ADDRESS_MASK]);
  Result := (FEndpoints[EP] as TUSBBulkInEndpoint).Recv(Buf,Length,Timeout);
End;

Function TUSBDeviceDebug.BulkOut(EP : Byte; ConstRef Buf; Length : LongInt; Timeout : LongInt) : LongInt;
Begin
  if FEndpoints.IndexOf(EP) < 0 then
    raise Exception.CreateFmt('Invalid endpoint number %d',[EP and USB_ENDPOINT_ADDRESS_MASK]);
  Result := (FEndpoints[EP] as TUSBBulkOutEndpoint).Send(Buf,Length,Timeout);
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

Procedure TUSBDeviceDebug.ClaimInterface(AIntf : PUSBInterfaceDescriptor);
Var NumEp,IEp : Integer;
    TheEP     : PUSBEndpointDescriptor;
    EP        : TUSBEndpoint;
Begin
  // create and claim new interface
  FInterface := TUSBInterface.Create(Self,AIntf);
  // create objects for all endpoints
  WriteLn('Interface ',AIntf^.bInterfaceNumber,
          ' (Alternate ',AIntf^.bAlternateSetting,')',
          ' "',USBGetString(AIntf^.iInterface),'":');
  NumEp := AIntf^.bNumEndpoints;
  For IEp := 0 to NumEP-1 do
    Begin
      TheEp := @AIntf^.endpoint^[IEp];
      Write('  EP ',   TheEP^.bEndpointAddress and USB_ENDPOINT_ADDRESS_MASK:2,
            ' ',Select(TheEP^.bEndpointAddress and USB_ENDPOINT_DIR_MASK <> 0,'IN ','OUT'),
            ' ',Select(TheEP^.bmAttributes and USB_ENDPOINT_TYPE_MASK,['Control','Isochronous','Bulk','Interrupt']));
      EP := Nil;
      if TheEP^.bEndpointAddress and USB_ENDPOINT_DIR_MASK <> 0 then
        Begin
          // IN endpoint
          Case TheEP^.bmAttributes and USB_ENDPOINT_TYPE_MASK of
            USB_ENDPOINT_TYPE_CONTROL     : ;
            USB_ENDPOINT_TYPE_ISOCHRONOUS : ;
            USB_ENDPOINT_TYPE_BULK        : EP := TUSBBulkInEndpoint     .Create(FInterface,TheEP);
            USB_ENDPOINT_TYPE_INTERRUPT   : EP := TUSBInterruptInEndpoint.Create(FInterface,TheEP);
          End;
        End
      else
        Begin
          // OUT endpoint
          Case TheEP^.bmAttributes and USB_ENDPOINT_TYPE_MASK of
            USB_ENDPOINT_TYPE_CONTROL     : ;
            USB_ENDPOINT_TYPE_ISOCHRONOUS : ;
            USB_ENDPOINT_TYPE_BULK        : EP := TUSBBulkOutEndpoint     .Create(FInterface,TheEP);
            USB_ENDPOINT_TYPE_INTERRUPT   : EP := TUSBInterruptOutEndpoint.Create(FInterface,TheEP);
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

