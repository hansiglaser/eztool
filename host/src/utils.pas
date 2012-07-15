{$MODE OBJFPC}
Unit Utils;

Interface
Uses SysUtils,StrUtils,Classes,BaseUnix,Unix;

Const HexChars = '0123456789ABCDEF';

Function HexToInt(St:ShortString):Int64;
Function Str2Int(St:ShortString):LongInt;
Function StrReplace(St:String;Src,Dst:String):String;
Procedure WriteDebug(St:AnsiString);
Function FileSearchGlob(Glob:AnsiString):AnsiString;
Function GetHeapSize : SizeInt;
Procedure HexDump(Addr:Integer;Var Buf;Length:SizeUInt);
Procedure HexDump(Var Buf;Length:SizeUInt);
Function StrHexDump(Var Buf; Length: SizeUInt) : AnsiString;
Function StrToHex(St:AnsiString):AnsiString;
Function RemoveFileExt(Filename,Ext:String) : String;
Function Indent(St:AnsiString;Num:Integer) : AnsiString;
Function PtrToStr(P:Pointer) : String;
Function CreateSplit(St:String;Delim:Char) : TStringList;
Function IntToBin(Value:Integer;Width:Integer) : String;
Function LoadFile(Const FileName : TFileName) : AnsiString;
Function MatchOption(Entry,FullOption:String;MinLength:Integer) : Boolean;
Function Exec(Command:AnsiString;Args:Array of AnsiString;Env:Array of AnsiString;Wait:Boolean):CInt;
Function SplitUsbID(IDs:String;Out idVendor,idProduct : Word) : Boolean;
Function FindFileInPath(AName,APath:String) : String;
Function Select(B:Boolean;T,F:String):String;
Function Select(I : Integer; Const S:Array of String) : String;
Function GetUSec : UInt64;

Implementation

Function HexToInt(St:ShortString):Int64;
Var I,J : LongInt;
Begin
  Result := 0;
  For I := 1 to Length(St) do
    Begin
      J := Pos(UpCase(St[I]),HexChars);
      if J = 0 then
        raise EConvertError.CreateFmt('Wrong hex char "%s" in hex number "%s" at position %d.',[St[I],St,I]);
      Result := Result shl 4 or (J-1);
    End;
End;

Function Str2Int(St:ShortString):LongInt;
Begin
  try
    Result := StrToInt(St);
  except
    WriteLn('Utils.Str2Int was called with "',St,'"! Returning 0.');
    Result := 0;
  End;
End;

Function StrReplace(St:String;Src,Dst:String):String;
Var L : LongInt;
Begin
  Result := '';
  repeat
    L := Pos(Src,St);
    if L = 0 then Exit(Result+St);
    Result := Result+Copy(St,1,L-1)+Dst;
    St := Copy(St,L+Length(Src),Length(St));
  Until False;
End;

Procedure WriteDebug(St:AnsiString);
Begin
  WriteLn('[',FormatDateTime('dd.mm.yyyy hh:nn:ss',Now),'] ',St);
End;

Function FileSearchGlob(Glob:AnsiString):AnsiString;
Var SR : TSearchRec;
Begin
  Result := '';
  Glob := ExpandFileName(Glob);
  if FindFirst(Glob,faAnyFile,SR) = 0 then
    Result := ExtractFilePath(Glob) + SR.Name;
  FindClose(SR);
End;

Function GetHeapSize : SizeInt;
Var Status : TFPCHeapStatus;
Begin
  Status := GetFPCHeapStatus;
  Result := Status.CurrHeapUsed;
End;

Procedure HexDump(Addr:Integer;Var Buf;Length:SizeUInt);
Var I : SizeUInt;
    S : ShortString;
Begin
  Write(IntToHex(Addr and $FFF0,4),': ',StringOfChar(' ',3*(Addr and $000F)));
  S := '';
  For I := 0 to Length-1 do
    Begin
      Write(IntToHex((PByte(@Buf + I))^,2),' ');
      if (PByte(@Buf + I)^ >= Ord(' ')) and (PByte(@Buf + I)^ < $80) then
        S := S + Chr(PByte(@Buf + I)^)
      else
        S := S + '.';
      if ((Addr+I) and $000F = $000F) and (I <> Length-1) then
        Begin
          WriteLn('  ',S);
          S := '';
          Write(IntToHex((Addr + I+1) and $FFF0,4),': ');
        End;
    End;
  WriteLn('  ',S);
End;

Procedure HexDump(Var Buf; Length: SizeUInt);
Begin
  HexDump($0000,Buf,Length);
End;

Function StrHexDump(Var Buf; Length: SizeUInt) : AnsiString;
Var I : SizeUInt;
Begin
  if Length <= 0 then Exit('');
  Result := '0000: ';
  For I := 0 to Length-1 do
    Begin
      Result += IntToHex((PByte(@Buf + I))^,2) + ' ';
      if (I and $000F = $000F) and (I <> Length-1) then
        Begin
          Result += ^J;
          Result += IntToHex(I+1,4)+': ';
        End;
    End;
  Result += ^J;
End;

Function StrToHex(St:AnsiString):AnsiString;
Var I : Integer;
    B : Byte;
Begin
  SetLength(Result,Length(St)*2);
  For I := 1 to Length(St) do
    Begin
      B := Ord(St[Length(St)-I+1]);
      Result[I*2  ] := HexChars[ B        and $0F+1];
      Result[I*2-1] := HexChars[(B shr 4) and $0F+1];
    End;
End;

Function RemoveFileExt(Filename,Ext:String) : String;
Begin
  if RightStr(Filename,Length(Ext)) = Ext then
    Result := Copy(Filename,1,Length(Filename) - Length(Ext))
  else
    Result := Filename;
End;

Function Indent(St:AnsiString;Num:Integer) : AnsiString;
Var Prefix : String;
    Resultt : AnsiString;
    A,B    : Cardinal;
Begin
  if St = '' then
    Exit('');
  Prefix := StringOfChar(' ',Num);
  if Pos(^J,St) = 0 then
    Exit(Prefix + St);
  A := 1;
  Resultt := '';
  repeat
    B := PosEx(^J,St,A);
    if B = 0 then break;
    Resultt += Prefix + Copy(St,A,B-A+1);
    A := B+1;
  until A >= Length(St);
  if B = 0 then
    Resultt += Prefix + Copy(St,A,Length(St));
  Result := Resultt;
End;

Function PtrToStr(P:Pointer) : String;
Begin
  Result := '$' + IntToHex(PtrUInt(P),sizeof(P)*2) + '^';
End;

Function CreateSplit(St:String;Delim:Char):TStringList;
Begin
  Result := TStringList.Create;
  Result.Delimiter       := Delim;
  Result.StrictDelimiter := False;   // ignore multiple occurences of delimiter
  Result.DelimitedText   := St;
End;

Function IntToBin(Value:Integer;Width:Integer):String;
Var I : Integer;
Begin
  SetLength(Result,Width);
  For I := Width downto 1 do
    Begin
      Result[I] := Chr(Ord('0') + (Value and $0001));
      Value := Value shr 1;
    End;
End;

Function LoadFile(Const FileName : TFileName) : AnsiString;
Begin
  With TFileStream.Create(FileName,fmOpenRead) do
    Begin
      try
        SetLength(Result, Size);
        Read(Pointer(Result)^, Size);
      except
        Result := '';  // Deallocates memory
        Free;
        raise;
      End;
      Free;
    End;
End;

(**
 * Returns true if Entry is FullOption with at least MinLength chars
 *
 * Examples: FullOption = '-configured'
 *   '-c'            -> true
 *   '-co'           -> true
 *   '-conf'         -> true
 *   '-conv'         -> false
 *   '-configured'   -> true
 *   '-configureddd' -> false
 *
 * MinLength referres to the total length including the preceding '-' (or '--'
 * or anything you like).
 * Examples: MinLength = 5
 *   '-c'            -> false
 *   '-co'           -> false
 *   '-conf'         -> true
 *   '-configured'   -> true
 *
 *)
Function MatchOption(Entry, FullOption: String; MinLength: Integer): Boolean;
Begin
  // same start and not more than FullOption?
  if Pos(Entry,FullOption) <> 1 then Exit(false);
  // minimum length
  if Length(Entry) < MinLength then Exit(false);
  // ok, match
  Result := True;
End;

Function Exec(Command:AnsiString;Args:Array of AnsiString;Env:Array of AnsiString;Wait:Boolean):CInt;
Var Pid     : TPid;
    CArgs   : PPChar;
    CEnv    : PPChar;
    I,J     : Integer;
Begin
  Pid := FpFork;
  if Pid < 0 then
    // error
    Exit(-fpGetErrNo)
  else if Pid = 0 then
    // we are in the child process
    Begin
      // prepare parameters
      GetMem(CArgs,(2+Length(Args))*SizeOf(PChar));
      CArgs[0] := PChar(Command);
      For I := 0 to Length(Args)-1 do
        CArgs[I+1] := PChar(Args[I]);
      CArgs[Length(Args)+1] := Nil;
      // prepare environment
      if Length(Env) > 0 then
        Begin
          I := 0;
          While EnvP[I] <> Nil do   // determine size of current environment
            Inc(I);
          GetMem(CEnv,(I+Length(Env)+1) * SizeOf(PChar));  // allocate new list
          Move(EnvP^,CEnv^,I * SizeOf(PChar));  // copy list
          For J := 0 to Length(Env)-1 do
            CEnv[I+J] := PChar(Env[J]);
          CEnv[I+Length(Env)] := Nil;
        End
      else
        CEnv := EnvP;
      // execute
      FpExecve(Command,CArgs,CEnv);  // will not return
    End
  else
    // we are still in the parent process: wait if Block = 1
    Begin
      if Wait then
        Result := WaitProcess(Pid)
      else
        Result := 0;
    End;
End;

Function SplitUsbID(IDs:String;Out idVendor,idProduct : Word) : Boolean;
Var I : Integer;
Begin
  I := Pos(':',IDs);
  if I = 0 then
    Exit(false);
  idVendor  := HexToInt(Copy(IDs,1,I-1));
  idProduct := HexToInt(Copy(IDs,I+1,Length(IDs)));
  Result := true;
End;

Function FindFileInPath(AName,APath:String) : String;
Begin
  APath := StrReplace(APath,'~',fpGetEnv('HOME'));
  { !!!TODO!!! $HOME points to "root"'s home when fork()ed before! Must find a function which reads /etc/passwd }
  APath := StrReplace(APath,'::',':');

  Result := FileSearch(AName,APath);
  if Result = '' then
    Exit;
  Result := ExpandFileName(Result);
End;

Function Select(B: Boolean; T, F: String): String;
Begin
  if B then Result := T
  else      Result := F;
End;

Function Select(I : Integer; Const S:Array of String) : String;
Begin
  if (I < 0) or (I >= Length(S)) then
    Exit('Invalid ('+IntToStr(I)+')');
  Result := S[I];
End;

Function GetUSec : UInt64;
Var TZ : TimeVal;
Begin
  fpGetTimeOfDay(@TZ,Nil);
  Result := TZ.tv_usec + TZ.tv_sec*1000000;
End;

End.
