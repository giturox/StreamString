program StreamsTester;
{$mode objfpc}
/////{$h+}

{$ifopt D+}{$define DEBUG}{$endif}
{$Macro On}
{$define INFOLINE:= {$i %file%},{$i %CURRENTROUTINE%} , {$i %line%}}

uses
   streamex, Classes, SysUtils, RtlConsts, fileutils, tagslist, StringStreamer,
   Variants, TextStreaming,
   TreeHandler,
   loggerV2, StringConversionErrorsUnit, stringconverters;

{$define FirstErrorStop := 1}

type
  TMyObject = class
  public
    Name: UnicodeString; //each char requires two bytes(chars) -> vt_TwoCharString
//    UniCodeName: WideString;
    number: uint64;
    points: Double;
    currentdate:TDateTime;
    StreamHandler: TStreamStringHandler;

    constructor Create;
    Destructor Destroy; override;

    procedure SaveToStream(AStream: TStream);
    function LoadFromStream(AStream: TStream):boolean;
  end;

//procedure TMyObject.SaveToStream(AStream: TStream);
//var
//  Len: UInt32;
//begin
//  Len := Length(Name);
//  AStream.Write(Len, SizeOf(Len));
//  AStream.Write(PChar(Name)^, Len);
//
//  Astream.WriteDWordBE(number);
//  Astream.WriteDouble(points);
//end;
//
//procedure TMyObject.LoadFromStream(AStream: TStream);
//var
//  Len: UInt32;
//
//begin
//  AStream.Read(Len, SizeOf(Len));
//  SetString(Name, PChar(nil), Len);
//  AStream.Read(PChar(Name)^, Len);
//  number:=Astream.readDWordBE;
//  points:=Astream.ReadDouble;
//end;

constructor TMyObject.Create;
begin
  self.StreamHandler := TStreamStringHandler.Create;
end;

Destructor TMyObject.destroy;
begin
  self.StreamHandler.Destroy;
end;

procedure TMyObject.SaveToStream(AStream: TStream);
var
  //Len: UInt32;
  //value:Variant;
  error: TStringConversionError;
begin
  try
    error:= SCE_ClearError;
    write('- w 0 -');
    StreamHandler.WriteVariantToStream_ErrorFallThrough(astream, self.Name, error);
    write('- w a -');

    StreamHandler.WriteVariantToStream_ErrorFallThrough(astream, self.number, error);
    write('- w b -');
    StreamHandler.WriteVariantToStream_ErrorFallThrough(astream, self.points, error);
    write('- w c -');
    if SCE_noerrors(error) then
    begin
      writeln('No error in write to stream');
    end
    else
    begin
      writeln('Error in write to stream ' + StringConversionErrorToString(error));
      log.LogError('Error in write to stream :' + SCE_ToString(Error), INFOLINE);

    end;

  except
    log.LogError('Save failed (exception)','MyObject.LoadFromStream');
  end;


end;

function TMyObject.LoadFromStream(AStream: TStream):boolean;
var
  //Len   : UInt32;
  //vt    : TVariantTupple;
  error : TStringConversionError;

{$ifdef DEBUG}
  {$endif}
begin
  try
    error := SCE_ClearError;
    Result := False;
    writeln('Load start');
  //  Log.LogStatus('Load start',{$i %file%},{$i %CURRENTROUTINE%} +', '+ {$i %line%});
    Log.LogStatus('Load start',INFOLINE);
    self.Name  := StreamHandler.ReadVariantFromSTream_ErrorFallThrough(AStream, vt_TwoCharString, error);
    if SCE_noerrors(error) then write('Name=', self.Name) else write('Error "', SCE_ToString(error),'" in name. ');
    self.number:= StreamHandler.ReadVariantFromSTream_ErrorFallThrough(AStream, vt_uint64, error);
    if SCE_noerrors(error) then write('Number=', self.number) else write('Error "', SCE_ToString(error),'" in number. ');
    self.points:= StreamHandler.ReadVariantFromSTream_ErrorFallThrough(AStream, vt_double, error);
    if SCE_noerrors(error) then write('Points=', self.points) else write('Error "', SCE_ToString(error),'" in points. ');;
    if SCE_noerrors(error) then
    begin
      writeln('No error in read from stream');
      writeln('Read result(',result,')  : ->   Name =', self.Name, ' number=', self.number,' Points=', self.points);
      Result:=True;
    end
    else
    begin
      Result := False;
      writeln('Error in read from stream "' + StringConversionErrorToString(error),'"');
      log.LogError('Load failed','MyObject.LoadFromStream');
    end;
    FreeAndNil(error)
  except
    log.LogError('Load failed (exception)','MyObject.LoadFromStream');
  end;
end;

{
fmCreate // TFileStream.Create creates a new file if needed.
fmOpenRead // TFileStream.Create opens a file with read-only access.
fmOpenReadWrite // TFileStream.Create opens a file with read-write access.
fmOpenWrite // TFileStream.Create opens a file with write-only access.
}
var
  oktest:boolean;
  wstream, rstream:TFileStream;
  myobj: TMyObject;
  bstack:TBracketStack;
  streamstr:TOneCharString;
  vartest:TVariantTupple;
  SingleVar:Single;
  DoubleVar:Double;
  //longwordvar:Uint32;
  //signedlongwordvar:int32;
  //quadvar:Uint64;
  //signedquadvar:int64;
  //strvar:string;
  //error:boolean;
  error_SCE:TStringConversionError;

const
  NOISEFACTOR = 0.05;


  function RandomizedNoiseInString( src:TOneCharString; MinIdx:Integer; MaxIdx:integer; var ok:boolean):TOneCharString;
  var
    len, idx , randomizations :integer;
  begin
    Result := src;
    if not ok then exit(result);
    if MinIdx >= length(src) then
    begin
      ok := false;
      writeln('Logik error:  MinIdx < length(src)  ->  Error');
      exit(result);
    end;
    len := length(src);
    if MaxIdx > len then MaxIdx:= len;
    repeat
      randomizations := 0;
      for idx:=MinIdx to MaxIdx do //do not randomize sync marker (or it will never find sync -> no error)
      begin
        if random < NOISEFACTOR then
        begin
          randomizations += 1;
          Result[idx] := chr( (ord(result[idx]) + Random(127)) AND 255);
        end;
      end;
    until randomizations > 0;
    writeln('randomizations = ', randomizations);
  end;

//Language tests:
//korean '로마는 하루아침에 이루어진 것이 아니다' (romaneun haruachime irueojin geosi anida) rome wasn't built in a morning (day)
//Ukranian 'Рим був побудований не за один день'
//Greek 'Η Ρώμη δεν χτίστηκε σε μια μέρα'
//Thai 'กรุงโรมไม่ได้สร้างเสร็จในวันเดียว'
//Chinese (traditional) '羅馬不是一天建成的'
//Sinhalese 'රෝමය එක දවසින් හදපු එකක් නෙවෙයි'

//Rewrite to use new stream functions!!!
  function test_part1(ok:boolean):Boolean;
    var filename:String;
  begin
    {$ifdef FirstErrorStop}
    if not(ok) then exit(False);
    {$endif}
    ok := False;
    filename:='Test.txt';
    try
      myobj:=TMyObject.Create;
      myobj.Name:='Æ er å a ø i a å (It/I is/am on the island in the river).';
      myobj.number:=42;
      myobj.points:=23.2;
      wstream:=TFileStream.Create(filename, fmCreate);
      myobj.SaveToStream(wstream);
      writeln('Write OK');
      ok := True;
    finally
      wstream.Destroy;
    end;

    {$ifdef FirstErrorStop}
    if not(ok) then exit(False);
    {$endif}

    myobj.name :='';
    myobj.number:=0;
    myobj.points:=0;

    try
      ok := False;
      rstream:=TFileStream.Create(filename, fmOpenRead);
      ok := myobj.LoadFromStream(rstream);
      if ok then
        writeln('read ok. Name=', myobj.Name, ' number=', myobj.number, ' points=',myobj.points)
      else
      begin

        writeln('Read not ok');

        ok:=False;
      end;
    finally
      myobj.Destroy;
      rstream.Destroy;
    end;
    Result:=ok;
  end;


  function test_part2(ok:boolean):Boolean;
  /// Test of variant functions and bracketstack
  var
      strunicodeVar:unicodestring;
      sh:TStreamStringHandler;
  begin
    {$ifdef FirstErrorStop}
    if not(ok) then exit (False);
    {$endif}
    try
      sh := TStreamStringHandler.Create;

      Result:= False;

      singlevar := 11.11;
      vartest.value := SingleVar;
      streamstr:=sh.VariantToStreamString(vartest.value, error_SCE);
      writeln('streamstr =',streamstr);
      vartest:= sh.StreamStringToVariantTupple(Streamstr,error_SCE);
      singlevar:=vartest.value;
      writeln('vartest =',vartest.value, ' gives singlevar=', SingleVar);

      DoubleVar := 22.22;
      vartest.value := DoubleVar;
      streamstr:=sh.VariantToStreamString(vartest.value,error_SCE);
      writeln('streamstr = "',streamstr,'"');
      vartest:= sh.StreamStringToVariantTupple(Streamstr,error_SCE);
      DoubleVar :=vartest.value;
      writeln('vartest =',vartest.value, ' gives doublevar=', DoubleVar );

      //Wide string test
      writeln('------------------');
      writeln('Widestring test');
      strunicodeVar:='Påskeæg er søde öôí€ ▒♥אש捑';
      vartest.value := strunicodeVar;
      streamstr:=sh.VariantToStreamString(strunicodeVar, error_SCE);
      writeln('streamstr =',streamstr);
      vartest:= sh.StreamStringToVariantTupple(Streamstr,error_SCE);
      strunicodeVar:='Påskeæg er søde öôí€ ▒♥אש捑';
      writeln('hexdump source=',StringDumpToHex(strunicodeVar));
      writeln('hexdump result=',StringDumpToHex(vartest.value));
      //strWidevar :=vartest.value;
      writeln('vartest =',vartest.value, ' gives strunicodeVar=', strunicodeVar );
      if (strunicodeVar = vartest.value) then
      begin
        writeln('*** Strings match ***');
        Result:=True;
      end
      else
      begin
        writeln('*** Strings do *NOT* match ***');
        exit(false);
      end;
      writeln('------------------');

      writeln('------------------');
      writeln('emptystring test');
      strunicodeVar:='';
      vartest.value := strunicodeVar;
      streamstr:=sh.VariantToStreamString(strunicodeVar, error_SCE);
      writeln('streamstr =',streamstr);
      vartest:= sh.StreamStringToVariantTupple(Streamstr,error_SCE);
      strunicodeVar :=vartest.value;
      writeln('vartest =',vartest.value, ' gives strunicodeVar=', strunicodeVar );
      writeln('------------------');

      //strvar:= WidestringToRawString(strWidevar,error_SCE);
      //writeln('strvar =',strvar, ' from strwidevar=', strWidevar );
      //strwidevar:= RawstringToWideString(strvar,error_SCE);
      //writeln('strvar =',strvar, ' to strwidevar=', strWidevar );

      //streamstr:=VariantToStreamString(RawstringToWideString(strvar,error_SCE), error_SCE);
      //writeln('streamstr =',streamstr);
      //vartest:= StreamStringToVariant(Streamstr,error_SCE);
      //strWidevar :=vartest;
      //writeln('vartest =',vartest, ' gives strwidevar=', strWidevar );

    finally
      sh.Destroy;
    end;

    Result:=False;
    try
      bstack:=TBracketStack.Create(100);
      bstack.bracket_Stack_tester;
      Result:=True;
    finally
      bstack.Destroy;
    end;

  end;

  function test_part3(ok:boolean):Boolean;
  /// Test sync marker and checksum (enabled and disabled)
  var
    //teststr:UTF8String;
//    SSP: TStreamStringParser;
    SSP: TStreamStringHandler;
    State: TStreamstringParserState;
    randomizations, idx :integer;
    TestVarArray : TVariantArray;
    SingleValue:Single;
    DoubleValue:Double;
//    TimeNow:TDateTime;
    strunicodeVar, strunicodeVarCmp : UnicodeString;
    StreamStr1, StreamStr2,StreamStr3: UTF8String;
    Ch: UTF8Char;
    //sh:TStreamStringHandler;
    TagTest:TTagData;

//function ParseStreamString():TvariantArray;

  begin
    {$ifdef FirstErrorStop}
    if not ok then exit(False);
    {$endif}
    try
      SSP:=TStreamStringHandler.Create;
      SingleValue := 12.34;
      DoubleValue := 12.0;
      ssp.DisableChecksum;
      ssp.DisableSync;
      streamstr1:=SSP.VariantToStreamString(SingleValue, error_SCE);
      writeln('Streamstr1: "', StreamStr1,'"');

      ssp.ResetState;
      for idx:=1 to length(StreamStr1) do
      begin
        state:=ssp.UpdateStateMachine(Streamstr1[idx]);
        write('<Char ="', Streamstr1[idx] ,'" State =', state,'> ');
      end;
      writeln;

      if not SCE_NoErrors(ssp.Error) then
      begin
        writeln('Conversion error =', SCE_ToString(ssp.Error));
        SSP.Destroy;
        exit(False);
      end;

      write ('Payload =', ssp.TagData.Value,', ');
      if not ApproxEqual(ssp.TagData.Value, SingleValue) then
      begin
        writeln('Does *not* match source');
        SSP.Destroy;
        exit(False);
      end;
      writeln('Approximately matches source');

      writeln('________     ________     ________     ________     ________     ________     ');
      writeln('__    __)   (__    __)   (__    __)   (__    __)   (__    __)   (__    __)   (');
      writeln('__)  (_________)  (_________)  (_________)  (_________)  (_________)  (_______');
      Writeln('Double test');
      writeln('____     ________     ________     ________     ________     ________     ____');
      writeln('  __)   (__    __)   (__    __)   (__    __)   (__    __)   (__    __)   (__  ');
      writeln(' (_________)  (_________)  (_________)  (_________)  (_________)  (_________) ');

      DoubleValue := 12.34567891234;
//      streamstr1:= ECS_ESCAPECHAR + ECS_SYNCCHAR + ssp.VariantToStreamString(DoubleValue, error_SCE);
      ssp.ActivateChecksum;
      ssp.ActivateSync;
      streamstr1:= ssp.VariantToStreamString(DoubleValue, error_SCE);

      ssp.ResetState;
      for idx:=1 to length(StreamStr1) do
      begin
        state:=ssp.UpdateStateMachine(Streamstr1[idx]);
        write('<Char="', Streamstr1[idx] ,'" State=', state,'> ');
      end;
      writeln;

      if SCE_HasErrors(ssp.Error) then
      begin
        writeln('Conversion error =', SCE_ToString(ssp.Error));
        SSP.Destroy;
        exit(False);
      end;

      write ('Payload =', ssp.TagData.Value,'   ');
      if not ApproxEqual(ssp.TagData.Value, DoubleValue) then
      begin
        writeln('Does *not* match source');
        SSP.Destroy;
        exit(False);
      end;
      writeln('Approximately matches source');

      writeln('Sync marker test #############################################');
      //check sync marker
      ssp.ActivateChecksum;
      ssp.ActivateSync;
      randomize;
      streamstr1:= ssp.VariantToStreamString(DoubleValue, error_SCE);
      streamstr2:= '';
      for idx:=1 to 10000 do
      begin
        streamstr2 := streamstr2 + chr(Random(255));
      end;
      streamstr3 := streamstr2 + streamstr1;

      writeln('Length of Streamstring with prepended noise = ',length(StreamStr3));
      ssp.ResetState;
      for idx:=1 to length(StreamStr3) do
      begin
//        write(Streamstr3[idx]);
        state:=ssp.UpdateStateMachine(Streamstr3[idx]);
        if ssp.HasError then
        begin
          write('Sync error -> resets state - ');
          ssp.ResetState;
          ssp.SetSceError(SCE_NoError);
        end;
        //write('<Char=', Streamstr1[idx] ,'State =', state,'> ');
      end;
      writeln;

      if ssp.HasError then
      begin
        writeln('Conversion error =', SCE_ToString(ssp.Error));
        SSP.Destroy;
        exit(False);
      end;

      write ('Payload = ', ssp.TagData.Value,', ');
      if not ApproxEqual(ssp.TagData.Value, DoubleValue) then
      begin
        writeln('Does *not* match source');
        SSP.Destroy;
        exit(False);
      end;
      writeln('Approximately matches source');

      Randomize;
      //check detection of noise in chars
      //the test is done with a Double float to also check that the system intercepts exceptions and continues.
      //In debugging mode, exceptions triggers the debugger, but the program should be able to continue and correctly report the conversion error.
      writeln('Noise detection test #############################################');

      ssp.ActivateChecksum;
      ssp.ActivateSync;
      streamstr1:= ssp.VariantToStreamString(DoubleValue, error_SCE);
      streamstr2 := RandomizedNoiseInString(StreamStr1, 3, length(StreamStr1), ok);
      if not ok then exit(false);

      ssp.ResetState;
      idx:=1;
      //for idx:=1 to length(StreamStr2) do
      while ((idx < 1000) And (not (ssp.IsDone))) do
      begin
        if idx < length(StreamStr2) then
          ch:=Streamstr2[idx]
        else
          ch:=chr (random(255));
        state := ssp.UpdateStateMachine(ch);
        write('<Char="', Streamstr1[idx] ,'" State =', state,'> ');
        idx += 1;
      end;
      writeln;

      if (randomizations > 0) then
      begin
        if ssp.IsDone then
        begin
          writeln('Completed parsing');
        end
        else
        begin
          writeln('***InComplete parsing');
        end;

        if SCE_NoErrors(ssp.Error)   then
        begin
          writeln('Error =', SCE_ToString(ssp.Error));

          if StreamStr1 = StreamStr2 then
            writeln('No difference in strings')
          else
          begin
            Writeln('No errors detected despite randomizations! (that is an error!)');
            writeln('StreamStr1 (original): "', StreamStr1,'"');
            writeln('StreamStr2 (Modified): "', StreamStr2,'"');
          end;
          ssp.Destroy;
          exit(false);
        end
        else
        begin
          writeln('Error correctly detected:');
          writeln('Error =', SCE_ToString(ssp.Error));
        end;
      end
      else
      begin
        Writeln('Error in randomization string test!');
        ssp.Destroy;
        exit(false);
      end;

      writeln('---------------< Sequential Stream tests part II >------------------');
      writeln('-------------------< This time it is personal >---------------------');
      ssp.ResetState;
//      ssp.ActivateChecksum;
//      ssp.ActivateSync;
      ssp.DisableChecksum; //To improve payload efficiency, the idea is to take a variant array
      ssp.DisableSync;

      strunicodeVar:='Påskeæg er søde "▒♥אש捑".StrEnd';
      streamstr2 := 'Nebudkanezars Excellent Space Chariot And The Space Broccoli Incident.'; //NESCATBI / N'SCATBI
      Setlength(TestVarArray, 4);
      TestVarArray[0] := DoubleValue;
      TestVarArray[1] := streamstr2;
      TestVarArray[2] := Now;
      TestVarArray[3] := StrUniCodeVar;
      StreamStr1 := '';
      for idx:=0 to High(TestVarArray) do
      begin
        write('V idx=', idx);
        streamstr1 += ssp.VariantToStreamString(TestVarArray[idx], error_SCE);
      end;

      writeln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
      writeln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
      for idx:=1 to length(StreamStr1) do
      begin
        state:=ssp.UpdateStateMachine(Streamstr1[idx]);
        if ssp.IsDone then
        begin
          writeln('Payload=', ssp.tagdata.Value);
          if vartype(ssp.tagdata.Value)=varOleStr then
            strunicodeVarCmp:=ssp.tagdata.Value;
          ssp.ResetState;
          writeln('/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\');
        end;
      end;
      writeln;
      if strunicodeVarCmp = strunicodeVar then writeln('Payload of unicodestr transferred correctly.');
      writeln;

      SSP.Destroy;
    except
      ok:=False;
    end;

    Result:= ok;
  end;

function test_part4(ok:boolean):Boolean;

type
  PRecForTest = ^TRecForTest;
  TRecForTest = Record
     name: string;
     FavoriteColour: string;
     NumberOfPencils: Uint64;
  end;

var
  SSP: TStreamStringHandler;
  State: TStreamstringParserState;
  randomizations, idx :integer;
  TestVarArray : TVariantArray;
  SingleValue:Single;
  DoubleValue:Double;
  warning:boolean;
  strunicodeVar, strunicodeVarCmp : UnicodeString;
  StreamStr1, StreamStr2,StreamStr3, StreamStr4: UTF8String;
  Ch: UTF8Char;
  TagTest1, TagTest2, TagTest3, TagTest4, TagTest5, TagTest6 :TTagData;

  TagArray_receiver: TTagObjArray;
  TagArray: TTagObjArray; //Stores tags. Incremented in steps of 20% of the current size (minimum increase is though 20 items), Array index 0 value contains the number of used elements in the array.
  FoundItem:Boolean;
  ItemArray:Array of TTagData;
  TestRec:TRecForTest;

  procedure ReturnFalse(ErrorMsg:String; filename, FunctionName,  lineNr:String);
  begin
    writeln('***Error:',ErrorMsg);
    log.LogError(ErrorMsg, INFOLINE);
    ssp.Destroy;
  end;



begin
  {$ifdef FirstErrorStop}
  if not(ok) then exit (False);
  {$endif}
  try
    SSP:=TStreamStringHandler.Create;
    writeln('###########################################################');
    writeln('--- TagTest ---');
    writeln('###########################################################');
    TagTest1.TagAbstractType:= Uint8(TT_Item);
    TagTest1.TagsId:=12345;
    TagTest1.TagsUserType:=$01;
    TagTest1.Value:='Cogito ergo est sum';
    ssp.ActivateChecksum;
    ssp.ActivateSync;
    Streamstr1 := ssp.TagToStreamString(TagTest1,error_SCE);

    writeln('  -Step 1-');
    ssp.ResetState;
    for idx:=1 to length(StreamStr1) do
    begin
      state:=ssp.UpdateStateMachine(Streamstr1[idx]);
      if ssp.IsDone then
      begin
        //if ((ssp.TagData.TagsUserType <> TagTest1.TagsUserType) Or
        //    (ssp.TagData.TagsId <> TagTest1.TagsId ) Or
        //    (ssp.TagData.TagAbstractType <> TagTest1.TagAbstractType ) Or
        //    (ssp.TagData.Value <> TagTest.value )) then
        if SSP.TagData <> TagTest1 then
        begin

          Writeln('*** Error in Tag transfer test! ***');
          if (ssp.TagData.Value <> TagTest1.value ) then
          begin
            writeln('Send. TagPayload=', TagTest1.Value);
            writeln('Recv. TagPayload=', ssp.TagData.Value);
          end;
          if (ssp.TagData.TagsId <> TagTest1.TagsId ) then
          begin
            writeln('Send. TagId=', TagTest1.TagsId);
            writeln('Recv. TagId=', ssp.TagData.TagsId);
          end;
          if (ssp.TagData.TagAbstractType <> TagTest1.TagAbstractType ) then
          begin
            writeln('Send. TagAbstractType=', TagTest1.TagAbstractType);
            writeln('Recv. TagAbstractType=', ssp.TagData.TagAbstractType);
          end;
          if (ssp.TagData.TagsUserType <> TagTest1.TagsUserType) then
          begin
            writeln('Send. TagUserType=', TagTest1.TagsUserType);
            writeln('Recv. TagUserType=', ssp.TagData.TagsUserType);
          end;
          ssp.Destroy;
          exit(false);
        end;
        Writeln('= Tagged data received OK =');
        writeln('TagPayload=', ssp.TagData.Value);
        writeln('TagId=', ssp.TagData.TagsId);
        writeln('TagAbstractType=', ssp.TagData.TagAbstractType);
        writeln('TagUserType=', ssp.TagData.TagsUserType);
        writeln('/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\');
      end;
    end;
    if not ssp.IsDone then
    begin
      Writeln('Parse error in tagtest1');
      ReturnFalse('Parse error in tagtest1', INFOLINE);
      exit(false);
    end
    else
    begin
      ssp.ResetState;
    end;

    writeln('  -Step 2-');

    //splitting function
    ssp.ActivateChecksum;
    ssp.ActivateSync;
    TagTest1.TagAbstractType:= Uint8(TT_Item);
    TagTest1.TagsId:=12345;
    TagTest1.TagsUserType:=$01;
    TagTest1.Value:='Påskeæg er søde "▒♥אש捑". Cogito ergo est sum.';

    Streamstr1 := ssp.TagToStreamString(TagTest1,error_SCE);
    //StreamStr1 := ssp.GlobalItemStr($0001, 123456789, 'Påskeæg er søde "▒♥אש捑". Cogito ergo est sum.',error_SCE);
    TagTest2.TagAbstractType:= Uint8( TT_RecordHeader);
    TagTest2.TagsId:=26781;
    TagTest2.TagsUserType:=$FA;
    TagTest2.Value:=46.47;
    Streamstr2 := ssp.TagToStreamString(TagTest2,error_SCE);
    //StreamStr2 := ssp.GlobalItemStr($FACE, 56781234, 46.47 ,error_SCE);
    StreamStr3 := StreamStr1 + StreamStr2;
    FoundItem:= False;
//    function ParseString(Source:TOneCharString;Var Value:TTagData; var FoundItem:boolean; Var ErrorCode:TStringConversionError):ToneCharString;
//    function ParseString(Source:TOneCharString;Var Value:Variant; var FoundItem:boolean; Var ErrorCode:TStringConversionError):ToneCharString;
    StreamStr4 := ssp.ParseString(StreamStr3, TagTest4, FoundItem, error_SCE);

    if not foundItem then
    begin
      //writeln('***Error: First item not found.');
      //log.LogError('ParseString test fail: nothing found', INFOLINE);
      ReturnFalse('ParseString test fail: nothing found', INFOLINE);
      exit(False);
    end;

    if not (TagTest4 = TagTest1) then
    begin
      //writeln('Mismatch on Tag1 vs received!');
      //log.LogError('ParseString test fail: Tag1 mismatch', INFOLINE);
      ReturnFalse('ParseString test fail: Tag1 mismatch', INFOLINE);
      exit(false);
    end;
//    Tagtest is a string, but tagdata is OleStr -> error!!!
    writeln('tag1.value=', ssp.TagData.Value);

    StreamStr3 := ssp.ParseString(StreamStr4, TagTest4, FoundItem, error_SCE);
    if not foundItem then
    begin
      //writeln('***Error: Second item not found.');
      //log.LogError('ParseString test fail: Second item not found.', INFOLINE);
      ReturnFalse('ParseString test fail: Second item not found.', INFOLINE);
      exit(false);
    end;

    if not (TagTest4 = TagTest2) then
    begin
      //writeln('Mismatch on tag2 vs received data! ');
      //log.LogError('ParseString test fail: Tag2 mismatch.', INFOLINE);
      //ssp.Destroy;
      //exit(False);
      ReturnFalse('ParseString test fail: Tag2 mismatch.', INFOLINE);
      exit(false);
    end;

    writeln('Match on both tagged items in stream string. ');
    log.LogStatus('ParseString test 1 success', INFOLINE);

    writeln('  -Step 3-');

    ////Test writefunctions
    //TestRec.name:='Lucy';
    //TestRec.FavoriteColour:='Red';
    //TestRec.NumberOfPencils:=24;
    //writeln(ssp.StartRecordStr($0001, ssp.GetNextID, 'RecTest',Pointer(@TestRec), error_SCE));
    //
    //writeln('  -Step 4-');

    ssp.ResetIdCounter;
    ssp.ResetState;
    ssp.ActivateChecksum;
    ssp.ActivateSync;
    ssp.Destroy;
  except
    ok:=False;
  end;

Result:= ok;

end;


function test_part5(ok:boolean):Boolean;
//Test Tag Array to str functions
var
  DatArray:TTagObjArray;
  TagArr: TTagArray;
  idx:integer;
  HeaderTag:TTagData;
  TagInit : TTagData;

  errors:TStringConversionError;

  StreamStr:TOneCharString;

  procedure ReturnFalse(ErrorMsg:String; filename, FunctionName,  lineNr:String);
  begin
    writeln('***Error:',ErrorMsg);
    log.LogError(ErrorMsg, INFOLINE);
    Datarray.Destroy;
  end;

const
  ArrayIdxMax=4;
begin
  {$ifdef FirstErrorStop}
  if not(ok) then exit (False);
  {$endif}
  try
    Datarray:=TTagObjArray.Create;
    writeln('DatArray created.');
    setlength(TagArr,ArrayIdxMax+2);
    TagInit.TagAbstractType:=Uint8(TT_Item);
    TagInit.TagsId:=70000;
    TagInit.TagsUserType:=257;
    TagInit.value := -1;
    for idx := 0 to ArrayIdxMax do
    begin
      TagInit.TagsId:=idx+70000;
      TagArr[idx]:=TagInit;
    end;
    TagArr[0].Value:='Cogito ergo est sum';
    TagArr[1].Value:='Røde påskeæg ÆØÅ';
    TagArr[2].Value:=14.2344;
    TagArr[3].Value:=Uint32(1444446);
    TagArr[4].Value:='Black adder goes forth';
//    writeln('Streamstr="', Streamstr,'"' );

    try
      errors := SCE_ClearError;
      for idx := 0 to ArrayIdxMax do
      begin
        write('<Tries to add item "',TagArr[idx].Value,'">');
        DatArray.AddItem((TagArr[idx]));
        writeln(' (Did an add item?)');
      end;
      StreamStr:=DatArray.ArrayToStreamStr(0,20);
      writeln('Streamstr="', Streamstr,'"' );
      if DatArray.ClearArray then write ('(ClearArray: succeeded)') else write('(ClearArray: failed!)');
      StreamStr:=DatArray.StreamStrToArray(StreamStr, HeaderTag); // DatArray.StreamStrToArray(StreamStr);

      if DatArray.ItemsInArray > 0 then
      begin
        for idx := 0 to DatArray.ItemsInArray-1 do
        begin
          Writeln('Data[', idx,'].value="', DatArray.DataArray[idx].Value.value,'"');
        end;
      end
      else
      begin
        ReturnFalse('Did not find any items in string!', INFOLINE);
        exit(false);
      end;
    finally
      writeln('Finalizes Try section');
      DatArray.destroy;
      exit(True);
    end;
  except
    exit(false);
  end;
end;


//----------------------------------------------------------------
//----------------------------------------------------------------
//----------------------------------------------------------------

  type
//    TTestStruct = TTagData;
    TPlanetStruct = record
      PlanetName:TOneCharString;
      AvgDistanceFromSunAU: f64;
      MassRelativeToEarth: f64;
      HasAtmosphere: boolean;
      GasGiant:boolean;
      NumberOfMoons: UInt32;
    end;

  const
    TypeID_PlanetName = 1;
    TypeID_AvgDistanceFromSunAU = 2;
    TypeID_MassRelativeToEarth = 3;
    TypeID_HasAtmosphere = 4;
    TypeID_GasGiant = 5;
    TypeID_NumberOfMoons = 6;

  const
    PlanetStructDefault : TPlanetStruct =
                          (PlanetName: '<noname>';
                           AvgDistanceFromSunAU: 1.0;
                           MassRelativeToEarth: 1.0;
                           HasAtmosphere: False;
                           GasGiant: False;
                           NumberOfMoons: 0);

    //update code to store and retrieve Record data from a StreamStringArray (TTagData. Need to be able to find tags in the array to meatch record fields with correct items in array
  type
    TPlanetStructObj         = class( TArrayObject )
    private
    public
      value:TPlanetStruct;
      constructor create; override;
      Function ToStreamString(StreamStrHandler:TStreamStringHandler; var Errors:TStringConversionError):TonecharString; override;
      function FromStreamString (StreamStrHandler:TStreamStringHandler; data:TonecharString;
                                 var FoundItem:boolean; Errors:TStringConversionError):Tonecharstring; override;
      procedure SetDefault; override;
      function ToText:ToneCharstring; override;
    end;

    TPlanetStructArray = specialize TGenArray<TPlanetStructObj, TPlanetStruct>;

  Constructor TPlanetStructObj.Create;
  begin
    //writeln('TTestStructObj: Create');
    Self.SetDefault;
  end;

  procedure TPlanetStructObj.SetDefault;
  begin
    //self.value.PlanetName:='EarthLike';
    //self.value.AvgDistanceFromSunAU:=1.0;
    //self.value.MassRelativeToEarth:=1.0;
    //self.value.HasAtmosphere:= True;
    //self.value.GasGiant:=False;
    //self.value.NumberOfMoons:=2;
    self.value := PlanetStructDefault;
  end;

  Function TPlanetStructObj.ToStreamString(StreamStrHandler:TStreamStringHandler; var Errors:TStringConversionError):TonecharString;
  var TagArray: TTagObjArray;
      TaggedItem: TTagData;
      //errors:TStringConversionError;
  begin
    try
      TagArray := TTagObjArray.create;

      try
        //Shared parts of the tags for items in the record
        //TaggedItem.Index:=0;
        TaggedItem.TagAbstractType:=TT_Item;
        TaggedItem.TagsUserType:=$0; //8 bit values -> failure

        //Now all items are tagged and put into the array
        TaggedItem.TagsId:=TypeID_PlanetName;
        TaggedItem.Value:=self.value.PlanetName;
        TagArray.AddItem(TaggedItem);

        TaggedItem.TagsId:=TypeID_AvgDistanceFromSunAU;
        TaggedItem.Value:=self.value.AvgDistanceFromSunAU;
        TagArray.AddItem(TaggedItem);

        TaggedItem.TagsId:=TypeID_MassRelativeToEarth;
        TaggedItem.Value:=self.value.MassRelativeToEarth;
        TagArray.AddItem(TaggedItem);

        TaggedItem.TagsId:=TypeID_HasAtmosphere;
        TaggedItem.Value:=self.value.HasAtmosphere;
        TagArray.AddItem(TaggedItem);

        TaggedItem.TagsId:=TypeID_GasGiant;
        TaggedItem.Value:=self.value.GasGiant;
        TagArray.AddItem(TaggedItem);

        TaggedItem.TagsId:=TypeID_NumberOfMoons;
        TaggedItem.Value:=self.value.NumberOfMoons;
        TagArray.AddItem(TaggedItem);

        Result := TagArray.ArrayToStreamStr(TaggedItem.TagsUserType, $0000);

        ////Store the string result in a tag
        //TaggedItem.TagAbstractType:= TT_Item;
        //TaggedItem.TagsId:=$00000000;
        //TaggedItem.TagsUserType:=$0000;
        //TaggedItem.Value:= Result;
        //Result:= StreamStrHandler.TagToStreamString(TaggedItem, errors);
      finally
        try
           TagArray.destroy;
        except
          on E: Exception do
          begin
            Result:='Exception - Array destructor fail for single record to streamstr :'+ e.ToString;
            StreamStrHandler.SetSceError(SCE_MemoryError);
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        //writeln('Exception (',e.ToString,') ');
        Result:='Exception - Array constructor fail for single record to streamstr:'+ e.ToString;
        StreamStrHandler.SetSceError(SCE_MemoryError);
      end;
    end;

  end;

  function TPlanetStructObj.FromStreamString (StreamStrHandler:TStreamStringHandler; data:TonecharString;
                             var FoundItem:boolean; Errors:TStringConversionError):Tonecharstring;
  var TagArray: TTagObjArray;
    TaggedItem: TTagData;
    HeaderTag: TTagData;
    idx:integer;
    Warning:Boolean;
  begin
    Warning := False;
    Result:= data;
//    writeln('TPlanetStructObj: FromStreamString(', data,')');
    if data = '' then
    begin
      StreamStrHandler.SetSceError(SCE_empty_line);
      log.LogError('Error in converting from string to struct' + SCE_ToString(StreamStrHandler.Error), INFOLINE);
      exit(Result);
    end;
    try
      TagArray := TTagObjArray.create;
      try
        self.SetDefault; //Initialize to default values (forward / backward compatibility)
        result:=TagArray.StreamStrToArray(result,HeaderTag );
        if SCE_HasErrors(Tagarray.error) then
        begin
          if TagArray.error.ConvError = SCE_SkippedItem then
          begin
            warning := True;
            Tagarray.ClearError;
          end
          else
          begin
            StreamStrHandler.SetSceError(TagArray.error);
            log.LogError('Error in converting from string to struct' + SCE_ToString(StreamStrHandler.Error), INFOLINE);
            exit(result);
          end;
        end;
        for idx:= 0 to TagArray.ItemsInArray-1 do
        begin
          case Tagarray.DataArray[idx].value.TagsId of
            TypeID_PlanetName:
                self.value.PlanetName := Tagarray.DataArray[idx].value.Value;
            TypeID_AvgDistanceFromSunAU:
                self.value.AvgDistanceFromSunAU := Tagarray.DataArray[idx].value.Value;
            TypeID_MassRelativeToEarth:
                self.value.MassRelativeToEarth := Tagarray.DataArray[idx].value.Value;
            TypeID_HasAtmosphere:
                self.value.HasAtmosphere := Tagarray.DataArray[idx].value.Value;
            TypeID_GasGiant:
                self.value.GasGiant := Tagarray.DataArray[idx].value.Value;
            TypeID_NumberOfMoons:
                self.value.NumberOfMoons := Tagarray.DataArray[idx].value.Value;
            otherwise
              writeln('Unknown tagid(',Tagarray.DataArray[idx].value.TagsId,') -> ignored tagitem');
          end;
        end;
      finally
        try
           TagArray.destroy;
        except
          on E: Exception do
          begin
//            writeln('Error in array destructor');
            StreamStrHandler.SetSceError(SCE_MemoryError);
            log.LogError('Exception - Array destructor fail for single record from streamstr :'+ e.ToString, INFOLINE);
          end;
        end;
        if Warning then StreamStrHandler.SetSceError(SCE_SkippedItem);
      end;
    except
      on E: Exception do
      begin
        //writeln('Exception (',e.ToString,') ');
//        writeln('Error in array constructor');
        StreamStrHandler.SetSceError(SCE_MemoryError);
        log.LogError('Exception - Array constructor fail for single record from streamstr :'+ e.ToString, INFOLINE);
      end;
    end;
    //writeln('Parsed result:');
    //writeln(self.ToText);
  end;

  function TPlanetStructObj.ToText:ToneCharstring;
  begin
    result:= '<TPlanetStructObj( PlanetName="' + VarToStr(self.value.PlanetName)  +'"'+
             ', AvgDistanceFromSunAU=' + FloatToString(self.value.AvgDistanceFromSunAU,5,2) +
             ', MassRelativeToEarth='+ FloatToString(self.value.MassRelativeToEarth,6,1) +
             ', HasAtmosphere=' + BoolToStr(self.value.HasAtmosphere,'Yes','No') +
             ', GasGiant=' + BoolToStr(self.value.GasGiant,'Yes','No') +
             ', NumberOfMoons=' + IntToString(self.value.NumberOfMoons) +
             ')>';
  end;

function test_part7(ok:boolean):Boolean;
//Test Tag Array to str functions
//From https://nssdc.gsfc.nasa.gov/planetary/factsheet/planet_table_ratio.html  27/5/23
//and https://en.wikipedia.org/wiki/List_of_natural_satellites  27/5/23
const
  Mercury : TPlanetStruct =(PlanetName: 'Mercury'; AvgDistanceFromSunAU: 0.387;
                       MassRelativeToEarth: 0.0553; HasAtmosphere: False;
                       GasGiant: False; NumberOfMoons: 0);
  Venus : TPlanetStruct =(PlanetName: 'Venus'; AvgDistanceFromSunAU: 0.723;
                       MassRelativeToEarth: 0.815; HasAtmosphere: False;
                       GasGiant: False; NumberOfMoons: 0);
  Earth : TPlanetStruct =(PlanetName: 'Terra'; AvgDistanceFromSunAU: 1.0;
                       MassRelativeToEarth: 1.0; HasAtmosphere: False;
                       GasGiant: False; NumberOfMoons: 1);
  Mars : TPlanetStruct =(PlanetName: 'Mars'; AvgDistanceFromSunAU: 1.52;
                       MassRelativeToEarth: 0.107; HasAtmosphere: False;
                       GasGiant: False; NumberOfMoons: 2);
  Jupiter : TPlanetStruct =(PlanetName: 'Jupiter'; AvgDistanceFromSunAU: 5.20;
                       MassRelativeToEarth: 317.8; HasAtmosphere: True;
                       GasGiant: True; NumberOfMoons: 95);
  Saturn : TPlanetStruct =(PlanetName: 'Saturn'; AvgDistanceFromSunAU: 9.57;
                       MassRelativeToEarth: 95.2; HasAtmosphere: True;
                       GasGiant: True; NumberOfMoons: 146);
  Uranus : TPlanetStruct =(PlanetName: 'Uranus'; AvgDistanceFromSunAU: 19.17;
                       MassRelativeToEarth: 14.5; HasAtmosphere: True;
                       GasGiant: True; NumberOfMoons: 27);
  Neptune : TPlanetStruct =(PlanetName: 'Neptune'; AvgDistanceFromSunAU: 30.18;
                       MassRelativeToEarth: 17.1; HasAtmosphere: True;
                       GasGiant: True; NumberOfMoons: 14);
  //Important miniplanet :-)
  Pluto : TPlanetStruct =(PlanetName: 'Pluto'; AvgDistanceFromSunAU: 39.48;
                       MassRelativeToEarth: 0.0022; HasAtmosphere: False;
                       GasGiant: False; NumberOfMoons: 5);

var
  DatArray:TPlanetStructArray;
  CompareArray : TPlanetStructArray;
  //make a version of test for TTestStructObjArray
  //VarArr: TVariantArray;
  idx:integer;

  HeaderTag:TTagData;
  TagInit : TTagData;
  errors:TStringConversionError;
  okay : boolean;
  StreamStr, StreamStrCopy,
  StringSrc, StringDst, leftover:TOneCharString;

  TempFile: TextFile;
  FileName: TOneCharString;
  IoFailure:boolean;

  procedure ReturnFalse(ErrorMsg:String; filename, FunctionName,  lineNr:String);
  begin
    writeln('***Error:',ErrorMsg);
    log.LogError(ErrorMsg, INFOLINE);
    Datarray.Destroy;
  end;

  function CompareRecord( val1, val2: TPlanetStruct): boolean;
  begin
    Result:= True;
    if val1.PlanetName <> val2.PlanetName then exit (false);
    write('Planetname match, ');
    //Floats/doubles may get rounding errors when converted for transport
    if not ApproxEqual(val1.AvgDistanceFromSunAU, val2.AvgDistanceFromSunAU) then exit (false);
    write('AvgDistanceFromSunAU match, ');
    if not ApproxEqual(val1.MassRelativeToEarth, val2.MassRelativeToEarth) then exit (false);
    write('MassRelativeToEarth match, ');
    if val1.HasAtmosphere <> val2.HasAtmosphere then exit (false);
    write('HasAtmosphere match, ');
    if val1.GasGiant <> val2.GasGiant then exit (false);
    write('GasGiant match, ');
    if val1.NumberOfMoons <> val2.NumberOfMoons then exit (false);
    write('NumberOfMoons match');
  end;

  procedure release;
  begin
    try
      try
        DatArray.ClearArray; // write ('*DatArrays.ClearArray*>> ');
      finally
        try
          DatArray.destroy;
          write ('*DatArray.destroy* ');
        except
          on E: Exception do
          begin
            writeln('DatArray destroy caused Exception:', E.ToString);
          end;
        end;
      end;
    finally
      try
        CompareArray.ClearArray; // write ('*CompareArray.ClearArray*>> ');
        CompareArray.destroy; //write ('*CompareArray.destroy* ');
      except
        on E: Exception do
        begin
          writeln('CompareArray destroy caused Exception:', E.ToString);
        end;
      end;
    end;
  end;

  procedure WriteHeapStatus;
  var  heapstatus: TFPCHeapStatus;
  begin
    heapstatus := GetFPCHeapStatus;
    writeln('***-------------------------------------------------------------------------***');
    writeln('    MaxHeapSize = ', heapstatus.MaxHeapSize, ' MaxHeapUsed = ', heapstatus.MaxHeapUsed);
    writeln('    CurrHeapSize = ', heapstatus.CurrHeapSize, ' CurrHeapFree = ', heapstatus.CurrHeapFree, ' CurrHeapUsed = ', heapstatus.CurrHeapUsed);
    writeln('***-------------------------------------------------------------------------***');
  end;

  function CompareOk:boolean;
  var idx:integer;

  begin
    Result:=True;
    //Compare the array of records with the base array values
    for idx:=0 to DatArray.ItemsInArray -1 do
    begin
      Write('PlanetName ="', DatArray.DataArray[idx].value.PlanetName,'"');
      if CompareRecord( DatArray.DataArray[idx].value,
            CompareArray.DataArray[idx].value) then
        writeln(' -> ok')
      else
      begin //compare failed release arrays (destructor releases individual items automatically)
        writeln('-> compare failure!');
        release;
        exit(false);
      end;
    end;
  end;

var
  Compare_is_ok:boolean;

begin
  {$ifdef FirstErrorStop}
  if not(ok) then exit (False);
  {$endif}
  Compare_is_ok:=False;
  try
    Datarray:=TPlanetStructArray.Create;
    CompareArray:=TPlanetStructArray.Create;
    writeln('DatArray/CompareArray created.');

    //Add items to array to send and compare array
    DatArray.AddItem(Mercury);	CompareArray.AddItem(Mercury);
    DatArray.AddItem(Venus);	CompareArray.AddItem(Venus);
    DatArray.AddItem(Earth);	CompareArray.AddItem(Earth);
    DatArray.AddItem(Mars);	CompareArray.AddItem(Mars);
    DatArray.AddItem(Jupiter);	CompareArray.AddItem(Jupiter);
    DatArray.AddItem(Saturn);	CompareArray.AddItem(Saturn);
    DatArray.AddItem(Uranus);	CompareArray.AddItem(Uranus);
    DatArray.AddItem(Neptune);	CompareArray.AddItem(Neptune);
    DatArray.AddItem(Pluto);	CompareArray.AddItem(Pluto);

    WriteHeapStatus;
    //Convert array to string
    StreamStr := DatArray.ArrayToStreamStr(0,0);
    if SCE_HasErrors(DatArray.error) then
    begin
      log.LogError( 'Could not convert from array to string :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln;
    writeln(' ___________');
    writeln('/           \_______________________________________________');
    writeln('StreamResult="', StreamStr,'"');
    writeln('\__________________________________________________________/');

    WriteHeapStatus;

    FileName:='test_str.txt';
    assign (TempFile, FileName);
    {$i-}
    rewrite(TempFile);
    if IOResult <> 0 then begin Writeln('Error: Could not open file for write.'); release; exit(false); end;

    writeln(tempfile, StreamStr); //Need to force \cr & \nl -> escapechars if found in string
    if IOResult <> 0 then begin Writeln('Error: Could not write array stream string to file'); release; exit(false); end;
    CloseFile(TempFile);
    if IOResult <> 0 then begin Writeln('Error: Could not close already open file.'); release; exit(false); end;

    IoFailure:=False;
    Assign (TempFile, FileName);
    Reset (TempFile);
    if IOResult <> 0 then begin Writeln('Error: Could not open file for read.'); release; exit(false); end;

    readln(tempfile, StreamStrCopy );
    if IOResult <> 0 then begin Writeln('Error: Could not read from file.'); IOFailure:=True; end;

    CloseFile(tempfile);
    if (IOResult <> 0) Or IOFailure then
      begin Writeln('Error: Could not close already open file.'); release; exit(false); end;
    {$i+}

    if StreamStr <> StreamStrCopy then
    begin
      Writeln('String read does *not* match string written.');
      release;
      exit(false);
    end
    else
    begin
      Writeln('String read matches string written.');
    end;

    //Clear array and convert from string to array
    Datarray.ClearArray;
    WriteHeapStatus;

    HeaderTag.TagsId:=0;
    HeaderTag.TagsUserType:=0;
    HeaderTag.TagAbstractType:=TT_Undefined;
    headertag.Value:=UInt16( 0);

    DatArray.StreamStrToArray(StreamStrCopy, HeaderTag);
    if SCE_HasErrors(DatArray.error) then
    begin
      log.LogError( 'Could not convert from string to array :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;

    writeln;
    writeln(' ___________');
    writeln('/           \_________________________________');
    writeln('StreamResult="', StreamStr,'"');
    Compare_is_ok:=CompareOk;

    writeln('Compare OK? =', Compare_is_ok);
    writeln('----------------------------------------------');

    if not Compare_is_ok then
    begin
      writeln('Release in progress');
      release; exit(false);
    end;

    //release;
    //exit(True);
    //Stops here

    writeln('###----------------------###');
    writeln('### StreamTester to file ###');
    writeln('###----------------------###');
    //Testing array to and from filestream
    //Resuse of DatArray
    if not DatArray.OpenReWrite('DatStream.txt') then
    begin
      log.LogError( 'Could not open array stream :'+SCE_ToString(DatArray.error) , INFOLINE);
      writeln('Open failed :'+SCE_ToString(DatArray.error));
      release; exit(false);
    end;
    writeln('Open OK');
    StreamStr := DatArray.ArrayToStreamStr(0,0);
    if SCE_HasErrors(DatArray.error) then
    begin
      log.LogError( 'Could not convert from array to file :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln('Array to stream OK');

    if not DatArray.CloseFile then
    begin
      log.LogError( 'Could not close array stream :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln('Stream close OK');

    DatArray.ClearArray;

    if not DatArray.OpenRead('DatStream.txt') then
    begin
      log.LogError( 'Could not open array stream :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln('*** Open for read =');
    StreamStr := DatArray.StreamStrToArray('',HeaderTag);
    if SCE_HasErrors(DatArray.error) then
    begin
      log.LogError( 'Could not convert from array to file :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln('Stream to array OK');
    if not DatArray.CloseFile then
    begin
      log.LogError( 'Could not close array stream :'+SCE_ToString(DatArray.error) , INFOLINE);
      release; exit(false);
    end;
    writeln('Stream close OK');

    if not CompareOk then exit(false);
    writeln('Compare OK');

  except
    writeln('Could not create test array in test 7');
    exit(False);
  end;
  WriteHeapStatus;

  release;
end;


  //----------------------------------------------------------------
  //----------------------------------------------------------------
  //----------------------------------------------------------------

function test_part6(ok:boolean):Boolean;
//Test Tag Array to str functions
var
  DatArray:TVariantObjArray;
  VarArr: TVariantArray;
  idx:integer;
  HeaderTag:TTagData;
  TagInit : TTagData;
  errors:TStringConversionError;
  okay : boolean;
  StreamStr, StringSrc, StringDst, leftover:TOneCharString;

  procedure ReturnFalse(ErrorMsg:String; filename, FunctionName,  lineNr:String);
  begin
    writeln('***Error:',ErrorMsg);
    log.LogError(ErrorMsg, INFOLINE);
    Datarray.Destroy;
  end;

begin
  {$ifdef FirstErrorStop}
  if not(ok) then exit (False);
  {$endif}
  try
    Datarray:=TVariantObjArray.Create;
    setlength(VarArr,6);
    VarArr[0]:='Cogito ergo est sum';
    VarArr[1]:='Røde påskeæg ÆØÅ';
    VarArr[2]:=14.2344;
    VarArr[3]:=Uint32(1444446);
    VarArr[4]:='Black adder goes forth';
    VarArr[5]:='Factor 5 games can be good';

    try
      errors := SCE_ClearError;
      for idx := 0 to 5 do
      begin
        DatArray.AddItem((VarArr[idx]));
      end;
      DatArray.OpenReWrite('bison.txt');
      StreamStr:=DatArray.ArrayToStreamStr(0,20);
      DatArray.CloseFile;

      DatArray.OpenRead('bison.txt');
      if SCE_HasErrors(DatArray.error) then writeln('...... funky error........');
      leftover := DatArray.StreamStrToArray(StreamStr, HeaderTag);
      DatArray.CloseFile;

      writeln('Array Read from stream ok...');
      StreamStr:=DatArray.ArrayToStreamStr(0,20);

      writeln('Streamstr="', Streamstr,'"' );

      StringSrc := StreamStr; //Since StreamStrToArray is used destructively, we need a copy to test error handling

      DatArray.ClearArray;
      DatArray.ClearArray;

      writeln('Arrays cleared');
      errors := SCE_ClearError;
      for idx := 0 to 5 do
      begin
        DatArray.AddItem((VarArr[idx]));
      end;
      DatArray.ClearArray;


      writeln('Here CD');

//      Streaming from streams works ok, but streaming from a string is not working

      leftover := DatArray.StreamStrToArray(StreamStr, HeaderTag);

      writeln('Here D');
      if DatArray.HasError then writeln('Errors: ', DatArray.ErrorToString);
      if DatArray.ItemsInArray > 0 then
      begin
        for idx := 0 to DatArray.ItemsInArray-1 do
        begin
          Writeln('Data[', idx,'].value="', DatArray.DataArray[idx].value,'"');
        end;
      end
      else
      begin
        ReturnFalse('Did not find any items in string!', INFOLINE);
        exit(false);
      end;

      if DatArray.HasError then begin
        ReturnFalse('Previous operation ended with errors', INFOLINE);
        exit(false);
      end;

      DatArray.ClearArray;
      writeln('Here E - after clear array');

      okay := True;

      if false then
      begin
        writeln('********************************************************');
        writeln('********************************************************');
        writeln('Pre noise string:');
        writeln(StringSrc);
        StringDst := RandomizedNoiseInString(StringSrc,50, 150,  okay); //Add noise -> should give errors when it is tested later
        writeln('String with noise (',idx,'):');
        writeln(StringDst);
        if StringDst = StringSrc then writeln('Strings identical ????');
        writeln('********************************************************');
        writeln('********************************************************');

        leftover := DatArray.StreamStrToArray(StringDst, HeaderTag);
        writeln('Leftover = "', leftover,'" and error=', DatArray.ErrorToString);
        if DatArray.NoError then ReturnFalse('Did not detect errors in mangled string during tests!', INFOLINE); // There should be an error

        if DatArray.ItemsInArray > 0 then
        begin
          for idx := 0 to DatArray.ItemsInArray-1 do
          begin
            Writeln('Data[', idx,'].value="', DatArray.DataArray[idx].value,'"');
          end;
          if DatArray.ItemsInArray <3 then
          begin
            writeln('Found less than 3 items -> error');
            okay:=False;
          end;
        end
        else
        begin
          //ReturnFalse('Did not find any items in string!', INFOLINE);
          writeln('Did not find any items in string!');
          okay:=False;
        end;
      end;

      //Add random char(s) to string to check error detection and handling
    finally
      DatArray.destroy;
    end;
  except
    exit(false);
  end;
  Result:=Okay;
end;

begin

  error_SCE :=SCE_ClearError;
  oktest:= True;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 0');
    writeln('- test UTF escape functions');
    writeln('=====================================================================');
    oktest:= test_UTF_escape_functions(oktest);
  end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 1');
    writeln('- Simple variant -> string & string -> variant streamtests ');
    writeln('=====================================================================');
    oktest:= test_part1(oktest);
  end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 2');
    writeln('- Test of variant functions and bracketstack');
    writeln('=====================================================================');
    oktest:= test_part2(oktest);
  end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 3');
    writeln('- Test sync marker and checksum (enabled and disabled)');
    writeln('=====================================================================');
    oktest:= test_part3(oktest);
  end;


  ////if oktest then
  ////begin
  ////writeln;
  ////writeln('=====================================================================');
  ////writeln('Test 4');
  ////writeln('- Array functions on tags & variants + operators (maybe depreceate?)');
  ////writeln('=====================================================================');
  ////oktest:= test_part4(oktest);
  ////end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 5');
    writeln('- Generic functions (TgenArray) - TTagType version');
    writeln('=====================================================================');
    oktest:= test_part5(oktest);
  end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 6');
    writeln('- Generic functions (TgenArray) - variant version');
    writeln('=====================================================================');
    oktest:= test_part6(oktest);
  end;

  if oktest then
  begin
    writeln;
    writeln('=====================================================================');
    writeln('Test 7');
    writeln('- Generic functions (TgenArray) - custem record version');
    writeln('=====================================================================');
    oktest:= test_part7(oktest);
  end;


  //if oktest then
  //begin
  //writeln;
  //writeln('=====================================================================');
  //writeln('Test Tree');
  //writeln('- Treehandler tests');
  //writeln('=====================================================================');
  //oktest := test_tree(oktest);
  //end;

  //--------------------------------------------------------------
  //check test funktionerne. Der mangler sammenligninger af strenge der har været gennem strenge/streams.
  //--------------------------------------------------------------
  writeln('  _____________');
  writeln(' / Result:     \_____________________________________________________ ');
  writeln('|                                                                    |');
  if oktest then
  begin
    writeln('| OK - All Tests are successful                                      |')
  end
  else
  begin
    writeln('| *FAILED* - At least one Test was NOT successful                    |');
  end;
  writeln('|                                                                    |');
  writeln('|                                                                    |');
  writeln('\____________________________________________________________________/');
  readln;
  FreeAndNil(error_SCE);
end.

