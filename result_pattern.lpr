program result_pattern;

{$mode objfpc}{$H+}{$Macro On}
{$modeswitch advancedrecords}
{$ifopt D+}{$define DEBUG}{$endif}

uses results, Math;

type
  TSimpleErrType = uint32;

  TResDouble = specialize TGenResult<Double, TSimpleErrType>;

  TComplexErrType = Record
    WheelCode: uint32;
    IsItBlue:boolean;
  end;

  THamsterRec = record
    NumberOfHamsters: uint64;
    HamstersWithMansions: uint64;
  end;
  TResHamster = specialize TGenResult<THamsterRec, TComplexErrType>;

//mandatory reference to the Hitchhikers guide to the Galaxy by Douglas Adams
function DeepThought:TResDouble;
begin
  if Random(100) > 50 then
    Result.SetResult(42.0) //Do you really understand the question?
  else
    Result.SetError(random(100));

  exit(Result);
end;

function DeeperThought:TResHamster;
var
  HamsterData: THamsterRec;
  ErrorData: TComplexErrType;
begin
  if Random(100) > 50 then
  begin
    HamsterData.NumberOfHamsters:= Random(10000);
    //Very complicated math that defies mortal comprehension
    HamsterData.HamstersWithMansions:=math.Floor( float(HamsterData.NumberOfHamsters) * 0.1 + 0.5); // ~10 % has mansions
    Result.SetResult(HamsterData);
  end
  else
  begin
    ErrorData.WheelCode:=random(100);
    if random(2) >= 1 then ErrorData.IsItBlue:=True else ErrorData.IsItBlue:=False;
    Result.SetError(ErrorData);
  end;

  exit(Result);
end;

var
  DoubleResult :TResDouble;
  HamsterResult : TResHamster;
  idx:int64;

begin
  Writeln('trying few times random deep/deeper thought :-)');

  Randomize;
  for idx:=1 to 10 do
  begin

    DoubleResult := DeepThought; //Generate either double precision float or an error
    if DoubleResult.success then
      Writeln('The meaning of life, the Universe and Everything =',DoubleResult.Result:3:0)
    else
      Writeln('Double trouble! Error code was (', DoubleResult.Error,')'); //Does this mean life has no meaning, but is only a shadow in the form of an error (attributed to LowGradeSokrates in the masterpiece "The philosophers FAQ of frisbees")

    HamsterResult := DeeperThought; //Generate either record or error
    if HamsterResult.Success then
      writeln('Nr. of hamsters=', HamsterResult.Result.NumberOfHamsters,
              ' of which ', HamsterResult.Result.HamstersWithMansions,' has mansions')
    else
      Write('Hamster crisis! ');
      if HamsterResult.Error.IsItBlue then write('Blue ') else write ('Maybe red/green ');
      writeln('Error code (', HamsterResult.Error.WheelCode,')');
  end;
  readln;
end.

