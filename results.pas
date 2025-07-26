unit results; //simple partial implementation of the result pattern

{$mode objfpc}{$H+}{$Macro On}{$modeswitch advancedrecords}
{$ifopt D+}{$define DEBUG}{$endif}

interface

uses
  Classes, SysUtils;


const
  Res_failure = 0; //with an error but no result
  Res_partial_failure = 1; //with an error but also a result
  Res_warning = 2; //with result and an error as a warning
  Res_Successful = 3; // with result and no error

type

  generic TGenResult<ResultType, ErrorType> = record //class(Tobject)
    private
      _ErrorInfo:       ErrorType;  //Error info of generic ErrorType
      _Result:          ResultType; //The actual value of the result of a generic resulttype
      _ResultClass:     Uint16;
      function _Success:boolean;
  	  function _Failure:boolean;
	    function _Warning:boolean;
	    function _partial_failure:boolean;
      function _Success_or_warning:boolean;
      function _Any_failure:boolean;
//      property ResultClass:Uint16 read _ResultClass;

    Public
      procedure SetResult(Result_:ResultType); //Sets result and sets success to true
      procedure SetWarning(Result_:ResultType; Error:ErrorType); //Sets result and sets success to true
      procedure SetPartialFailure(Result_:ResultType; Error:ErrorType); //Sets result and sets success to true
      procedure SetFailure(Error:ErrorType); //Sets error value and sets success to false
      property Success:Boolean read _Success;
      property Failure:Boolean read _Failure;
      property Warning:Boolean read _Warning;
      property partial_failure:Boolean read _Partial_failure;
      property SuccessOrWarning:boolean read _success_or_warning;
      property AnyFailure:boolean read _Any_Failure;

      property Result:ResultType read _Result;
      property Error:ErrorType read _ErrorInfo;
    end;

implementation

  function TGenResult._Success:Boolean;
  begin
    if (self._ResultClass = Res_Successful) then exit(True) else exit(False);
  end;

  function TGenResult._Warning:Boolean;
  begin
    if (self._ResultClass = Res_warning) then exit(True) else exit(False);
	end;

  function TGenResult._Failure:Boolean;
  begin
    if (self._ResultClass = Res_Failure) then exit(True) else exit(False);
	end;

  function TGenResult._Partial_Failure:Boolean;
  begin
    if (self._ResultClass = Res_Partial_Failure) then exit(True) else exit(False);
	end;

  function TGenResult._Any_Failure:Boolean;
  begin
    if (self._ResultClass = Res_Partial_Failure) Or
      (self._ResultClass = Res_Failure) then exit(True) else exit(False);
	end;

  function TGenResult._Success_or_warning:Boolean;
  begin
    if (self._ResultClass = Res_Successful) Or
      (self._ResultClass = Res_Warning) then exit(True) else exit(False);
	end;

  procedure TGenResult.SetFailure(Error:ErrorType);
  begin
    self._ResultClass:= Res_failure;
    self._ErrorInfo := Error;
  end;

  procedure TGenResult.SetResult(Result_:ResultType);
  begin
    self._ResultClass:= Res_Successful;
    Self._Result := Result_;
  end;

  //Result was a success but the error contains info about the result (eg. trying to reading past the end of a file could be classified as a warning)
  procedure TGenResult.SetWarning(Result_:ResultType; Error:ErrorType );
  begin
    self._ResultClass:= res_warning;
    Self._Result := Result_;
    self._ErrorInfo := Error;
  end;

  //Result was a failure but the result contains potentially usefull data (eg. en error has occured, but there is a partial result.)
  procedure TGenResult.SetPartialFailure(Result_:ResultType; Error:ErrorType);
  begin
    self._ResultClass:= res_partial_failure;
    self._ErrorInfo := Error;
    Self._Result := Result_;
  end;

end.

(*
https://www.delphibasics.co.uk/Article.php?Name=Records

Records with variant parts:
Things get very interesting now. There are times when a fixed format record is not useful. First, we may wish to store data in the record in different ways. Second, we may want to store different types of data in a part of a record.
The Delphi TRect type illustrates the first concept. It is defined like this:

//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 type
   TRect = packed record
     case Integer of
       0: (Left, Top, Right, Bottom: Integer);
       1: (TopLeft, BottomRight: TPoint);
   end;
//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Here we have a record that holds the 4 coordinates of a rectangle. The Case clause tells Delphi to map the two following sub-sections onto the same area (the end) of the record. These variant sections must always be at the end of a record. Note also that the case statement has no end statement. This is omitted because the record finishes at the same point anyway.
The record allows us to store data in two ways:

//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 var
   rect1, rect2 : TRect;
 begin
  // Setting up using integer coordinates
   rect1.Left   := 11;
   rect1.Top    := 22;
   rect1.Right  := 33;
   rect1.Bottom := 44;

  // Seting up rect2 to have the same coordinates, but using points instead
   rect2.TopLeft     := Point(11,22);
   rect2.BottomRight := Point(33,44);
 end;
//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


The TRect record showed two methods of reading from and writing to a record. The second concept is to have two or more record sub-sections that have different formats and lengths.
This time we will define a fruit record that has a different attribute section depending on whether the fruit is round or long:

//# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
 type
  // Declare a fruit record using case to choose the
  // diameter of a round fruit, or length and height ohterwise.
   TFruit = Record
     name : string[20];
     Case isRound : Boolean of// Choose how to map the next section
       True  :
         (diameter : Single); // Maps to same storage as length
       False :
         (length   : Single;  // Maps to same storage as diameter
          width    : Single);
   end;

 var
   apple, banana : TFruit;

 begin
  // Set up the apple as round, with appropriate dimensions
   apple.name     := 'Apple';
   apple.isRound  := True;
   apple.diameter := 3.2;

  // Set up the banana as long, with appropriate dimensions
   banana.name    := 'Banana';
   banana.isRound := False;
   banana.length  := 7.65;
   banana.width   := 1.3;

  // Let us display the fruit dimensions:
   if apple.isRound
   then ShowMessageFmt('Apple diameter = %f',[apple.diameter])
   else ShowMessageFmt('Apple width = %f , length = %f',
                       [apple.width, apple.length]);
   if banana.isRound
   then ShowMessageFmt('Banana diameter = %f',[banana.diameter])
   else ShowMessageFmt('Banana width = %f , length = %f',
                       [banana.width, banana.length]);
 end;
 //# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

 Apple diameter = 3.2
 Banana width = 3.20 , length = 7.65

	Note that the Case statement now defines a variable, isRound to hold the type of the variant section. This is very useful, and recommended in variable length subsections, as seen in the code above.
*)

