unit StringConversionErrorsUnit;

{$mode objfpc}{$H+}{$Macro On}{$modeswitch advancedrecords}
{$ifopt D+}{$define DEBUG}{$endif}

{$ifdef DEBUG}
{$define DEBUGLVL:=1}
//{$define DEBUGLVL:=2}
//{$define DEBUGLVL:=3}
//{$define DEBUGLVL:=4}
{$else}
{$define DEBUGLVL:=0}
{$endif}

interface

uses
    Classes, SysUtils,
    {$ifdef DEBUG}
    loggerV2,
    {$endif}
    StringConverters,
    results;

type

TStringConvError_set =
(
  SCE_NoError = 0,
  SCE_Checksum_Error, //Checksum mismatch
  SCE_unknown_linetype, //Line classifier unknown
  SCE_ignored_line, //line only contains comment
  SCE_error_in_quotes, //Quotes mismatch
  SCE_empty_line,  //line has no content (zero length)
  SCE_syntax_error, //Line has an error in the syntax
  SCE_illegal_character, //in a non string part of line
  SCE_abrupt_line_termination,  //Line size is shorter than the size indicated
  SCE_bracket_mismatch, //Brackets does not match
  SCE_nesting_to_deep, //more than MAX_NESTING_DEPTH
  SCE_IOerror_Read_Error, //stream read error occured, check FileError for details
  SCE_IOerror_Write_Error, //stream write error occured, check FileError for details
  SCE_IOerror_StreamSeek_Error, //Stream seek failed with an exception (trapped and cleared)
  SCE_IOerror_StreamSize_Error, //Stream size read failed with an exception (trapped and cleared)
  SCE_IOerror_StreamCreate_read_Error,  //Could not create stream for reading
  SCE_IOerror_StreamCreate_write_Error,
  SCE_IOerror_StreamClose_Error,
  SCE_ident_Error, //Error in identifier
  SCE_length_Error, //Error in lenght section stream read
  SCE_payload_conversion_Error, //Error in converting payload from string to variant tupple
  SCE_Wrong_Type, //Not expected type in read from stream
  SCE_MemoryError, //Error in memoryhandling, check MemoryError for details
  SCE_Logic_error,  //can be used for testing of logic: Should never happen except for unhandled errors
  SCE_ArrayHeader_missing,
  SCE_ArrayHeader_WrongUserType,
  SCE_SkippedItem   //Skipped item due to checksum error -> next sync (only if sync is enabled will this logic be enabled)
);

//  map exceptions from streams to errors in SCEFE_xxxx
TStreamError_set = ( ///<<String Conversion Error File Error (SCEFE)
  SCEFE_NoError,  //No error detected
  SCEFE_EStreamError,
  SCEFE_EFCreateError,
  SCEFE_EFOpenError,
  SCEFE_EFilerError,
  SCEFE_EReadError,
  SCEFE_EWriteError,
  SCEFE_EClassNotFound,
  SCEFE_EMethodNotFound,
  SCEFE_EInvalidImage,
  SCEFE_EResNotFound,
  SCEFE_EListError,
  SCEFE_EBitsError,
  SCEFE_EStringListError,
  SCEFE_EComponentError,
  SCEFE_EParserError,
  SCEFE_EOutOfResources,
  SCEFE_EInvalidOperation,
  SCEFE_Unknown             //Catch all for unhandled exceptions
);

TMemoryError_set = ( ///<< String Conversion Error Memory Error (SCEME)
  SCEME_NoError,
  SCEME_Out_of_Memory,
  SCEME_Checksum_error,
  SCEME_MagicNumber_failure, //Magic number used for debugging pointer errors
  SCEME_error_0,
  SCEME_error_1,
  SCEME_error_2,
  SCEME_error_3,
  SCEME_error_4,
  SCEME_error_5,
  SCEME_error_6,
  SCEME_error_7,
  SCEME_error_8,
  SCEME_error_9
);

//TErrorRecommendedAction = ( //Maybe better replace with severity number on a scale
//  ERA_NoError,
//  ERA_WarnUser,
//  ERA_SaveBackup,
//  ERA_FastDumpToFileAndLogFile,
//  ERA_StopAndPutToLogFile,
//  ERA_CrashImminent
//);

//TStringConvErrorRecord = Record
//  ConvError   : TStringConvError_set;
//  FileError   : TStreamError_set;
//  MemoryError : TMemoryError_set;
//end;

TStringConvError = record
  private
    _ConvError   : TStringConvError_set;
    _FileError   : TStreamError_set;
    _MemoryError : TMemoryError_set;
    //_SCE_error : TStringConvErrorRecord;
    //_failure  : boolean;
    //_success  : boolean;
    //_warning  : boolean;
    procedure _SetConvError(Err:TStringConvError_set);
    procedure _SetMemError(Err:TMemoryError_set);
    procedure _SetStreamvError(Err:TStreamError_set);
    function _HasErrors:boolean;
    function _NoErrors:boolean;
    function ToString:UTF8String;
  public
    property HasErrors:boolean read _HasErrors;
    property NoErrors:boolean read _NoErrors;
    //property Success:boolean read _success;
    //property Warning:boolean read _warning;
    //property Failure:boolean read _failure;
    property ConvError:TStringConvError_set read _ConvError write _SetConvError;
    property FileError:TStreamError_set read _FileError write _SetStreamvError;
    property MemoryError:TMemoryError_set read _MemoryError write _SetMemError;
    property ErrorStr:UTF8String read ToString;
    procedure ClearError;
end;

implementation

{$ifdef DEBUG}
{$define PRINTLVL := PRINTLEVEL_WARNING}
{$endif}

{$Macro On}{$define INFOLINE:=PRINTLVL, {$i %file%}, {$i %CURRENTROUTINE%}}


    function TStringConvError.ToString:UTF8String;

      (*
        SCEME_NoError,
        SCEME_Out_of_Memory,
        SCEME_Checksum_error,
        SCEME_MagicNumber_failure, //Magic number used for debugging pointer errrors
        SCEME_error_0,
        SCEME_error_1,
        SCEME_error_2,
        SCEME_error_3,
        SCEME_error_4,
        SCEME_error_5,
        SCEME_error_6,
        SCEME_error_7,
        SCEME_error_8,
        SCEME_error_9
      *)
      function MemErrorToStr:UTF8String;
      begin
        Result:= 'Error Text E41';
        try
          case self.MemoryError of
            SCEME_NoError: result := '';
            SCEME_Out_of_Memory: result := 'SCEME_Out_of_Memory';
            SCEME_Checksum_error: result := 'SCEME_Checksum_error';
            SCEME_MagicNumber_failure: result := 'SCEME_MagicNumber_failure'; //Magic number used for debugging pointer errrors
            SCEME_error_0: result := 'SCEME_error_0';
            SCEME_error_1: result := 'SCEME_error_1';
            SCEME_error_2: result := 'SCEME_error_2';
            SCEME_error_3: result := 'SCEME_error_3';
            SCEME_error_4: result := 'SCEME_error_4';
            SCEME_error_5: result := 'SCEME_error_5';
            SCEME_error_6: result := 'SCEME_error_6';
            SCEME_error_7: result := 'SCEME_error_7';
            SCEME_error_8: result := 'SCEME_error_8';
            SCEME_error_9: result := 'SCEME_error_9';

          otherwise
            Result := 'Unknown memory error';
          end;
        except
          on E: Exception do
          begin
            //writeln('Exception (',e.ToString,') ');
            Result:='Exception due to error variable not initialized correctly Exception:'+ e.ToString;
            {$if DEBUGLVL>=1}
            log.LogError( 'Exception due to error variable not initialized correctly Exception:'+ e.ToString, INFOLINE , {$i %line%});
            {$endif}
          end;
        end;
        if result <> '' then result := ', '+ Result;

        //if error.MemoryError <> SCEME_NoError then
        //  result :='(M' + IntToString(integer(error.MemoryError)) + ')'
        //else
        //  result:='';
      end;

      (*
         SCEFE_NoError,  //No error detected
         SCEFE_EStreamError,
         SCEFE_EFCreateError,
         SCEFE_EFOpenError,
         SCEFE_EFilerError,
         SCEFE_EReadError,
         SCEFE_EWriteError,
         SCEFE_EClassNotFound,
         SCEFE_EMethodNotFound,
         SCEFE_EInvalidImage,
         SCEFE_EResNotFound,
         SCEFE_EListError,
         SCEFE_EBitsError,
         SCEFE_EStringListError,
         SCEFE_EComponentError,
         SCEFE_EParserError,
         SCEFE_EOutOfResources,
         SCEFE_EInvalidOperation,
         SCEFE_Unknown             //Catch all for unhandled exceptions
       *)

      function FileErrorToStr:UTF8String;
      begin
        if self.FileError <> SCEFE_NoError then
          result :=', (F' + IntToString(integer(self.FileError)) + ')'
        else
          result := '';
      end;

      function ErrorNrToStr_:UTF8String;
      begin
        Result:='(E' + IntToString(integer(self.ConvError)) + ')';
      end;

      function ErrorNrToStr:UTF8String;
      begin
        Result:=ErrorNrToStr_ + MemErrorToStr + FileErrorToStr;
      end;

      begin
        //result:= '<SCE ERROR(' + IntToString(Cardinal(error.ConvError))+')> ';
        Result:= 'Error Text E41';
        try
          case self.ConvError of
            SCE_NoError: Result := 'No error' + ErrorNrToStr;
            SCE_Checksum_Error: Result := 'Checksum mismatch' + ErrorNrToStr;
            SCE_unknown_linetype: Result := 'Line classifier unknown' + ErrorNrToStr;
            SCE_ignored_line: Result := 'line only contains comment' + ErrorNrToStr;
            SCE_error_in_quotes: Result := 'Quotes mismatch' + ErrorNrToStr;
            SCE_empty_line: Result := 'line has no content (zero length)' + ErrorNrToStr;
            SCE_syntax_error: Result := 'Line has an error in the syntax' + ErrorNrToStr;
            SCE_illegal_character: Result := 'illegal char in a non string part of line' + ErrorNrToStr;
            SCE_abrupt_line_termination: Result := 'Line size is shorter than the size indicated' + ErrorNrToStr;
            SCE_bracket_mismatch: Result := 'Brackets does not match' + ErrorNrToStr;
            SCE_nesting_to_deep: Result := 'more than "MAX_NESTING_DEPTH" in file structure' + ErrorNrToStr;
            SCE_IOerror_Read_Error: Result := 'stream read error occured' + ErrorNrToStr;
            SCE_IOerror_Write_Error: Result := 'stream write error occured' + ErrorNrToStr;
            SCE_ident_Error: Result := 'Error in identifier' + ErrorNrToStr;
            SCE_length_Error: Result := 'Error in lenght section stream read' + ErrorNrToStr;
            //SCE_CRC_Error: Result := 'Error in CRC in stream read' + ErrorNrToStr;
            SCE_payload_conversion_Error: Result := 'Error in converting payload from string to variant tupple' + ErrorNrToStr;
            SCE_Wrong_Type: Result := 'Not expected type in read from stream' + ErrorNrToStr;
            SCE_Logic_error: Result := 'can be used for testing of logic: Should never happen except for unhandled errors' + ErrorNrToStr;
            SCE_SkippedItem: Result:= 'Skipped item due to checksum error -> next sync (only if sync is enabled will this logic be enabled)' + ErrorNrToStr;

            SCE_IOerror_StreamSeek_Error: Result := 'Stream seek failed with an exception (trapped and cleared)' + ErrorNrToStr;
            SCE_IOerror_StreamSize_Error: Result := 'Stream size read failed with an exception (trapped and cleared)' + ErrorNrToStr;
            SCE_IOerror_StreamCreate_read_Error: Result := 'Could not create stream for reading' + ErrorNrToStr;
            SCE_IOerror_StreamCreate_write_Error: Result := 'Stream Create write Error' + ErrorNrToStr;
            SCE_IOerror_StreamClose_Error: Result := 'Stream Close Error' + ErrorNrToStr;
            SCE_MemoryError: Result := 'Error in memoryhandling('+ ErrorNrToStr;
            SCE_ArrayHeader_missing: Result := 'ArrayHeader missing' + ErrorNrToStr;
            SCE_ArrayHeader_WrongUserType: Result := 'ArrayHeader Wrong User Type' + ErrorNrToStr;

          otherwise
            Result := 'Unknown error' + ErrorNrToStr;
          end;
        except
          on E: Exception do
          begin
            //writeln('Exception (',e.ToString,') ');
            Result:='Exception due to error variable not initialized correctly Exception:'+ e.ToString;
            {$if DEBUGLVL>=1}
            log.LogError( 'Exception due to error variable not initialized correctly Exception:'+ e.ToString, INFOLINE , {$i %line%});
            {$endif}
          end;
        end;
      end;

	  //function TStringConvError.ToString(error:TStringConversionError ):TUTF8String;
	  //begin
		 // Result:= TStringConvError.ToString(error);
	  //end;


    function TStringConvError._NoErrors:boolean;
	  begin
	    if (self._ConvError <> SCE_NoError) Or
	       (self._FileError <> SCEFE_NoError) Or
	       (self._MemoryError <> SCEME_NoError) then
	      Result:=False
	    else
	      Result:=True;
	  end;

	  function TStringConvError._HasErrors:boolean;
	  begin
	    if self.NoErrors then exit(False) else exit(True);
	  end;

	  procedure TStringConvError._SetConvError(Err:TStringConvError_set);
	  begin
	    self._ConvError:= err;
	    self._FileError:= SCEFE_NoError;
	    self._MemoryError:= SCEME_NoError;
	    //self._Success:= False;
	    //self._Warning:= False;
	    //self._failure:= True;
	  end;

	  procedure TStringConvError._SetMemError(Err:TMemoryError_set);
	  begin
	    self._ConvError:= SCE_NoError;
	    self._FileError:= SCEFE_NoError;
	    self._MemoryError:= err;
	    //self._Success:= False;
	    //self._Warning:= False;
	    //self._failure:= True;
	  end;

	  procedure TStringConvError._SetStreamvError(Err:TStreamError_set);
	  begin
	    self._ConvError:= SCE_NoError;
	    self._FileError:= err;
	    self._MemoryError:= SCEME_NoError;
	    //self._Success:= False;
	    //self._Warning:= False;
	    //self._failure:= True;
	  end;

	  procedure TStringConvError.ClearError;
	  begin
	    self._ConvError:= SCE_NoError;
	    self._FileError:= SCEFE_NoError;
	    self._MemoryError:= SCEME_NoError;
	    //self._Success:= True;
	    //self._Warning:= False;
	    //self._failure:= False;
	  end;

end.

