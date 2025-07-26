//******************************************************************************
{*
* @file fileutils.pas
* @author \$author
* @version \$Id$
* @date \$Date$
* @brief Filehandling with backwards and forwards compatibility
* Purpose: To enable compatibility between versions of the program, files are
* stored using tags so that the different attributes does not have a fixed
* postion in a file. Every field in a file has a tag, that marks what the data
* field immediately after contains. Before we read a certain structure type, it
* is filled with standard values that are overwritten as we read the individual
* tags. That way we may maintain forwards and backwards compatibility, since an
* unknown tag should just be ignored. (Like we also ignore unknown record types.)
* @remarks
*}
//******************************************************************************

unit fileutils; {$mode objfpc}
(***********************************************************************
short: Filehandling
Purpose: To enable compatibility between versions of the program, files are
 stored using tags so that the different attributes does not have a fixed
 postion in a file. Every field in a file has a tag, that marks what the data
 field immediately after contains. Before we read a certain structure type, it
 is filled with standard values that are overwritten as we read the individual
 tags. That way we may maintain forwards and backwards compatibility, since an
 unknown tag should just be ignored. (Like we also ignore unknown record types.)


Changes:
    12.10.01: initial version
    19.11.22: Major revision
    25.11.22: Split from TextCiv


***********************************************************************)
{$H+}
{@TODO: (In progress): Parsing of tags/records with types both user and abstract}
{@TODO: Parsing of data lines with no tags}
{@TODO: (In progress): Convert special reader to general purpuse structured fil reader/saver}
{@TODO: (In progress): Make tag library with variable arrays for strings and values : <tag>, <string> , <value.double>, <value.quadword>}
{@TODO: handle arrays of simple types}
{@TODO: handle arrays of records }
{@TODO:  handle intrafile pointers (special tags)}

{$ifopt D+}{$define DEBUG}{$endif} //Define Debug if debug option is enabled (to make the code more readable)

interface
{$ifdef FPUNONE}
{$Error 'Needs FPU in current version'}
{$endif}


uses
  classes, sysutils, math, variants, fpwidestring, dateutils, Dialogs,
  crc, tagslist, StringConverter;

const

  ExpectTagLine = true;
  ExpectNonTagLine = false;

  comment_char = ';';
  Quotes_Char = '"';
  escape_char = '\';
  illegal_bracket_char = #0;
  no_bracket_char = #1;

//                            '0123456789ABCDEF'; //we encode hex values as A-P characters to easily distingiush between numbers and hex numbers without prefix
//  FastHexChars_signed     = 'abcdefghijklmnop'; //lower case fasthex numbers are for signed numbers
//  FastHexChars_unsigned   = 'ABCDEFGHIJKLMNOP'; //Upper case fasthex numbers are for unsigned numbers
//The first char in the hex number determines the wether the number is signed or unsigned. Any subsequent nubmers should maintain case for consistency reasons. Do not assume that the case will remain ignored
  Fasthex_signed_start ='a';
  Fasthex_signed_end ='p';
  Fasthex_unsigned_start ='A';
  Fasthex_unsigned_end ='P';



    MAX_NESTING_DEPTH = 32000; //max bracket depth

    DOS_ERROR_SYNTAX = -1;




type

  TLineCodeError = (lce_no_error,
                    lce_unknown_linetype,
                    lce_ignored_line,
                    lce_error_in_quotes,
                    lce_malformed_number,
                    lce_empty_line,
                    lce_syntax_error,
                    lce_illegal_character,
                    lce_abrupt_line_termination,
                    lce_bracket_mismatch,
                    lce_nesting_to_deep); //more than MAX_NESTING_DEPTH
  Tlinetype = (lt_rec, //record start
               lt_lst, //list of records and or tags start
               lt_tag, //tag line
               lt_cmd, //Command for parser (unused and ingored for now)
               lt_rnd, //record end
               lt_lnd, //list end
               lt_dat, //Data line
               lt_unk, //unknown type (for unknown future linetypes)
               lt_ign, //ignored type (for planned future linetypes (e.g. links))
               lt_err); ///<rec=record, lst=list of records, tag=tagged value line, cmt=comment,rnd=record end, lnd= list end, unk=unknown

               TBracket = ( no_bracket, //This line has no bracket (no tree structure of its own)
               bracket_stack_range_error, //
               bracket_type_error,
               bracket_mismatch_error,
               start_group, //Starts a group (structured set of data)
               end_group,   //ends a group of structured data (also means that we now go up in the tree structure again for later items
               start_parenthesis, //Currently unused (skip until end_parenthesis for now)
               end_parenthesis,   //currently unused (resume normal operation if detected )
               start_array,       //Currently not fully developed, but is supposed to be for 2d arrays made lt_dat lines (e.g. for images)
               end_array );       //ends an array
  TValueType = ( vt_unknown,   //Do not know type (future version)
                 vt_undefined, //Valuetype is not defined
                 vt_string,    //Value is a string (size specification should be written, but is currently ignored on read (assumes type safe language
                 vt_float,     //Value is a float as either single (4 bytes) or double (8 bytes)
                 vt_integer,   //value is an integer of specified size
                 vt_root,      //value is not a value but name of root record "(record="<name>","<recordtype>","<abstract_type>"
                 vt_array,     //value is not a value but name of 2d array "{list="<name>","<listtype>","<abstract_type>"
                 vt_linkref,   //Reference to a defined link (both forward and backward (currently ignored, but paritally parsed)
                 vt_link);     //Defines a link that can be referenced to. NAmespace is global (currently ignored, but paritally parsed)
  TTagValueReaderErrors = ( tv_unknown, //Is unknown to this reader (file from future version), line is well formed
               tv_undefined, //The value type is undefined
               tv_error_mismatched_quotes, //The quotes for an item are not matching
               tv_error_command_and_no_value, //there is a dangling comma (at or near line end)
               tv_tag_type_mangled); //Line is completely unreadable


  TSectionError = (se_Section_syntax_is_ok,
                   se_Section_quotation_error,
                   se_Section_has_number_error,
                   se_Section_type_is_unknown,
                   se_parser_did_not_find,
                   se_parsing_beyond_string);
  Tsection = record
               value:Variant;
               section_type:TValueType;
               section_is_a_string:boolean;
               section_is_a_number:boolean;
               section_error:TSectionError;
             end;

  TSectionbuffer = record
                   length_of_buffer:Integer;
                   errorcode:integer;
                   Found_Equal_Sign:Boolean;
                   Found_tags:Boolean;
                   line_Error_Code:TLineCodeError;
                   buffer:array of Tsection;
                 end;


  TTagValueAndType = record
    Section_value : Tsection;
    tag_type_abstract : Tsection;
    tag_type_datasize :integer; //in bytes; valid options are: 1,2,4,8,16,32 numbers are in decimalformat or hexformat with prefix $
  end;



  TFileLineParsed=
  record
    raw_str:String; //Raw input
    FirstChar:Char;
    LineType:Tlinetype; //Parsed linetype
    bracket:TBracket; //If there is a bracket, then what type
    identifier:string; //For tags what is the identifier label
    value_str:string; //What is the value
    value_type_ident:string;
    value_type_abstract:TValueType;
    value:double;
    value_int:Int128Rec;
    error_code:TLineCodeError;
    tag_value_error : TTagValueReaderErrors;
  end;

  TLineTypePreparse = Record
    LineType:Tlinetype;
    remainder:string;
  end;

  //TSettings = record
  //  hues, gains:array [0..5] of Single;
  //  boost, exposure, mixrate :single;
  //  usehuemap, useHueEqualizer, scale2x, newBrightness,
  //  staggered, randomDither, noDither:Boolean;
  //end;

  TStructuredObjectError = (SOE_no_Error, SOE_Checksum_error, SOE_write_error, SOE_read_error, SOE_structural_error);
  TstructuredObjectState = (SOS_idle, SOS_writing_to_file, SOS_reading_from_file, SOS_error, SOS_tags_ready, SOS_prepare_to_write);



  PStructuredObject  = ^TStructuredObject;
  TStructuredObjectPointer = class(Tobject)
    private
    public
      id:string;
      ptr:PStructuredObject;
      constructor Create(name:string; newptr:PStructuredObject);
//      Destructor  Destroy; override;
  End;

  TStructuredObject  = class(TObject)
    private
      valid, initalized, checksum_ok:boolean;
      CurrentState:TstructuredObjectState;
      parent, child, prev, next:TStructuredObjectPointer;
      current_Tag:integer;
      Current_Error:TStructuredObjectError;
      TagList:TTagsList; //Temporary storage of tags as the are received..
      checksum:UInt64;

    public
      constructor Create;
      Destructor  Destroy; override;
      Function    convert_tags_to_objectdata:TStructuredObjectError; //virtual;
      Function    convert_objectdata_to_tags:TStructuredObjectError; //virtual;
      Function    Prepare_tags_array(number_of_tags:integer):TStructuredObjectError;
      procedure   Free_tags_array;

  end;

  TFileReader = class
    private
      { Private declarations }
    public
      { Public declarations }
      IdTags:TTagsList;
      HasReadFile:Boolean;
      constructor Create;
      Destructor  Destroy; override;

      //Handlers:
      function remove_comment(indline:string):string;
      //function remove_whitespace(indline:string):string;
      function parseline(inputline:string; var name:string; var id:integer):TLinetype;
      function WriteTagsUnit(tagsunitfilename:string):boolean;
      function ReadTagsFile(tagsfilename:string):boolean;
      procedure test_unit;
  end;


  //may contain a memory leak!
  TBracketStack = class
    private
      { Private declarations }
    public
      { Public declarations }
      bracket_Stack:array of variant; //To ensure brackets are type matched we stack them
      bracket_Stack_pointer:integer; //Stack grows in the positive direction
      constructor Create(size:integer);
      Destructor  Destroy;
      procedure reset_bracket_stack;
      procedure init_bracket_stack(size:integer);
      function push_bracket_char(char_to_push:char ):Char;
      function pop_bracket_char:Char;
      function pop_bracket:TBracket;
      function push_bracket(bracket_to_push:TBracket ):TBracket;
      function bracket_Stack_tester:Boolean;
//      function bracket_Stack_tester:Boolean;
  end;

  TFileOfText = record
    SimpleFileReader:TFileReader;
    filename:string;
    errorcode:integer; //IO error code FPC numbers
    errorstring:string; //IO error code -> english text
    FileHandle:TextFile;
    level_depth:integer; //Non ignored depth (used to identify tree structure)
    ignore_depth:integer; //ignored depth (used to to ensure we do not track unknown structures)
    lineno:integer; //Current lineno (used to inform where parser is in case of an error
    bracketStack:TBracketStack;
//    bracket_Stack:array of char; //To ensure brackets are type matched we stack them
//    bracket_Stack_pointer:integer; //Stack grows in the positive direction
  end;


  function Bracket_Char_To_Bracket_Type(firstchar:char):TBracket;
  function Bracket_type_To_Bracket_char(bracket:Tbracket):char;
  function bracket_is_a_startbracket(bracket:TBracket):Boolean;
  function bracket_is_an_endbracket(bracket:TBracket):Boolean;
  function BracketMatch(bracket1, bracket2:TBracket):boolean;


//  function IntToString(value:LongInt):String;
//  function FloatToString(value:double; totaldigits, decimals:integer):String;
  function write_list_end(var fileToWrite:TFileOfText):string;
  function write_list_item(var fileToWrite:TFileOfText; tagname:string; item:real):string;
  function write_list_start(var fileToWrite:TFileOfText; listname:string; numitems:integer):string;
  function write_tag(var fileToWrite:TFileOfText; tagname:string; tagvalue:string; IsAString:boolean ):string;
  function write_record_end(var fileToWrite:TFileOfText):string;
  function write_record_start(var fileToWrite:TFileOfText; recordname:string):string;
  function write_text_with_indent(var fileToWrite:TFileOfText; textstring:string; indentchange:integer; postchange:boolean):string;
  function write_indent(var fileToWrite:TFileOfText; TabDepth:integer):string;
  function IoresultToText(var fileToWrite:TFileOfText):string;
  function ErrorCodeToText(errorcode:integer):string;
  //procedure TestReadFile;
  function LineCodeToErrorString(linecode_error:TLineCodeError):string;
  function DetectLineType(textline:string):TFileLineParsed;

  function OpenStructuredTextFile(filename:String):TFileOfText;
  function read_and_parse_line(var fil:Tfileoftext):TfileLineParsed;
  function CloseStructuredTextFile(var fil:TFileOfText):integer;
  function GetIoResult(var fil:TFileOfText):integer;
  procedure Test_parse_functions;
  //Function UnitTest_variant_functions(var success:boolean):Boolean;

implementation

//DefaultFileFormatSettings : TFormatSettings = (
  //  CurrencyFormat: 1;
  //  NegCurrFormat: 5;
  //  ThousandSeparator: ' ';
  //  DecimalSeparator: '.';
  //  CurrencyDecimals: 2;
  //  DateSeparator: '-';
  //  TimeSeparator: ':';
  //  ListSeparator: ',';
  //  CurrencyString: 'C';
  //  ShortDateFormat: 'dd/mm/yyyy';
  //  LongDateFormat: 'dd" "mmmm" "yyyy';
  //  TimeAMString: 'AM';
  //  TimePMString: 'PM';
  //  ShortTimeFormat: 'hh:nn';
  //  LongTimeFormat: 'hh:nn:ss';
  //  ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
  //                    'Jul','Aug','Sep','Oct','Nov','Dec');
  //  LongMonthNames: ('January','February','March','April','May','June',
  //                   'July','August','September','October','November','December');
  //  ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  //  LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
  //  TwoDigitYearCenturyWindow: 50;
  //);
  //




constructor TStructuredObjectPointer.Create(name:string; newptr:PStructuredObject);
begin
  inherited create;
  self.id:=name;
  self.ptr:=newptr;
end;


function UpdateIoResult(var fil:TFileOfText):integer;
begin
  fil.errorcode:= IOResult;
  fil.errorstring:=ErrorCodeToText(fil.errorcode);
  Result:=fil.errorcode;
end;

function GetIoResult(var fil:TFileOfText):integer;
begin
//  fil.errorcode:= IOResult;
//  fil.errorstring:=ErrorCodeToText(fil.errorcode);
  Result:=fil.errorcode;
end;

function Bracket_Char_To_Bracket_Type(firstchar:char):TBracket;
begin
  Result:=no_bracket;
  case firstchar of
    '{': Result:=start_group;
    '[': Result:=start_array;
    '(': Result:=start_parenthesis;
    '}': Result:=end_group;
    ']': Result:=end_array;
    ')': Result:=end_parenthesis;
  end;
end;

function Bracket_type_To_Bracket_char(bracket:Tbracket):char;
begin
  Result:=illegal_bracket_char;
  case bracket of
    no_bracket         : Result:=no_bracket_char;
    start_group        : Result:='{';
    start_array        : Result:='[';
    start_parenthesis  : Result:='(';
    end_group          : Result:='}';
    end_array          : Result:=']';
    end_parenthesis    : Result:=')';
  end;
end;

function bracket_is_a_startbracket(bracket:TBracket):Boolean;
begin
  Result:=False;
  case bracket of
    start_group        : Result:=True;
    start_array        : Result:=True;
    start_parenthesis  : Result:=True;
  end;
end;

function bracket_is_an_endbracket(bracket:TBracket):Boolean;
begin
  Result:=False;
  case bracket of
    end_group          : Result:=True;
    end_array          : Result:=True;
    end_parenthesis    : Result:=True;
  end;
end;

function BracketMatch(bracket1, bracket2:TBracket):boolean;
var tempbracket:TBracket;
  procedure swap_brackets;
  begin
    tempbracket:=bracket2;
    bracket2:=bracket1;
    bracket1:=tempbracket;
  end;

begin
  Result:=False;
  if (bracket1 = no_bracket) or (bracket2 = no_bracket) then
  begin
    Result:=False;
  end
  else
  begin
    //Reduce the number of checks by ensuring if there is one "end_..." it is in bracket2
    //Note that if bracket2 contained an "end_..." then the result will be false as it should be
    if (bracket_is_an_endbracket( bracket1)) then swap_brackets;

    if (bracket1 = start_group) and (bracket2 = end_group) then Result:=True
    else
      if (bracket1 = start_parenthesis) and (bracket2 = end_parenthesis) then Result:=True
      else
        if (bracket1 = start_array) and (bracket2 = end_array) then Result:=True
        else
          Result:= False;
  end;
end;



procedure TBracketStack.reset_bracket_stack;
begin
//  for i:=0 to (MAX_NESTING_DEPTH -1) do
//  begin
//    fil.bracket_Stack[i]:=illegal_bracket_char;
//  end;
  self.bracket_Stack_pointer:=-1;
end;

procedure TBracketStack.init_bracket_stack(size:integer);
var i:integer;
begin
  SetLength(self.bracket_Stack, MAX_NESTING_DEPTH);
  for i:=0 to (MAX_NESTING_DEPTH -1) do
  begin
    self.bracket_Stack[i]:=illegal_bracket_char;
  end;
  self.bracket_Stack_pointer:=-1;
end;

CONSTRUCTOR TBracketStack.Create(size:integer);
begin
  inherited create;
  self.init_bracket_stack(size);
  writeln('-----Created bracketstack...');
end;

Destructor TBracketStack.Destroy;
begin
  setlength(self.bracket_Stack,0);
  writeln('-----Destroyed bracketstack...');
  inherited destroy;
end;

function TBracketStack.push_bracket_char(char_to_push:char ):Char;
begin
  //make sure that no illegal bracket char can bve pushed onto stack
  //since illegal_bracket_char is used to signal errors
  if char_to_push = illegal_bracket_char then char_to_push:=no_bracket_char;
  if self.bracket_Stack_pointer < -1 then
  begin
    Result:=illegal_bracket_char;
  end
  else
  begin
    self.bracket_Stack_pointer += 1;
    if self.bracket_Stack_pointer >= MAX_NESTING_DEPTH then
    begin
      self.bracket_Stack_pointer := MAX_NESTING_DEPTH -1;
      Result:=illegal_bracket_char;
    end
    else
    begin
      self.bracket_Stack[self.bracket_Stack_pointer] := char_to_push;
      Result := char_to_push;
    end;
  end;
end;


function TBracketStack.pop_bracket_char:Char;
begin
  if self.bracket_Stack_pointer >= MAX_NESTING_DEPTH then
  begin
    Result:=illegal_bracket_char;
  end
  else
  begin
    if self.bracket_Stack_pointer < 0 then
    begin
      self.bracket_Stack_pointer := -1;
      Result:=illegal_bracket_char;
    end
    else
    begin
      Result := self.bracket_Stack[self.bracket_Stack_pointer];
      self.bracket_Stack_pointer -= 1;
    end;
  end;
end;

function TBracketStack.pop_bracket:TBracket;
var
  bracket:Char;
begin
  bracket:=pop_bracket_char;
  if bracket = illegal_bracket_char then
    Result:=bracket_stack_range_error
  else
    Result:=Bracket_Char_To_Bracket_Type(bracket);
end;


function TBracketStack.push_bracket(bracket_to_push:TBracket ):TBracket;
begin
  Result := Bracket_Char_To_Bracket_Type(
              self.push_bracket_char(
                Bracket_type_To_Bracket_char(bracket_to_push)));
end;


function TBracketStack.bracket_Stack_tester:Boolean;
var test_stack: array of char;
    i:integer;
    max_depth:integer;
begin
  max_depth := length(self.bracket_Stack);
  SetLength(test_stack, max_depth);
  for i:=0 to (max_depth -1) do
  begin
    test_stack[i]:=char(random(253)+2);
    push_bracket_char( test_stack[i]);
  end;
  Result:=True;
  //reached end of stack and we should get an error
  Result := Result and (push_bracket_char(char(random(253)+2)) = illegal_bracket_char);
  Result := Result and (push_bracket_char(char(random(253)+2)) = illegal_bracket_char);
  Result := Result and (push_bracket_char(char(random(253)+2)) = illegal_bracket_char);
  Result := Result and (push_bracket_char(char(random(253)+2)) = illegal_bracket_char);
  Result := Result and (push_bracket_char(char(random(253)+2)) = illegal_bracket_char);
  for i:=(max_depth -1) downto 0 do
  begin
    if pop_bracket_char <> test_stack[i] then
    begin
      Result:=False;
    end;
  end;
  //reached beginning of stack and we should get an error
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  Result := Result and (pop_bracket_char = illegal_bracket_char);
  if Result = True then
  begin
    writeln;
    writeln('Stack test ok.');
    result := Result and BracketMatch(start_group, end_group);
    result := Result and BracketMatch(start_parenthesis, end_parenthesis);
    result := Result and BracketMatch(start_array, end_array);
    result := Result and BracketMatch(end_group, start_group);
    result := Result and BracketMatch(end_parenthesis, start_parenthesis);
    result := Result and BracketMatch(end_array, start_array);

    result := Result and not BracketMatch(no_bracket, start_group);
    result := Result and not BracketMatch(end_group, no_bracket);

    result := Result and not BracketMatch(end_parenthesis, start_group);
    result := Result and not BracketMatch(end_array, start_group);

    result := Result and not BracketMatch(end_group, start_parenthesis);
    result := Result and not BracketMatch(end_array, start_parenthesis);

    result := Result and not BracketMatch(end_parenthesis, start_array);
    result := Result and not BracketMatch(end_group, start_array);
    if Result = False then
    begin
      writeln('<error in bracket match>');
    end
    else
    begin
      writeln('<bracket match ok>');
      push_bracket( start_group);
        push_bracket( start_parenthesis);
          push_bracket( start_array);
          result := Result and BracketMatch(end_array,       pop_bracket);
          push_bracket( start_array);
            push_bracket( start_array);
              push_bracket( start_group);
                push_bracket( start_group);
                result := Result and BracketMatch(end_group,       pop_bracket);
              result := Result and BracketMatch(end_group,       pop_bracket);
            result := Result and BracketMatch(end_array,       pop_bracket);
          result := Result and BracketMatch(end_array,       pop_bracket);
          push_bracket( start_parenthesis);
            push_bracket( start_array);
              push_bracket( start_parenthesis);
              result := Result and BracketMatch(end_parenthesis, pop_bracket);
            result := Result and BracketMatch(end_array,       pop_bracket);
          result := Result and BracketMatch(end_parenthesis, pop_bracket);
        result := Result and BracketMatch(end_parenthesis, pop_bracket);
      result := Result and BracketMatch(end_group,       pop_bracket);
      result := Result and (pop_bracket = bracket_stack_range_error);
      result := Result and (pop_bracket = bracket_stack_range_error);
      result := Result and (pop_bracket = bracket_stack_range_error);
      push_bracket(start_array);
      result := Result and BracketMatch(end_array,       pop_bracket);
      result := Result and (pop_bracket = bracket_stack_range_error);
      if Result = False then
      begin
        writeln('<!!!error in bracket match + stack test>');
      end
      else
      begin
        writeln('<bracket match +stack test ok>');
        for i:=0 to (MAX_NESTING_DEPTH -1) do
        begin
          test_stack[i]:=char(random(253)+2);
          push_bracket_char(test_stack[i]);
        end;
        reset_bracket_stack;
        Result := Result and (pop_bracket_char = illegal_bracket_char);
        Result := Result and (pop_bracket_char = illegal_bracket_char);
        Result := Result and (pop_bracket_char = illegal_bracket_char);
        if Result = False then
        begin
          writeln('<!!!error in bracket reset test>');
        end
        else
        begin
          writeln('<bracket reset test ok>');
        end;
      end;
    end;
  end;
  setlength(test_stack,0);
  if Result = False then
  begin
    writeln;
    writeln('*** Error in bracket stack test ***');
  end
  else
  begin
    writeln;
    writeln('*** Stack test ok ***');
  end;
end;

function OpenStructuredTextFile(filename:String):TFileOfText;
begin
  Result.filename:=filename;
  Result.errorcode:=0;
  Result.errorstring:=ErrorCodeToText(Result.errorcode);
  Result.level_depth := 0;
  Result.ignore_depth := -1;
  Result.lineno :=0;
  Result.SimpleFileReader:=TFileReader.Create;

  AssignFile(Result.FileHandle, filename);
  {$i-}
  reset(result.FileHandle);
  UpdateIoResult(Result);
  {$i+}

//  writeln(':IORES='+Result.errorstring);
  //If IOresult <> 0 then
  //begin
  //  writeln ('File ',filename,' doesn''t exist')
  //end
  //else
  //begin
  //  writeln ('File ',filename,' exists');
  //end;
  Result.bracketStack:=TBracketStack.Create(MAX_NESTING_DEPTH);
  //Result.BracketStack.init_bracket_stack(MAX_NESTING_DEPTH);
end;


function MakeSectionBuffer(length_of_buffer:integer):TSectionbuffer;
var
  x:Integer;
begin
  result.length_of_buffer := length_of_buffer;
  SetLength(Result.buffer, length_of_buffer);
  for x := 0 to (result.length_of_buffer - 1) do
  begin
   result.buffer[x].value:='';
   result.buffer[x].section_type:=vt_undefined;
  end;
end;

function SetParseChars:TParseChars;
begin
  Result.comment_char:=comment_char;
  Result.Quotes_Char:=Quotes_Char;
  Result.escape_char:=escape_char;
end;

function read_and_parse_line(var fil:Tfileoftext):TfileLineParsed;


var textline:string;
    popped_char:char;
    popped_bracket:TBracket;

  function update_error(old_error:TLineCodeError;
                         new_error:TLineCodeError):TLineCodeError;
  begin
    if (old_error = lce_no_error) then
    begin
      Result:=new_error;
    end
    else
    begin
      Result:=old_error;
    end;
  end;

  var
      parsechar:TParseChars;
begin
  parsechar:=SetParseChars;

//  writeln(' read and parse  ');
  {$i-}
  readln(fil.FileHandle,Result.raw_str);
  UpdateIoResult(fil);
  {$i+}
  Result.error_code:=lce_no_error;
  Result.LineType:=lt_unk; //Initialize as unknown

  if (Result.raw_str <> '') then
  begin
    textline:=  remove_whitespace(
                  fil.SimpleFileReader.remove_comment(Result.raw_str),parsechar);
//    writeln('*''',textline,'''*');
    Result := DetectLineType(textline);
    if bracket_is_a_startbracket(Result.bracket) then
    begin
      if fil.bracketStack.push_bracket( result.bracket) = bracket_stack_range_error then
      begin
        Result.error_code:=update_error(Result.error_code, lce_nesting_to_deep);
      end;
    end
    else
    begin
      if bracket_is_an_endbracket(Result.bracket) then
      begin
        popped_bracket := fil.bracketStack.pop_bracket;
        if popped_bracket = bracket_stack_range_error then
        begin
          Result.error_code:=update_error(Result.error_code, lce_bracket_mismatch);
        end
        else
        begin
          if not BracketMatch(popped_bracket, result.bracket) then
          begin
            Result.error_code:=lce_bracket_mismatch;
            Result.error_code:=update_error(Result.error_code, lce_bracket_mismatch);
          end;
        end;
      end;
    end;

    //Tlinetype = (rec,lst,tag,cmt,rnd,lnd,unk, ign);
    ///<rec=record, lst=list of records, tag=tagged value line, cmd=command,rnd=record end, lnd= list end, unk=unknown
    case Result.LineType of
      lt_rec:
        write('<record>');
      lt_lst:
        write('<List>');
      lt_tag:
        if Result.value_str = '' then
        begin
          write('<Tag>',Result.identifier, '=',Result.value);
        end
        else
        begin
          write('<Tag>',Result.identifier, '="',Result.value_str,'"');
        end;
      lt_rnd:
        write('<record end>');
      lt_dat:
        write('<data line>');
      lt_lnd:
        write('<List end>');
      lt_ign:
        write('<Ignored>');
      lt_unk:
        write('<Unknown>');
      lt_err:
        writeln('***Error at line ',fil.lineno+1);
    else
      writeln('***Malformed type at line', fil.lineno+1);
    end;
    //TBracket = ( no_bracket, start_group, end_group, start_parenthesis, end_parenthesis, start_array, end_array );
    case Result.bracket of
      no_bracket:
        write(' -No Bracket- ');
      start_group:
        begin
          fil.level_depth := fil.level_depth + 1;
          write(' {{{ /',fil.level_depth,'/');
        end;
      end_group:
        begin
          fil.level_depth := fil.level_depth - 1;
          write(' }}} /',fil.level_depth,'/');
        end;
      start_parenthesis: //not implemented yet
        write(' ((( ');
      end_parenthesis: //not implemented yet
        write(' ))) ');
      start_array:
        write(' [[[ '); //not implemented yet
      end_array:
        write(' ]]] '); //not implemented yet
    else
      write('<***Error: bracket unknown***>');
    end;
    if fil.ignore_depth > -1 then
    begin
      if (fil.level_depth <= fil.ignore_depth) then
      begin
        if Result.bracket = end_group then
        begin
          if Result.LineType = lt_ign then
          begin
            fil.ignore_depth := -1;
          end
          else
          begin
            writeln('***Error mismatch in brackets types at line ',fil.lineno);
          end;
        end;
      end
      else
      begin
        write(' *ignored* ');
      end;
    end
    else
    begin
      if (Result.LineType = lt_ign) AND
         (Result.bracket = start_group) then
      begin
        fil.ignore_depth := fil.level_depth - 1;
      end;
    end;
    writeln;
    fil.lineno := fil.lineno +1;
  end
  else
  begin
    Result.error_code:=update_error(Result.error_code, lce_empty_line);
  end;

end;

//procedure CloseStructuredTextFile(var fil:TFileOfText);
//begin
//end;

function CloseStructuredTextFile(var fil:TFileOfText):integer;
begin
  writeln('++++close text file');
  {$i-}
  CloseFile(fil.FileHandle);
  UpdateIoResult(fil);
  {$i+}
  fil.bracketStack.Destroy;
  if fil.level_depth <> 0 then
  begin
    if fil.errorcode = 0 then
    begin
      writeln('***Error mismatch in brackets');
    end
    else
    begin
      writeln('***Error in closing file');
    end;
  end;
  Result:= fil.errorcode;
end;



function FindChar_but_skip_quotes(str:string; ch:Char; startpos:integer):integer;
  var i:integer;
      in_quotes :boolean;
      preceeding_char:char;
begin
  i:=startpos;
  preceeding_char:=' '; //anything but escape_Char
  //write ('FCBS : startpos = i =', i,' ! ');
  if i < length(str) then
  begin
    in_quotes:=False;
    while (i<length(str)) and ((str[i]<>ch) or (in_quotes)) do
    begin
      if (str[i] = Quotes_Char) and (preceeding_char <> escape_char) then
      begin
        if in_quotes then in_quotes:=False else in_quotes:=True;
      end;
      preceeding_char:=str[i];
      i:=i+1;
      //write ('<FCBS loop: i =', i,' > ');
    end;
    if (str[i]=ch) then
    begin
      //write ('<FCBS: Found at: ',i,'> ');
      result:=i;
    end
    else
    begin
      //write ('<FCBS error:did not find> ');
      Result:= FINDERROR_DID_NOT_FIND;
    end;
  end
  else
  begin
    //write ('<FCBS error:beyond> ');
    Result:=FINDERROR_POSITION_BEYOND_STRING;
  end;
end;

function FindChar(str:string; ch:Char):integer;
  var i:integer;
begin
  i:=1;
  while (i<length(str)) and (str[i]<>ch) do
  begin
    i:=i+1;
  end;
  if Length(str) > 0 then
  begin
    if (str[i]=ch) then result:=i else Result:= -1;
  end
  else
  begin
    Result:= FINDERROR_DID_NOT_FIND;
  end;
end;

function FindEqualSign(str:string):integer;
begin
  Result:=findchar(str, '=');
end;

function RemoveQotationMarks(indstr:string; var errorflag:TLineCodeError):string;
//  var i:integer;
begin
  errorflag:=lce_syntax_error;
  if (indstr[1]='"') and (indstr[length(indstr)]='"') then
  begin
    indstr:=Copy(indstr,2,length(indstr)-2);
//        write('<String found (',indstr,')>');

    errorflag:=lce_no_error;
  end
  else
  begin
//        write('<String not found!>');
    errorflag:=lce_error_in_quotes;
  end;
  result:=indstr;
end;

function GetTag(str:string):String;
var i:integer;
    //EqualPos:integer;
    retur:string;
begin
  i:=1;
  retur:='';
  while (i<=length(str)) and (str[i]<>'=') do
  begin
    retur:=retur+str[i];
    i:=i+1;
  end;
  result:=retur;
end;

function GetValue(str:string):string;
  var i:integer;
      EqualPos:integer;
      retur:string;
begin
  EqualPos:=FindEqualSign(str);
  if EqualPos = FINDERROR_POSITION_BEYOND_STRING then EqualPos:=1;
  if EqualPos = FINDERROR_DID_NOT_FIND then EqualPos:=1;
  i:=EqualPos;
  retur:='';
  while (i<=length(str)) do
  begin
    retur:=retur+str[i];
    i:=i+1;
  end;
  result:=retur;
end;

function Parse_to_comma_or_end(str:string; var pos:integer):TTagValueAndType;
var startpos,len:integer;
begin
  startpos:=pos;
  pos:=FindChar_but_skip_quotes(str,',',Pos);
  if (pos = FINDERROR_POSITION_BEYOND_STRING ) then
  begin
    Result.Section_value.section_error:=se_parsing_beyond_string;
    write ('-<PTCOE FINDERROR_POSITION_BEYOND_STRING>-');
  end
  else
  begin
    //if we did not find a comma but have more string left, then assume last part is a section
    if (pos = FINDERROR_DID_NOT_FIND) then
    begin
      pos:=length(str)+1;
    end;
    len:=(pos) - startpos;
    if len > 0 then Result.Section_value.value:=Copy(str,startpos,len) else Result.Section_value.value :='';
//    write ('-<PTCOE str[', startpos, ',', pos, '="', Result.value,'">-');
    result.tag_type_abstract.value:='';
    Result.Section_value.section_error:=se_Section_syntax_is_ok;
    pos:=pos+1;
  end;
end;

function Extract_Value_and_Type(str:string):TSectionbuffer;
var
    idx, number_of_Sections:integer;
     startpos, pos:integer;
     section:TTagValueAndType;
     //sectionbuffer:TSectionbuffer;
     temp_line_error_code:TLineCodeError;

begin
  result.length_of_buffer:=0;
//  Result.tag_type:='';
//  Result.value:='';
//  Result.tag_type_abstract:='';
  Result.errorcode:=0;
  startpos:=FindEqualSign(str);
  Result.Found_Equal_Sign:=False;
  Result.Found_tags:=False;
  if length(str) > 0 then
  begin
    if (startpos = FINDERROR_POSITION_BEYOND_STRING) then
    begin
      write('error: beyond string');
      Result.errorcode:=FINDERROR_POSITION_BEYOND_STRING;
    end
    else
    begin
      //if we have no equal sign parse it as sections
      if startpos = FINDERROR_DID_NOT_FIND then
      begin
        startpos:=1;
  //      write('No equal sign ');
      end
      else
      begin
        startpos += 1; //skip equal sign
        Result.Found_Equal_Sign:=True;
      end;
    end;
    number_of_Sections :=0;

    if startpos > 0 then
    begin
      //find the number of sections so we can allocate for it
      pos:=startpos;

      while (pos < length(str))  do
      begin
  //      write ('( pos = ',pos,') ');

        section :=Parse_to_comma_or_end(str, pos);
        //Since the length is greater than 1 there is at least one section
        //However if the position is at the end the string then there is no more to
        //find and we get the seek beoynd error -> no more sections
        if section.Section_value.section_error <> se_parsing_beyond_string then
        begin
          number_of_Sections += 1;
  //        writeln;
  //        write ('**found section at pos= ', pos, '="', section.value,'"  ');
        end;
        //We found no comma -> all the string is the value -> stop while loop
        if section.Section_value.section_error = se_parser_did_not_find then
        begin
          pos := length(str);
        end;
      end;
  //
      if number_of_Sections > 0 then
      begin
  //      writeln ('( Round 2 , sections = ',number_of_Sections,') ');
        Result.Found_tags:=True;
        pos:=startpos;
        idx := 0;
        Result:= MakeSectionBuffer(number_of_sections);
  //
        while (((pos <= (length(str)+1)) and (pos >0)) and
               (idx < (number_of_Sections))
              ) do
        begin
  //        write ('( pos = ',pos,') ');
          section := Parse_to_comma_or_end(str, pos);
  //        write ('( pos after = ',pos,' - Value="',section.value,'") ');
          if section.Section_value.section_error = se_Section_syntax_is_ok then
          begin
            if pos >= 0 then
            begin
              temp_line_error_code:=lce_no_error;
              section.Section_value.value:= RemoveQotationMarks( section.Section_value.value, temp_line_error_code );
              //if temp_line_error_code = lce_error_in_quotes then
              //begin
              //  section.;
              //end;
              Result.buffer[idx].value := section.Section_value.value;
  //            write('<-section [',idx,']="',Result.buffer[idx].str,'"-> ');
              idx += 1;
            end
            else
            begin
              write ('( Error pos < 0 =',pos,') ');
            end;
          end
          else
          begin
            Result.buffer[idx].value := '';
          end;
        end;
      end
      else
      begin
        Result.errorcode:=FINDERROR_POSITION_BEYOND_STRING; //No sections
      end;
    end
    else //Negative start position
    begin
      Result.errorcode:=FINDERROR_POSITION_BEYOND_STRING; //No sections
    end;
  end
  else //Empty string
  begin
    Result.errorcode:=FINDERROR_POSITION_BEYOND_STRING; //No sections
  end;
end;

procedure Test_parse_functions;
var testbuffer:TSectionbuffer;
     idx:integer;
     test_Str:string;
     SimpleFileReader : TFileReader;

procedure DoTestBuffer;
var idx:integer;
     parsechar:TParseChars;

begin
  parsechar:=SetParseChars;

  writeln('~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""');
  writeln('_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,');
  writeln('~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""');
  writeln('_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,');
  writeln('Teststr = "',test_Str,'"');
  testbuffer:=Extract_Value_and_Type(test_Str);
  test_Str:=SimpleFileReader.remove_comment(remove_whitespace(test_Str, parsechar));
  writeln('Teststr cleaned= "',test_Str,'"');
  testbuffer:=Extract_Value_and_Type(test_Str);

  writeln('Found equal sign=',testbuffer.Found_Equal_Sign, ' - Found_tags=', testbuffer.Found_tags);
  if testbuffer.length_of_buffer >0 then
  begin
    for idx:=0 to testbuffer.length_of_buffer-1 do
    begin
      write('Item=["',testbuffer.buffer[idx].value ,'"], ');
    end;
  end;
  WriteLn;
end;

var
  success:boolean;
  i:integer;

begin
  SimpleFileReader:=TFileReader.Create;
  test_Str:='test="item1",2334,"item,2",item_3';
  DoTestBuffer;
  test_Str:='test "item1",2334,"item,2",item_3';
  DoTestBuffer;
  test_Str:='           ;        test ="item1"';
  DoTestBuffer;
  test_Str:='test   =         "item1 1 2 3 ;hest" ;comment added';
  DoTestBuffer;
  test_Str:='test=1.23323e-6, 1e-5';
  DoTestBuffer;
  test_Str:='test="aa"';
  DoTestBuffer;
  test_Str:='test=';
  DoTestBuffer;
  test_Str:='(record="game","gametype","record"';
  DoTestBuffer;

  writeln;
  success:=True;
  //i:=0;
  //while ((i < 1000) and (success)) do
  //begin
  //  success:=UnitTest_variant_functions(success); //Generates random numbers and conversions
  //  i += 1;
  //  write('#');
  //end;
  //writeln;
  //if i=1000 then
  //begin
  //  writeln('_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,');
  //  writeln('*                       !!! GREAT SUCCESS !!!                        *');
  //  writeln('_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,_,~""~,');
  //end;

  SimpleFileReader.Destroy;
end;



function DetectLineType(textline:string):TFileLineParsed;
  //Find out what we are looking at (assumes that white space has been removed

(*
TBracket = ( no_bracket, //This line has no bracket (no tree structure of its own)
             start_group, //Starts a group (structured set of data)
             end_group,   //ends a group of structured data (also means that we now go up in the tree structure again for later items
             start_parenthesis, //Currently unused (skip until end_parenthesis for now)
             end_parenthesis,   //currently unused (resume normal operation if detected )
             start_array,       //Currently not fully developed, but is supposed to be for 2d arrays made lt_dat lines (e.g. for images)
             end_array );       //ends an array

*)



  function GetIdent(str:string; ExpectedType:boolean):string;
    var retur:string;
  begin
    retur:='';
    if ExpectedType=ExpectNonTagLine then
    begin
      if length(str) >= 5 then
      begin
        retur:=str[2]+str[3]+str[4]+str[5];
        retur:=upcase(retur);
      end;
    end;
    result:=retur;
  end;

  function ParseLine(parsestr:string; ExpectedType:TLineType):TFileLineParsed;

  var value:double;
      errorflag:TLineCodeError;
      //line_error_code:integer;
      code:integer;
      retur:TFileLineParsed;

  begin
    retur.error_code:=lce_no_error;
    case ExpectedType of
      lt_rec: //record start
      Begin
        retur.identifier:='Record'; //for structs/records
        retur.value_str:=GetValue(parsestr);
        retur.LineType:=lt_rec;
      end;
      lt_lst: //list start
      begin
        retur.identifier:='List';  //For array like structures
        retur.value_str:=GetValue(parsestr);
        retur.LineType:=lt_lst;
      end;
      lt_tag: //Tag
      begin
        errorflag:=lce_no_error;
        retur.identifier:=GetTag(parsestr);
        retur.value_str:=GetValue(parsestr);
        retur.value_str:=Copy(retur.value_str,2,length(retur.value_str)-1);
        if length(retur.value_str)>=1 then
        begin
          Val(retur.value_str ,retur.value,code);
        end
        else
        begin
          retur.value:=0;
          code:=-1;
          retur.error_code:=lce_abrupt_line_termination;
        end;
        case code of
          1: //If it is a non numeric from Val() above
          begin
            retur.value_str:=RemoveQotationMarks(retur.value_str,errorflag);
//            if errorflag=true then write ('<malformed string (missing quotes>');
            retur.error_code:=errorflag;
          end;
          2:
          begin
//            write('(Incorrect number format)');
            errorflag := lce_malformed_number;
            retur.error_code:=lce_malformed_number;
          end;
        end;
//        writeln('Tagvalue "',retur.value_str,'":=',retur.value:3:2,' code=',code, 'Err=',errorflag,' ');
        if errorflag = lce_no_error then
        begin
          retur.LineType:=lt_tag;
        end
        else
        begin
          retur.LineType:=lt_err;
          retur.error_code:=errorflag;
        end;
      end;

      lt_lnd: //List end
      begin
        retur.identifier:='EndList';
        retur.value_str:='EndList';
//        write('<EndList>');
        retur.LineType:=lt_lnd;
      end;

      lt_rnd: //Record end
      begin
        retur.identifier:='EndRecord';
        retur.value_str:='EndRecord';
//        write('<EndRecord>');
        retur.LineType:=lt_rnd;
      end;
      else
      begin
        retur.identifier:='Unknown';
        retur.value_str:='Unknown';
        retur.value:=-1;
//        write('<???>');
        retur.LineType:=lt_unk;
      end;
    end;
    result := retur;
  end;

  var firstchar:Char;
  var ident:string;
      ParsedStructure:TFileLineParsed;

begin
  ParsedStructure.LineType := lt_ign;
  ParsedStructure.bracket := no_bracket;
  ParsedStructure.identifier := '';
  ParsedStructure.value := 0.0;
  ParsedStructure.value_str := '';
  ParsedStructure.error_code:=lce_empty_line;
  if Length(textline) >=1 then
  begin
    ParsedStructure.error_code:= lce_no_error;
    //write('< "',textline,'" > ');
    firstchar:=textline[1];
    //check the problem: Will this get destroyed by parseline???
    case firstchar of
      '{', '(', '[': //We now go a level down and start a block
      Begin
//        write('Found "{" ');
        ident:=GetIdent(textline, ExpectNonTagLine);
        case ident of
          'RECO':
          begin
            ParsedStructure:=ParseLine(textline, lt_rec);
          end;
          'LIST':
          begin
            ParsedStructure:=ParseLine(textline, lt_lst);
          end;
          '$REC':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end;
          '$LIS':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end;
          '$LIN':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end
          else
          begin
  //          write('Unknown:"',ident,'"');
            ParsedStructure.LineType := lt_unk;
          end;
        end;
//        ParsedStructure.bracket := start_group;
        ParsedStructure.firstChar:= FirstChar;
        ParsedStructure.bracket  := Bracket_Char_To_Bracket_Type(firstchar);
      end;
      '}', ')', ']':
      Begin
        ident:=GetIdent(textline, ExpectNonTagLine);
  //      write('"',ident,'"');
        case ident of
          'RECO':
          begin
            ParsedStructure:=ParseLine(textline, lt_rec);
          end;
          'LIST':
          begin
            ParsedStructure:=ParseLine(textline, lt_lst);
          end;
          '$REC':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end;
          '$LIS':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end;
          '$LIN':
          begin
  //          writeln('Ident :"',ident,'" ignored');
            ParsedStructure.LineType := lt_ign;
          end
          else
          begin
  //          write('Unknown:"',ident,'"');
            ParsedStructure.LineType := lt_unk;
          end;
        end;
//        ParsedStructure.bracket := end_group;
        ParsedStructure.firstChar:=FirstChar;
        ParsedStructure.bracket:=Bracket_Char_To_Bracket_Type(firstchar);
      end;
      '#':
      begin
        ParsedStructure.LineType := lt_dat;
      end;
      '$', '@', '!', '%':
      begin
//        Writeln('System tag ignored (for now)');
        ParsedStructure.LineType := lt_ign;
        ParsedStructure.bracket := no_bracket;
        ParsedStructure.error_code:= lce_ignored_line;
      end
      else
      Begin
        ident:=GetIdent(textline, ExpectTagLine);
        ParsedStructure:=ParseLine(textline, lt_tag);
  //      write('"',ident,'"');
  //      writeln('Tag');
        ParsedStructure.bracket := no_bracket;
      end;
    end;
  end;
  result := ParsedStructure;
end;

function LineCodeToErrorString(linecode_error:TLineCodeError):string;
begin
  case linecode_error of
    lce_ignored_line: result:='Line is ignored. (possible future expansion)';
    lce_empty_line: result:='Line is empty.';
    lce_no_error: result:='No error detected.';
    lce_abrupt_line_termination: result:='Line has unexpectedly ended.';
    lce_malformed_number: result:='Line is ignored. (possible future expansion)';
    lce_unknown_linetype: result:='Linetype is not recognized.';
    lce_error_in_quotes: result:='Quotation marks are somhow wrong or unmatched?';
    lce_syntax_error: result:='General purpose syntax error.';
    lce_illegal_character: result:='Non ascii character used in token.';
  otherwise
    result:='Unknown error code:'+IntToString(Integer(linecode_error));
  end;
end;


var
  writefile:TextFile;
  tabdepth:integer;
//  currentError:integer;
//  CurrentErrorText:string;


function ErrorCodeToText(errorcode:integer):string;
begin
  case errorcode of
    DOS_ERROR_SYNTAX: Result:= 'Syntax error in file';
    0: result:='';
    //dos errors :
    2: result:='File not found.';
    3: result:='Path not found.';
    4: result:='Too many open files.';
    5: result:='Access denied.';
    6: result:='Invalid file handle.';
    12: result:='Invalid file-access mode.';
    15: result:='Invalid disk number.';
    16: result:='Cannot remove current directory.';
    17: result:='Cannot rename across volumes.';
    //I/O errors :
    100: result:='Error when reading from disk.';
    101: result:='Error when writing to disk.';
    102: result:='File not assigned.';
    103: result:='File not open.';
    104: result:='File not opened for input.';
    105: result:='File not opened for output.';
    106: result:='Invalid number.';
    //Fatal errors :
    150: result:='Disk is write protected.';
    151: result:='Unknown device.';
    152: result:='Drive not ready.';
    153: result:='Unknown command.';
    154: result:='CRC check failed.';
    155: result:='Invalid drive specified.';
    156: result:='Seek error on disk.';
    157: result:='Invalid media type.';
    158: result:='Sector not found.';
    159: result:='Printer out of paper.';
    160: result:='Error when writing to device.';
    161: result:='Error when reading from device.';
    162: result:='Hardware failure.';
    otherwise
      result:='Unknown error code ='+inttostring(errorcode);
  end;
end;

//function IoresultToText(var fileToWrite:TFileOfText):string;

function IoresultToText(var fileToWrite:TFileOfText) :string;
begin
  fileToWrite.errorcode:=IOResult;
//  writeln('io result =',currentError);
  fileToWrite.errorstring:=ErrorCodeToText(fileToWrite.errorcode);
  result:=fileToWrite.errorstring;
end;


///Indentation is only to make the file more readable. The reader does not use indentation
///
function write_indent(var fileToWrite:TFileOfText; TabDepth:integer):string;
const
  spacesforprint = '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    '+
                   '                                                                                                    ';
  TablengthTotal = length(spacesforprint);
  indent = 2;
  MaxTabDepth = (TablengthTotal div indent) - 1;

begin
  REsult:='';
  TabDepth:=min(tabdepth, MaxTabDepth);
  write(fileToWrite.FileHandle, Copy(spacesforprint, 0, TabDepth*indent));
end;

function write_text_with_indent(var fileToWrite:TFileOfText; textstring:string; indentchange:integer; postchange:boolean):string;
begin
  if fileToWrite.errorcode = 0 then
  begin
    if postchange = False then
      tabdepth:=tabdepth + indentchange;
    write_indent(fileToWrite, tabdepth);
    result := IoresultToText(fileToWrite);
    if result = '' then
    begin
      writeln(fileToWrite.FileHandle,textstring);
      result := IoresultToText(fileToWrite);
      if postchange = True then
        tabdepth:=tabdepth + indentchange;
    end;
  end
  else
  begin
    Result:=filetowrite.errorstring;
  end;
end;

function write_record_start(var fileToWrite:TFileOfText; recordname:string):string;
begin
  Result:= write_text_with_indent(fileToWrite, '{record = "'+recordname+'"', 1, True);
end;

function write_record_end(var fileToWrite:TFileOfText):string;
begin
  Result:= write_text_with_indent(fileToWrite, '}record', -1, False);
end;

function write_tag(var fileToWrite:TFileOfText; tagname:string; tagvalue:string; IsAString:boolean ):string;
begin
  if IsAString then
    Result:= write_text_with_indent(fileToWrite, tagname+' = "'+tagvalue+'"', 0, True)
  else
    Result:= write_text_with_indent(fileToWrite, tagname+' = '+tagvalue, 0, True);
end;

function write_item(var fileToWrite:TFileOfText; tagvalue:string; IsAString:boolean ):string;
begin
  if IsAString then
    Result:= write_text_with_indent(fileToWrite, '# "'+tagvalue+'"', 0, True)
  else
    Result:= write_text_with_indent(fileToWrite, '# '+tagvalue+'', 0, True)
end;


function write_list_start(var fileToWrite:TFileOfText; listname:string; numitems:integer):string;
begin
  result := write_tag(fileToWrite,listname,IntToString(numitems),false);
  Result := write_text_with_indent(fileToWrite, '{list = "'+listname+'"', 1, True);
end;

function write_list_item(var fileToWrite:TFileOfText; tagname:string; item:real):string;
begin
  result := write_tag(fileToWrite,tagname,FloatToString( item, 0,8) ,false);
//  Result := write_text_with_indent(fileToWrite, '{list = "'+listname+'"', 1, True);
end;

function write_list_end(var fileToWrite:TFileOfText):string;
begin
  Result:= write_text_with_indent(fileToWrite, '}list', -1, False);
end;



constructor TFileReader.Create;
begin
  Inherited;
  //add my code
  HasReadFile:=false;
end;

destructor TFileReader.Destroy;
begin

  //Do not add code after next line
  inherited; //We call the inherited destructor after our own
end;

//function TFileReader.remove_whitespace(indline:string):string;
//var j:integer;
//    result_str:string;
//    preceeding_char:char;
//    in_quotes:boolean;
//begin
//  result_str:='';
//  in_quotes:=False;
//  preceeding_char:=' '; //any char not escape char
//  for j:=1 to Length(indline) do
//  begin
//    //Toggle if we are in quotes or not.
//    if (indline[j] = Quotes_Char)  then
//    begin
//      if in_quotes then
//      begin
//        //still has an error if we are in quotes
//        if (preceeding_char <> escape_char) then
//          in_quotes:=False;
//      end
//      else
//        in_quotes:=True;
//    end
//    else
//    begin
//      if preceeding_char = escape_char then //We are not in quotes and if we had escape char before it was suppressed
//      begin
//        result_str := result_str + escape_char;
//        write(indline[j]);
//      end;
//    end;
//    if (in_quotes) then //We store without question if we are within quotes
//    begin
//      //We suppress escape char but handle it through the preceeding mechanism shown above
//      if (indline[j] <> escape_char) then
//        result_str := result_str + indline[j];
//    end
//    else
//    begin
//      if ((indline[j]<>' ') AND (indline[j]<>#9) ) then
//      begin
//        result_str := result_str + indline[j];
//      end;
//    end;
//    preceeding_char:=indline[j];
//  end;
//  remove_whitespace := result_str;
//end;

//function TFileReader.Convert_escapechars(


///Remember to call remove comment before removing whitespace
function TFileReader.remove_comment(indline:string):string;
  var j:integer;
      result_str:string;
      in_quotes:boolean;
      preceeding_char:char;

begin
  result_str:='';
  j:=1;
  in_quotes:=False;
  preceeding_char:=' '; //any char not escape char
  while (j<=Length(indline)) AND ((indline[j]<>comment_char) Or in_quotes)  do
  begin
    if (indline[j] = Quotes_Char)  then
    begin
      if in_quotes then
      begin
        if (preceeding_char <> escape_char) then
          in_quotes:=False;
      end
      else
        if (preceeding_char <> escape_char) then
          in_quotes:=True;
    end;
    result_str := result_str + indline[j];

    preceeding_char:=indline[j];
    j:=j+1;
  end;
  remove_comment := result_str;
end;

function TFileReader.parseline(inputline:string; var name:string; var id:integer):TLinetype;
var linetype:integer;
    equ_pos, j, tag_id:integer;
    tag_name, tag_idstr, temp_str:string;
    parsechar:TParseChars;
begin
    parsechar:=SetParseChars;

  temp_str:=remove_comment(remove_whitespace(inputline, parsechar));
  equ_pos:=0;
  j:=1;
  parseline:=lt_ign;
  if (temp_str[1]='+') then
  begin
    tag_name:=Copy (temp_str,2,length(temp_str)-1);
    name:=tag_name;
    parseline:=lt_cmd;
  end
  else
  begin
    while (j<=Length(temp_str)) and (equ_pos=0) do
    begin
      if (temp_str[j]='=') then
      begin
        equ_pos:=j;
      end;
      j:=j+1;
    end;
    if (equ_pos>0) AND (equ_pos<length(temp_str)) then
    begin
      tag_name:=Copy(temp_str,1, (equ_pos-1));
      name:=tag_name;
      tag_idstr:=copy(temp_str, equ_pos+1, length(temp_str)-(equ_pos));
      //writeln('<equ_pos=',equ_pos,'><tagidstr="',tag_idstr,'"');
      Try
        id:=StrToInt(tag_idstr);
      except
        On E : EConvertError do
        begin
          id:=-1;
        end;
      end;
      if (id<0) then
      begin
        parseline:=lt_ign;
      end
      else
      begin
        parseline:=lt_tag;
      end;
    end
    else
    begin
      parseline:=lt_ign;
    end;

  end;
end;

procedure TFileReader.test_unit;
var
  test_tag,teststr:string;
  test_id:integer;
  parsechar:TParseChars;
begin
  teststr:='   gnu       =                        45;heste er dog dummere...         ';
  writeln(remove_comment(remove_whitespace(teststr, parsechar)));
  if (parseline(teststr, test_tag, test_id)=LT_tag) then
  begin
    writeln('<Id "',test_tag,'">=<',test_id,'>');
  end
  else
  begin
    writeln('malformed tag line!');
  end;

end;

function TFileReader.WriteTagsUnit(tagsunitfilename:string):boolean;
var tagsfile:TextFile;
    temp_str,tag_name,indstr:string;
    j,tag_id:integer;
    curr_tag:TTag;

begin
  Result:= False;
  AssignFile(tagsfile,tagsunitfilename);
  Rewrite(tagsfile);
  writeln(tagsfile,'unit tags;');
  writeln(tagsfile);
  writeln(tagsfile,'{$mode objfpc}{$H+}');
  writeln(tagsfile);
  writeln(tagsfile,'interface');
  writeln(tagsfile,'  uses  Classes, SysUtils;');
  writeln(tagsfile);
  writeln(tagsfile,'const');
  for j := 0 to IDtags.TagList.Count - 1 do
  begin
    curr_tag:=IDtags.GetTag(j);
    writeln(tagsfile,'  ',curr_tag.tagname,' = ', curr_tag.tagid,';');
  end;
  writeln(tagsfile);
  writeln(tagsfile,'implementation');
  writeln(tagsfile,'end.');

  CloseFile(tagsfile);
end;

///Not fully developed function. to simple design currently only global tags list
function TFileReader.ReadTagsFile(tagsfilename:string):boolean;
var tagsfile:TextFile;
    temp_str,tag_name,indstr:string;
    tag_id:integer;
    linetype:TLinetype;
begin
  Result:=False;
  if (not(Assigned(IdTags))) then
  begin
    IdTags:=TTagsList.Create;
    HasReadFile:=true;
    AssignFile(tagsfile,tagsfilename);
    reset(tagsfile);
    while not(EOF(tagsfile)) do
    begin
      readln(tagsfile,indstr);
      linetype:=parseline(indstr, tag_name, tag_id);
      if (linetype=LT_tag) then
      begin
//        writeln('<Id "',tag_name,'">=<',tag_id,'>');
        IdTags.AddTag(tag_name,tag_id, 0, TT_Global_V1);
      end
      else
      begin
        if (linetype=lt_rec) then
        begin
          writeln('<Record "',tag_name,'">');
        end;
      end;
    end;
    CloseFile(tagsfile);
  end;
end;


constructor TStructuredObject.Create;
begin
  TagList:=TTagsList.Create;
end;

Destructor  TStructuredObject.Destroy;
begin
  self.Free_tags_array;
  inherited destroy;
end;


Function    TStructuredObject.convert_tags_to_objectdata:TStructuredObjectError;
begin
  Result := SOE_no_Error;
  /// stub: needs code
end;

Function    TStructuredObject.convert_objectdata_to_tags:TStructuredObjectError;
begin
  Result := SOE_no_Error;
  /// stub: needs code
end;

Function    TStructuredObject.Prepare_tags_array(number_of_tags:integer):TStructuredObjectError;
begin
  Result := SOE_no_Error;
  self.Free_tags_array;
  self.TagList.Create;
end;

procedure   TStructuredObject.Free_tags_array;
begin
  self.TagList.Destroy;
end;

{
function TFileReader.ReadSaveFile(Savefilename:string):boolean;
begin
  {
  read game record
  intialize game and set game state
  read game map
  For all in player list do
    read player record
    add player to game
    read fog of war
    add fog of war to player
    for all units in player do
      read unit record
      add unit to player and set unit state
    for all cities in player do
      read city record including improvements and state of city and citizens
      add city to player and set city state
    for all map improvements in player do
      read map improvement record
      add map improvement to player and to map

  }
end;
}

{
function TFileReader.ReadSimplifiedSaveFile(Savefilename:string):boolean;
begin
  {
  read game record
  intialize game and set game state
  read game map
  For all in player list do
    read player record
    add player to game
    read fog of war
    add fog of war to player
    for all units in player do
      read unit record
      add unit to player and set unit state
    for all cities in player do
      read city record including improvements and state of city and citizens
      add city to player and set city state
  }
end;
}

end.


