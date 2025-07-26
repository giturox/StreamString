unit loggerV2;
{
  $Id: logger.pas,v 1.2 2006/11/26 16:58:04 savage Exp $

}
{******************************************************************************}
{                                                                              }
{                Error Logging Unit                                            }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Logging functions...                                                       }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{               2001 - DL : Initial creation                                   }
{         25/10/2001 - DRE : Added $M+ directive to allow published            }
{                                in classes. Added a compile directive         }
{                                around fmShareExclusive as this does not      }
{                                exist in Free Pascal                          }
{                                                                              }
{******************************************************************************}
{
  $Log: logger.pas,v $
  Revision 1.2  2006/11/26 16:58:04  savage
  Modified to create separate log files. Therefore each instance running from the same directory will have their own individual log file, prepended with a number.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}
{
  Version 1.3  2022/10/27 UJA
  modified to store in comma separated (CSV) text format for easy parsing. Also removed Jedi header dependency.
}
//{$I jedi-sdl.inc}

//{$WEAKPACKAGEUNIT OFF}

interface

{$Macro On}
{$define INFOLINE:= {$i %file%},{$i %CURRENTROUTINE%}}
uses
  Classes,
  SysUtils;


type
  TPrintLevel = (
    PRINTLEVEL_NONE = 0,
    PRINTLEVEL_ERROR = 1,
    PRINTLEVEL_WARNING = 2,
    PRINTLEVEL_ALL = 3);


  TLogger = class
  private
    FFileHandle : TextFile;
    FApplicationName : string;
    FApplicationPath : string;
    S:string;
    procedure WriteString(PrintIt:boolean; messagetype, timestamp, message, filename, FunctionName, lineNr:String);
  protected


  public
    constructor Create;
    destructor Destroy; override;
    function GetApplicationName: string;
    function GetApplicationPath: string;
    procedure LogWarning (WarningMessage:string; PrintLevel:TPrintLevel;  FileName,  FunctionName, LineNr:string);
    procedure LogStatus( StatusMessage:string; PrintLevel:TPrintLevel; FileName, FunctionName, LineNr:string);
    procedure LogError( ErrorMessage:string; PrintLevel:TPrintLevel;  FileName, FunctionName, LineNr:string);
    procedure LogWarning (WarningMessage, FileName,  FunctionName, LineNr:string);
    procedure LogStatus( StatusMessage, FileName, FunctionName, LineNr:string);
    procedure LogError( ErrorMessage, FileName, FunctionName, LineNr:string);
    procedure LogStatus( StatusMessage : string; Location : string );
    procedure LogWarning( WarningMessage : string; Location : string );
    procedure LogError( ErrorMessage : string; Location : string );

  published
    property ApplicationName : string read GetApplicationName;
    property ApplicationPath : string read GetApplicationPath;
  end;

var
  Log : TLogger;

implementation

{ TLogger }
constructor TLogger.Create;
var
  FileName : string;
  FileNo : integer;
begin
  FApplicationName := ExtractFileName( ParamStr(0) );
  FApplicationPath := ExtractFilePath( ParamStr(0) );
  FileName := FApplicationPath + ChangeFileExt( FApplicationName, '.log' );
  FileNo := 0;
  while FileExists( FileName ) do
  begin
    inc( FileNo );
    FileName := FApplicationPath + IntToStr( FileNo ) + ChangeFileExt( FApplicationName, '.log' )
  end;
  AssignFile( FFileHandle, FileName );
  ReWrite( FFileHandle );
  S := 'Messagetype, TimeStamp, Message, FileName, Function, LineNr';
  WriteLn( FFileHandle,  S );
  (*inherited Create( FApplicationPath + ChangeFileExt( FApplicationName, '.log' ),
                    fmCreate {$IFNDEF FPC}or fmShareExclusive{$ENDIF} );*)
end;

destructor TLogger.Destroy;
begin
  CloseFile( FFileHandle );
  inherited;
end;

function TLogger.GetApplicationName: string;
begin
  result := FApplicationName;
end;

function TLogger.GetApplicationPath: string;
begin
  result := FApplicationPath;
end;

//usage:
procedure TLogger.WriteString(PrintIt:boolean; messagetype, timestamp, message, filename, FunctionName,  lineNr:String);
begin
  s:=messagetype + ', ' + timestamp + ', "' + message +
       '", "' + filename + '", "' + FunctionName + '", '+ ' Line='+ lineNr;
  WriteLn( FFileHandle,  S );
  Flush( FFileHandle );
  if printit then WriteLn( S );
end;

procedure TLogger.LogWarning (WarningMessage:string; PrintLevel:TPrintLevel;  FileName,  FunctionName, LineNr:string);
begin
  writestring(printLevel >= PRINTLEVEL_WARNING, 'Warning:', TimeToStr(Time), WarningMessage, FileName, FunctionName, LineNr);
end;

procedure TLogger.LogStatus( StatusMessage:string; PrintLevel:TPrintLevel; FileName, FunctionName, LineNr:string);
begin
  writestring(printLevel >= PRINTLEVEL_ALL, 'Status:', TimeToStr(Time), StatusMessage, FileName, FunctionName, LineNr);
end;

procedure TLogger.LogError( ErrorMessage:string; PrintLevel:TPrintLevel;  FileName, FunctionName, LineNr:string);
begin
  //if printLevel >= PRINTLEVEL_ERROR then
  //  write('(print level print) ')
  //else
  //  write('(print level= ' , PrintLevel,') ');
  writestring(printLevel >= PRINTLEVEL_ERROR, 'Error:', TimeToStr(Time), ErrorMessage, FileName, FunctionName, LineNr);
end;

procedure TLogger.LogError( ErrorMessage, FileName, FunctionName, LineNr:string);
begin
  self.LogError(ErrorMessage, PRINTLEVEL_NONE, FileName, FunctionName,LineNr);
end;

procedure TLogger.LogError(ErrorMessage, Location: string);
begin
  self.logError(ErrorMessage, location, '', '-1');
end;

procedure TLogger.LogStatus( StatusMessage, FileName, FunctionName, LineNr:string);
begin
  self.LogStatus(StatusMessage, PRINTLEVEL_NONE, FileName, FunctionName,LineNr);
end;

procedure TLogger.LogStatus(StatusMessage, Location: string);
begin
  self.LogStatus(StatusMessage, Location,'','-1');
end;

procedure TLogger.LogWarning (WarningMessage, FileName,  FunctionName, LineNr:string);
begin
  self.LogWarning(WarningMessage, PRINTLEVEL_NONE, FileName, FunctionName,LineNr);
end;

procedure TLogger.LogWarning (WarningMessage, Location: string);
begin
  self.logWarning(WarningMessage, location,'', '-1');
end;

initialization
begin
  Log := TLogger.Create;
  Log.LogStatus( 'Starting Application', INFOLINE , {$i %line%});
end;

finalization
begin
  Log.LogStatus( 'Terminating Application', INFOLINE , {$i %line%});
  Log.Free;
  Log := nil;
end;

end.
 
