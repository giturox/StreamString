program object_list_testt;
uses classes, variants, TreeHandler;
//  , heaptrc;

const MAX_WIDTH = 50;
      MAX_LENGTH = 20;
      MIN_WIDTH = 10;
      MIN_LENGTH = 5;

type
  TRectangle = class(Tobject)
    private
      var _length, _width: Int64;
      Function GetLength : Int64;
      Function GetWidth : Int64;
      Procedure Setlength(l : Int64);
      Procedure SetWidth(w : Int64);
    public
      property width : Int64 Read GetWidth Write SetWidth;
      property length : Int64 Read _length Write Setlength;
      constructor create(l, w: Int64);
      destructor Destroy; override; //override is Important otherwise parent.destroy is used!!!
      function toStr:string;
    end;

TDiscoRectangle = class(TRectangle)
  private
    var _Red,_Green, _Blue: UInt16;
  public
    property red : UInt16 Read _Red Write _Red;
    property Green : UInt16 Read _Green Write _Green;
    property Blue : UInt16 Read _Blue Write _Blue;

    constructor create(l, w: Int64;  r,g,b:Uint16);
//    function toString:string; override;
  end;

function X2Str(x:variant):string;
begin
  Result:=VarToStr(x);
end;


constructor TRectangle.create(l, w: Int64);
begin
  inherited Create;
  self.length:=l;
  self.width:=w;
end;

destructor TRectangle.Destroy;
begin
  inherited destroy;
  writeln('***'+self.ClassName+' Destroyed');
end;

function TRectangle.toStr:String;
begin
//  inherited ToString;
  Result:=Self.ClassName + ' Size=' + VarToStr(self.InstanceSize) +
          ' HashCode(hex)=$' + hexStr(self.GetHashCode,8);
end;

procedure TRectangle.SetWidth(w: Int64);
begin
  if W > MAX_WIDTH then w := MAX_WIDTH;
  if W < MIN_WIDTH then w := MIN_WIDTH;
  _width:=w;
end;

procedure TRectangle.Setlength(l: Int64);
Begin
  if l > MAX_LENGTH then l := MAX_LENGTH;
  if l < MIN_LENGTH then l := MIN_LENGTH;
  _length:=l;
end;

function TRectangle.GetLength:Int64;
begin
  Result:=_length;
end;

function TRectangle.GetWidth:Int64;
begin
  Result:=_Width;
end;

constructor TDiscoRectangle.create(l, w: Int64; r,g,b:Uint16);
begin
  inherited Create(l,w);
  self._Red:=r;
  self._Green:=g;
  self._Blue:=b;
end;


var
  BoxStuff:TRectangle;
  DiscoBoxStuff:TDiscoRectangle;
  DiscoBoxStuff2:TDiscoRectangle;

  ObjectList:TList;

  idx:integer;


begin
  Randomize;
  //BoxStuff:=TRectangle.create(40,50);
  //writeln('Width = ',BoxStuff.width);
  //writeln('length = ',BoxStuff.length);
  //
  //BoxStuff.width:=100;
  //BoxStuff.length:=0;
  //writeln('Width = ',BoxStuff.width);
  //writeln('length = ',BoxStuff.length);
  //writeln(BoxStuff.ToStr);
  //Boxstuff.free;
  //
  //DiscoBoxStuff:=TDiscoRectangle.create(40,50, 12, 128, 254);
  //DiscoBoxStuff.width:=100;
  //DiscoBoxStuff.length:=0;
  //writeln('*DISCO* Width  = ',DiscoBoxStuff.width);
  //writeln('*DISCO* length = ',DiscoBoxStuff.length);
  //writeln(DiscoBoxStuff.toStr);
  //
  //DiscoBoxStuff2:=TDiscoRectangle.create(40,50, 12, 128, 254);
  //DiscoBoxStuff2.width:=100;
  //DiscoBoxStuff2.length:=0;
  //writeln('*DISCO* Width  = ',DiscoBoxStuff2.width);
  //writeln('*DISCO* length = ',DiscoBoxStuff2.length);
  //writeln(DiscoBoxStuff2.toStr);
  //DiscoBoxStuff2.Free;
  //DiscoBoxStuff.Free;
  //
  //ObjectList:=TList.Create;
  //
  //for idx:=0 to 3 do
  //begin
  //  DiscoBoxStuff:=TDiscoRectangle.create(random(20),random(60), random(255), random(255), random(255));
  //  ObjectList.Add(DiscoBoxStuff);
  //end;
  //
  //for idx:=0 to ObjectList.Count-1 do
  //begin
  //  writeln('Width=',TDiscoRectangle(ObjectList[idx]).width,
  //          ' Length=',TDiscoRectangle(ObjectList[idx]).length);
  //  writeln(TDiscoRectangle(ObjectList[idx]).toStr);
  //end;
  //
  ////take care off items you have to the list
  //for idx:=ObjectList.Count-1 downto 0 do
  //begin
  //  write('<Free ',idx,'> ');
  //  TDiscoRectangle(ObjectList[idx]).Free;
  //end;
  //
  //ObjectList.Clear;
  //ObjectList.Free;

  test_tree(True);
  Write('Done press return:');
  readln;
end.


