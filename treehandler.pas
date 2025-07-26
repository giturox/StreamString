unit TreeHandler;

{$define unit_test}
{$define write_to_console}


{$mode objfpc}
interface

uses
{$ifdef unit_test}
   loggerV2,
{$endif}
   classes, sysutils, StrUtils, StringConverter;


const
  MAX_TREE_SIBLING_WIDTH = 1024*1024*1024; //1 Giga
  MAX_TREE_CHILD_DEPTH   = 1024*1024*1024; //1 Giga

Type
  TDirection=(DirLeft,DirRight,DirUp,DirDown);
  TTreeError=(TreeNoError, TreeMemoryError,TreeStructureIncosistent, TreeEmpty,
              TreePayloadError, TreeInternalEraseError, TreeInternalSearchError);

                         // ObjectOk = OK (noError)   ,
                         // ObjectWarning = Warning (read objecterrorcode for details)
                         // ObjectErrorNonCritical= RecoverableError (call object recovery function to recover
                         // ObjectErrorCritical = Cannot recover from error (but you may try to save a recover file)
                         // ObjectTotalFailure = Cannot recover from error and avoid any further use of object
  TObjectInternalState = (ObjectOk, ObjectWarning, ObjectErrorNonCritical, ObjectErrorCritical, ObjectTotalFailure);


  //Type
  //  mylist  = class(TObject);
  //  generic TGenList<_T>=class(TObject)
  //  Public
  ////    type
  ////       TCompareFunc = function(const Item1, Item2: _T): Integer;
  //    var
  //      items : array of _T;
  //    procedure Add(new_item: _T);
  //    procedure list;
  //  end;
  //
  //
  //
  //procedure TList.Add(new_item: _T);
  //beginnew
  //  SetLength(Items, Length(Items) + 1);
  //  Items[Length(Items) - 1] := new_item;
  ////  WriteLn(new_item);
  //end;
  //
  //procedure TList.list;
  //var i:integer;
  //begin
  //  for i:=0 to Length(Items)-1 do
  //    writeln('List[',i,']');
  //end;
  //
  //Type
  //  TDiscoBoxList = specialize TFPGList<TDiscoRectangle>;
  //  TStructList = specialize Tlist<TStruct>;
  //  TFPClist = specialize TFPGList<Tstruct>;
  //
  //var
  //  ListInstance: TIntList;
  //  StructInstance: TFpcList;
  //  items_to_add:array [1..10] of TStruct;
  //  idx:Integer;

  //var
  //  BoxStuff:TRectangle;
  //  DiscoBoxStuff:TDiscoRectangle;
  //
  ////  genericslist:TDiscoBoxList;
  //  ObjectList:TList;

  //The payload here stores all items as strings in a single large array and these referenced by their indices
  TModule = Record
    x,y:Real;                 //Coordinates
    width,height:Real;        //Width of children underneath

    Selected:Boolean;         //Is the square selected by user
    Folded:boolean;
    direction:TDirection;     //What way are user looking?
    loop:Boolean;             //Is this a square with a loop?
    selection:Boolean;        //Is this a sqaure with an if/case construct?

    ///Following items are index to a string array
    LoopText_idx            : uint32;     //Loop belongs to parent
    SelectionText_idx       : uint32;     //Selectiontext belongs to children
    DocShort_idx            : uint32;     //Generated Doxygen Short description
    Parameters_idx          : uint32;     //Generated Doxygen parameters description
    PlantUMLDiagramCode_idx : uint32;     //Text for PlantUML diagrams to be embedded in Doxygen comments
  end;

  p_TBlock = ^TBlock;
  TBlock = Record
    {$ifdef unit_test}
    DEBUG_blockid1:uint32;
    DEBUG_blocknr:uint32;
    {$endif}
    Next,Prev:Uint32;       //Next/previous in the double-chain (uses index instead of pointers)
    Parent,Child:Uint32;    //Parent (if any) and first-child (if any children) (uses index instead of pointers)

    //id:Uint32; //
    Payload_Idx: Uint32; //Idx = 0 is not used -> means empty item. The item at idx=0 points to the next available item.
                         //That item will then containt the idx of the next available item.
                         //
  end;

//  TStringList = Array of TOneCharString;  //Stores ALL strings
//  TBlockArray = ;
//  TBlockArray = class (Specialize TGenArray<TBlock>)
  //Function ItemToStreamString(item: TBlock):TOneCharString;  override;
  //
  //end;

  TModuleList = Array of TModule;

  TBlockList = Array of TBlock;


  //p_TBlock= ^TBlock;
  //TBlock = class(TObject)
  //  {$ifdef unit_test}
  //  DEBUG_blockid1:uint32;
  //  DEBUG_blocknr:uint32;
  //  {$endif}
  //  BlockLabel:String;
  //  Next,Prev:TBlock;       //Next/previous in the double-chain
  //  Parent,Child:TBlock;    //Parent (if any) and first-child (if any children)
  //  direction:TDirection;     //What way is the user looking?
  //  selected:boolean;
  //  folded:Boolean;
  //  payload_idx:integer;
  //  InternalState: TObjectInternalState;
  //Public
  //  constructor Create;
  //  destructor destroy; override;
  //  {$ifdef unit_test}
  //  function CheckBlock:boolean;
  //  {$endif unit_test}
  //End;

  {$ifndef usetree}
   implementation
   end.
  {$else}


  TTreeHandler = Class(TObject)
    Private
      { Private declarations \}
      changed: boolean;
      //*
      //sizefactor,
      //BoxWidth,
      //BoxHeight,
      //VerticalSpacing,
      //HorisontalSpacing:Real;
      initialized:boolean;

      nodes_in_tree:UInt64;

      cursor_idx,
      Root_idx:Uint32;

      Tree: TBlockList;
      Items: TItemList;
      STringlist: TStringList;

      _treeChanged:Boolean;


      procedure IncreaseNodeCount;
      procedure DecreaseNodeCount;

      Procedure AddToEnd(current,NewSibling:TBlock);
      Procedure InsertAfter(current,NewSibling:TBlock);
      Procedure InsertBefore(current,NewSibling:TBlock);
      Procedure InitCurrent(current:TBlock);
      Function CreateRoot(name:String):Boolean;

    Public
      { Public declarations }
      property TreeChanged : boolean Read _treeChanged Write _treeChanged;
      property NodesInTree : UInt64 Read nodes_in_tree;
      property IsInitialized : boolean Read initialized;

      constructor Create(name:String);
      destructor destroy; override;


      Function AddChildToEnd(Parent:TBlock; name:String):TBlock;
      Function InsertChild(Parent:TBlock; name:String; pos:integer):TBlock;
      Function Insert(Parent,Current:TBlock; name:String):TBlock;
      Function Remove(Current:TBlock):Boolean;
      Function FindFirst(current:TBlock):TBlock;
      Function FindLast(current:TBlock):TBlock;
      Function FindMiddle(current:TBlock):TBlock;
      Procedure RemoveChildren(current:TBlock);

//      ***Modify to use functions from old recursive diagram handler function

      //Procedure InitCurrent(current:TBlock);
      function ValidMoveDestination(to_block:TBlock):boolean;
      function MoveSubTree(from_block, to_block:TBlock):boolean;
      function AddPayload(block:TBlock; payload:pointer):integer;
      function RemovePayload(block:TBlock; payload:pointer):Boolean;

      //function GetCurrent:TBlock;
      Function InsertInDirection(Current:TBlock; name:String;
                                 direction:TDirection):TBlock;
//      Procedure SetSelect(state:boolean);
      Procedure SetSelect(startblock:TBlock; state:boolean);

      Procedure SetCursor(NewPointer:TBlock);
      procedure GoUp;
      Procedure GoDown;
      Procedure GoLeft;
      Procedure GoRight;
      function CheckBlockinDirection(current:TBlock; Direction:TDirection):Tblock;
      function CheckBlockinDirection(Direction:TDirection):Tblock;
      function GoInDirection(current:TBlock; Direction:TDirection):Tblock;
      function GoInDirection(Direction:TDirection):Tblock;
      procedure SetDirUp;
      Procedure SetDirDown;
      Procedure SetDirLeft;
      Procedure SetDirRight;
      //procedure SetDragSource(current:TBlock; state:boolean);
      //procedure SetDragDest  (current:TBlock; state:boolean);
      //Function  FindHit(x,y:real):TBlock;
      //Procedure SetScale(newsize:Real);
      //Function  GetScale:Real;
      //procedure ToggleFolded;
      //Procedure UpdateTreeGraphics;
      //Procedure DrawTree(DrawCanvas:TCanvas; printing:boolean);
      //Function  has_diagram_changed:boolean;
      //procedure set_diagram_changed;

      //Function  SaveToFile(filnavn:String):boolean;
      //Function  ActivityDiagramToFile(filnavn:string):boolean;
      //Function  ReadFromFile(filnavn:String):boolean;
      Function  TreeToTextFile(filnavn:string):boolean;
      {$ifdef unit_test}
      function ValidateBlockitem(idx:uint32):boolean;
      {$endif unit_test}

  End; //Class

{$ifdef unit_test}
function test_tree(success:boolean):boolean;
{$endif unit_test}

implementation

{$ifdef unit_test}
const
  MAGIC_NUMBER_42 = $5AC396F0;
{$endif}

{$ifdef unit_test}
function TTreeHandler.ValidateBlockitem(idx:uint32):boolean;
begin
  if self.Tree[idx].DEBUG_blockid1 <> MAGIC_NUMBER_42 then
  begin
    Result:= False;
  end
  else
    Result := True;
end;
{$endif}

//Constructor TBlock.Create;
//begin
//  {$ifdef unit_test}
//  self.DEBUG_blockid1:=MAGIC_NUMBER_42;
//  {$endif}
//  self.Child:=NIL;
//  self.Next:=NIL;
//  self.Prev:=NIL;
//  self.Parent:=NIL;
//  self.BlockLabel:='';
//  self.direction:=DirDown;
//  self.selected:=False;
//  self.folded:=False;
//  self.payload_idx:=-1;
//end;

//destructor TBlock.destroy;
//begin
//
//  inherited destroy;
//end;
//Function TDiagramhandler.CreateRoot(name:String):Boolean;
//begin
////  inherited;
//end;

constructor TTreehandler.Create(name:String);
begin
//  inherited Create;
  initialized:=CreateRoot(name);
end;

Function TTreehandler.CreateRoot(name:String):Boolean;
const
  Start_Idx=1;
//Var return:Boolean;
Begin
  self.root:=Start_Idx;
  self.cursor:=Start_Idx;
  Result:=False;
  nodes_in_tree:=0;

  try
//    New(Root);
    Root:=TBlock.Create;
    InitCurrent(root);
    Root.BlockLabel:=name;
//    clear_diagram_changed;
    initialized:=True;
    TreeChanged:=False;
    cursor:=root;
    cursor.direction:=DirDown;
    Result:=True;
    IncreaseNodeCount;
  Except
    Result:=False;
  end;

  //Sizes are in millimeters
  //PageWidth:=420.2;         //A3 landscape
  //PageHeight:=297.0;        //-"-
  //SetSCale(3.0);

End;

procedure TTreehandler.IncreaseNodeCount;
begin
  if self.nodes_in_tree < MaxInt then
  begin
    self.nodes_in_tree += 1;
  end;
end;

procedure TTreehandler.DecreaseNodeCount;
begin
  if self.nodes_in_tree > 1 then
  begin
    self.nodes_in_tree -= 1;
  end;
end;

Procedure TTreehandler.InitCurrent(current:TBlock);
Begin
  {$ifdef unit_test}
  current.DEBUG_blockid1:=MAGIC_NUMBER_42;
  current.DEBUG_blocknr:= self.nodes_in_tree+1;
    {$ifdef write_to_console}
    write('[Init(',current.DEBUG_blocknr,')] ');
    {$endif}
  {$endif}
  current.Child:=NIL;
  current.Next:=NIL;
  Current.Prev:=NIL;
  Current.Parent:=NIL;
  current.BlockLabel:='';
  Current.direction:=DirDown;
  Current.selected:=False;
  Current.folded:=False;
  current.payload_idx:=-1;
End;

destructor TTreeHandler.destroy;
begin
  cursor:=Nil;
  FreeAndNil(root);
  inherited destroy;
//  dispose(root);
end;

{********************************************************************
Description: Adds a new sibling to the end of a list of TBlocks     *
Inputs:                                                             *
  <current> = Current sibling to which list we will add a sibling   *
  <NewSibling> = new sibling to add                                 *
********************************************************************}
Procedure TTreehandler.AddToEnd(current,NewSibling:TBlock);
var
    count : UInt32;
Begin
  count := 0;
  While current.Next<>NIL Do //Continue until end of list
  Begin
    count := count + 1;
    if count > MAX_TREE_SIBLING_WIDTH then exit;
    current:=current.Next;
  End; //while
  Current.Next:=NewSibling; //Add new sibling to end of list
End;

{********************************************************************
Description: Inserts a new sibling after current                    *
Inputs:                                                             *
  <current> = Current sibling to which list we will add a sibling   *
  <NewSibling> = new sibling to add                                 *
********************************************************************}
Procedure TTreehandler.InsertAfter(current,NewSibling:TBlock);
Var next:TBlock;
Begin
  if current<>NIL Then
  Begin
    next:=current.Next;
    Current.Next:=NewSibling;
    NewSibling.Parent:=current.Parent;
    NewSibling.Prev:=current;
    NewSibling.Next:=next; //If current is at the end of the list then next will
                            //be NIL
    If next<>NIL Then
    Begin
      next.Prev:=NewSibling;
    End
    Else
    Begin
      //Do nothing
    End; //else
  End
  Else //It is not so much an insert as an init. of NewSibling
  Begin
    NewSibling.Prev:=NIL;
    NewSibling.Next:=NIL;
  End; //else
End;

{********************************************************************
Description: Inserts a new sibling Before current                   *
Inputs:                                                             *
  <current> = Current sibling to which list we will add a sibling   *
  <NewSibling> = new sibling to add                                 *
********************************************************************}
Procedure TTreehandler.InsertBefore(current,NewSibling:TBlock);
Var prev:TBlock;
Begin
  if current<>NIL Then
  Begin
    try
      prev:=current.Prev;
      Current.prev:=NewSibling;
      NewSibling.Prev:=prev; //If current is at the end of the list then
                            //prev will be NIL
      NewSibling.Next:=Current;

      NewSibling.Parent:=Current.Parent;
    Except
      {$ifdef write_to_console}
      writeln('exception at A in InsertBefore!');
      {$endif}
    End;
    try
      if NewSibling.Parent <> NIL then
        NewSibling.Parent.Child:=findfirst(current);
    Except
      writeln('exception at B in InsertBefore!');
    End;
    try
      If prev<>NIL Then
      Begin
        prev.Next:=NewSibling;
      End
      Else
      Begin
        //Do nothing
      End;
    Except
      {$ifdef write_to_console}
      writeln('exception at C in InsertBefore!');
      {$endif}
    End;
  End;
End;




{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.AddChildToEnd(Parent:TBlock; name:String):TBlock;
Var return:TBlock;
    child:TBlock;
Begin
  //{$ifdef unit_test}
  //if parent <> Nil then parent.c
  //{$endif}

  Try
    //New(child);
    child:=TBlock.Create;
    InitCurrent(child);
    Child.BlockLabel:=name;
    Child.Parent:=Parent; //It has a parent
    Child.Child:=NIL;     //Still no children
    Child.next:=NIL;      //It will be attached to evt. siblings
    Child.Prev:=NIL;
    IncreaseNodeCount;

    If Parent.Child=NIL Then //If parent had no children before do a simple add
    Begin
      Parent.Child:=Child;
    End
    Else //Insert new child at end of the line of children
    Begin
      AddToEnd(Parent.Child,Child);
    End; //if
    Return:=Child;
  Except
    Return:=NIL;
  End; // try
  AddChildToEnd:=Return;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.InsertChild(Parent:TBlock; name:String;
                                     Pos:Integer):TBlock;
Var
  return:TBlock;
  current,
  child:TBlock;
  i:Integer;

Begin
  Try
    //New(child);
    Child:=TBlock.Create;
    InitCurrent(child);
    Child.BlockLabel:=name;
    Child.Parent:=Parent; //It has a parent
    Child.Child:=NIL;     //Still no children
    Child.next:=NIL;      //It will be attached to evt. siblings
    Child.Prev:=NIL;
    IncreaseNodeCount;

    If Parent.Child=NIL Then //If parent had no children before do a simple add
    Begin
      Parent.Child:=Child;
    End
    Else //Insert new child in the line of children at pos
    Begin
      If pos<=0 Then
      Begin
        InsertBefore(Parent.Child,Child);
      End
      Else //pos<>0
      Begin
        i:=1;
        Current:=Parent.Child;
        While (i<pos) AND (Current.Next<>NIL) Do //Continue until position OR
                                                  //end-of-list is found
        Begin
          Current:=Current.Next;
          i:=i+1;
        End; //while
        InsertAfter(current,child);
      End; //if
    End; //if
    Return:=child;
  Except
    Return:=NIL;
  End; // try
  InsertChild:=Return;
End;

Function TTreehandler.Insert(Parent,Current:TBlock; name:String):TBlock;
var Child,Retur:TBlock;
Begin
  Retur:=NIL;
  If (Current<>NIL) OR (Parent<>NIL) Then
  Begin
    Try
      //New(child);
      Child:=TBlock.Create;
      InitCurrent(child);
      Child.BlockLabel:=name;
      if parent=NIL Then
      Begin
        Parent:=Current.Parent; //They have the same parent
      End;
      Child.Parent:=Parent; //It has a parent
      Child.Child:=NIL;     //Still no children
      Child.next:=NIL;      //It will be attached to evt. siblings
      Child.Prev:=NIL;
      IncreaseNodeCount;

      If Parent.Child=NIL Then //If parent had no children before do a simple add
      Begin
        if current<>NIL Then
        Begin
          {$ifdef write_to_console}
          //Writeln('Til alle andre: Programmet fortsætter og Uffe ');
          //Writeln('undskylder fejlen. ');
          Writeln('Det er en god ide at gemme NU under et ANDET filnavn ');
          Writeln('og genstarte programmet!!! ');
          Writeln('Til Uffe: Der er en fejl i programmet. ');
          Writeln('Treehandler.insert : Parent^.child=0 og current != 0!');
          {$endif}
          //ShowMessage('Til alle andre: Programmet fortsætter og Uffe '+
          //            'undskylder fejlen. '+
          //            'Det er en god ide at gemme NU under et ANDET filnavn '+
          //            'og genstarte programmet!!! '+
          //            'Til Uffe: Der er en lille fejl i programmet. '+
          //            'Diagramhandler.insert:Parent^.child=0 og current != 0!');
        End;
        Parent.Child:=Child;
      End
      Else //Insert new child in the line of children at pos
      Begin
        if current=NIL Then
        Begin
{          temp:=parent^.Child;
          temp^.Prev:=Child;
          Child^.Next:=Temp;
          parent^.Child:=child;}
          InsertAfter(FindLast(parent.Child),child);
        End
        Else
        Begin
          insertAfter(current,child);
        End;
      End; //if
      Retur:=child;
    Except
      Retur:=NIL;
    End; // try
  End;
  insert:=Retur;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.InsertInDirection(Current:TBlock; name:String;
                                           direction:TDirection):TBlock;
var scanner,newblock, prev, next:TBlock;
Begin
//  retur:=NIL;
  If current<>NIL Then
  Begin
    Case direction of
      DirLeft:
      Begin
        Try
          //New(newBlock);
          newBlock:=TBlock.Create;
          InitCurrent(NewBlock);
        Except
          {$ifdef write_to_console}
          writeln('exception at A in DirLeft!');
          {$endif}
          NewBlock:=NIL;
        End;
        Try
          NewBlock.BlockLabel:=name;
          InsertBefore(current,newBlock);
        Except
          {$ifdef write_to_console}
          writeln('exception at B in DirLeft!');
          {$endif}
          NewBlock:=NIL;
        End;
        Try
          IncreaseNodeCount;
          Cursor:=newblock;
        Except
          {$ifdef write_to_console}
          writeln('exception at C in DirLeft!');
          {$endif}
          NewBlock:=NIL;
        End;
      End;
      DirRight:
      Begin
//        NewBlock:=InsertAfter(NIL,Current,name);
        newBlock:=TBlock.Create;
        InitCurrent(NewBlock);
        NewBlock.BlockLabel:=name;
        InsertAfter(current,NewBlock);
        IncreaseNodeCount;
        Cursor:=newblock;
      End;
      DirUp:
      Begin
        Try
          //New(newBlock);
          newBlock:=TBlock.Create;
          InitCurrent(NewBlock);
          prev:=current.Parent;
          NewBlock.Child:=current;
          NewBlock.Parent:=Current.Parent;
          NewBlock.Next:=Current.Next;
          NewBlock.Prev:=Current.Prev;
          Current.Next:=NIL;
          Current.Prev:=NIL;
          IncreaseNodeCount;

          If NewBlock.Next<>NIL Then NewBlock.Next.Prev:=Newblock;
          If NewBlock.Prev<>NIL Then NewBlock.Prev.Next:=Newblock;
          NewBlock.Child.Parent:=Newblock;
          NewBlock.Parent.Child:=FindFirst(newblock);
          NewBlock.BlockLabel:=name;
          Cursor:=newblock;
        Except
          NewBlock:=NIL;
        End;
      End;
      DirDown:
      Begin
        Try
          //New(newBlock);
          newBlock:=TBlock.Create;
          InitCurrent(NewBlock);
          IncreaseNodeCount;

          NewBlock.Child:=FindFirst(current.Child); // Make sure tree is coherent
          NewBlock.Parent:=Current;

          Current.Child:=Newblock;
          NewBlock.BlockLabel:=name;

          scanner:=NewBlock.Child; //Give new parent
          While scanner<>NIL do
          Begin
            scanner.Parent:=newblock;
            scanner:=scanner.Next;
          End;
        Except
          NewBlock:=NIL;
        End;
      End;
    End;
  End;
  if newblock <> NIL then
  begin
    self.cursor:=newblock;
  end
  else
  begin
    {$ifdef write_to_console}
    write('***NeWblock in insertindirection == Nil ***');
    {$endif}
  end;
  InsertInDirection:=newblock;
End;

{********************************************************************
Description: Finds first item on the same level                     *
Inputs: Current block                                               *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.FindFirst(current:TBlock):TBlock;
Begin
  try
  FindFirst:=Current;

  if current<>NIL Then
  Begin
    {$ifdef write_to_console}
    write(' ~!~ ');
    {$endif}
    While (Current.Prev<>NIL) do
    Begin
      Current:=Current.Prev;
      {$ifdef write_to_console}
      write(' <<-~ ');
      {$endif}
    End;
    {$ifdef write_to_console}
    write(' ~v~ ');
    {$endif}
  End
  Else
  Begin
    {$ifdef write_to_console}
    write(' ~o~ ');
    {$endif}
    //Do nothing, since current is nil
  End;
  except
    {$ifdef write_to_console}
    writeln('OBJECTION findfirst!');
    {$endif}
  end;
  {if current<>NIL Then
    ShowMessage('Current<>NIL')
  Else
    ShowMessage('Current=NIL');}


  {$ifdef write_to_console}
  if current=NIL Then writeln('Findfirst:Current=NIL');
  {$endif}
  FindFirst:=Current;
End;


{********************************************************************
Description: Finds first item on the same level                     *
Inputs: Current block                                               *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.FindMiddle(current:TBlock):TBlock;
Var count,middle:integer;
    first:TBlock;
Begin
  if current<>NIL Then
  Begin
    While (Current.Prev<>NIL) do
    Begin
      Current:=Current.Prev;
    End;
    first:=Current;
    //Now we are standing at the start and now we count til we reach the end
    count:=0;
    While (Current.Next<>NIL) do
    Begin
      Current:=Current.Next;
      count:=count+1;
    End;
    middle:=round(count/2);
    count:=0;
    Current:=first;
    While ((Current.Next<>NIL) AND (count<middle)) do
    Begin
      Current:=Current.Next;
      count:=count+1;
    End;

  End
  Else
  Begin
    //Do nothing, since current is nil
  End;
  {if current<>NIL Then
    ShowMessage('Current<>NIL')
  Else
    ShowMessage('Current=NIL');}

  FindMiddle:=Current;
End;

{********************************************************************
Description: Finds last item on the same level                     *
Inputs: Current block                                               *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.FindLast(current:TBlock):TBlock;

Var count:integer;
    first:TBlock;
Begin
  if current<>NIL Then
  Begin
    count := 0;
    While (Current.Next<>NIL) AND (Count < MAX_TREE_SIBLING_WIDTH) do
    Begin
      {$ifdef write_to_console}
      write('(FINDLAST -> )');
      {$endif}
      Current:=Current.Next;
      Count += 1;
    End;
  End
  Else
  Begin
    //Do nothing, since current is nil
  End;

  FindLast:=Current;
End;

//{********************************************************************
//Description:                                                        *
//Inputs:                                                             *
//Outputs:                                                            *
//********************************************************************}
//Function TTreehandler.FindLast(current:TBlock):TBlock;
//Begin
//  if current<>NIL Then
//  Begin
//    While (Current.Next<>NIL) do
//    Begin
//      Current:=Current.Next;
//    End;
//  End
//  Else
//  Begin
//    //Do nothing, since current is nil
//  End;
//  FindLast:=Current;
//End;


{********************************************************************
Description: Removes all children to current                        *
Inputs: current element, whose children are to be removed           *
Outputs: *None*                                                     *
********************************************************************}
Procedure TTreehandler.RemoveChildren(current:TBlock);
const
  GO_UP = 1;
  GO_LEFT = 2;
  GO_RIGHT = 3;
  GO_NO_WHERE = -1;

  function WhereTo2Str(where:integer):string;
  begin
    Result:='<unknown direction>';
    case where of
      GO_LEFT: Result:='<GO_LEFT>';
      GO_RIGHT: Result:='<GO_RIGHT>';
      GO_UP: Result:='<GO_UP>';
      GO_NO_WHERE: Result:='<GO_NO_WHERE>';
    end;
  end;
//function GoToFarthestDown(Current:TBlock; GeneralDirection:TDirection):TBlock;
//var lookdown, lookside:TBlock;
//    foundit:Boolean;
//begin
//  Result:=Current;
//  if current<>NIL Then
//  Begin
//    if (GeneralDirection = DirUp)   or
//       (GeneralDirection = DirDown) then
//    Begin
//      GeneralDirection:= DirRight; //If wrong direction is chosen a meaningful choice is enforced
//    end;
//    lookdown := CheckBlockinDirection(current, DirDown);
//    lookside := CheckBlockinDirection(current, GeneralDirection);
//  end;
//end;
//
//Var curr,next:TBlock;
//Begin
//  if current<>NIL Then
//  Begin
//    If current.Child<>NIL Then
//    Begin
//      curr:=current.Child;
//      Current.Child:=NIL;
//      While (curr<>NIL) Do
//      Begin
//        next:=curr.Next;
//        If curr.Child<>NIL Then
//        Begin
//          try
//            RemoveChildren(curr);
//          except
//
//          end;
//        End;
//        FreeandNil(curr);
//        DecreaseNodeCount;
//        curr:=next;
//      End;
//    End;
//  End;
//End;

{$ifdef unit_test}
procedure DumpStatus(current:TBlock);
begin
  write ('<# nodes:',self.NodesInTree, '>');
  if current <> Nil then
  begin
    write ('Current node (', current.DEBUG_blocknr,'):',
             ' parent=',current.Parent <> Nil,
             ' child=',current.Child <> Nil,
             ' prev=',current.prev <> Nil,
             ' next=',current.next <> Nil
             );
    if current.DEBUG_blockid1 = MAGIC_NUMBER_42 then
      writeln('- block is valid.')
    else
      writeln(' *** block is NOT valid!!! ***');
  end
  else
  begin
    write ('Current = Nil');
  end;
end;
{$endif}

{$ifdef unit_test}
procedure NodeFreeandNil(current:TBlock);
begin
  if (current <> Nil) then
  begin
    if (current.DEBUG_blockid1 = MAGIC_NUMBER_42) then
    begin
      {$ifdef write_to_console}
      write('* Trying to free block nr (', current.DEBUG_blocknr,') *');
      {$endif}
      current.DEBUG_blockid1:=0; //allows a check to see if we try to free already freed block
      FreeandNil(current);
    end
    else
    begin
      {$ifdef write_to_console}
      writeln('Trying to free already freed block???');
      {$endif}
      readln;
    end;
  end
  else
  begin
    {$ifdef write_to_console}
    writeln('Trying to free nil pointer???');
    {$endif}
  end;
end;
{$else}
procedure NodeFreeandNil(current:TBlock);
begin
  FreeandNil(current);
end;
{$endif}

Var curr,next:TBlock;
Begin
  if current<>NIL Then
  Begin
    If current^.Child<>NIL Then
    Begin
      curr:=current^.Child;
      Current^.Child:=NIL;
      While (curr<>NIL) Do
      Begin
        next:=curr^.Next;
        If curr^.Child<>NIL Then
        Begin
          RemoveChildren(curr);
        End;
        Dispose(curr);
        curr:=next;
      End;
    End;
  End;
End;

///// "current" is changed to the node one level down (first child)
///// Idea is to handle the tree structure as with a writing cursor
//function StepDown(current:TBlock; var depth_counter:Int32):TBlock;
//begin
//  if current <> Nil then
//  begin
//    if current.child <> Nil then
//    begin
//      current:=current.child;
//      depth_counter += 1;
//    end;
//  end;
//  Result:=current;
//end;
//
///// tries to find a child node furthest to the right as low down as possible
///// (warning bad ascii graphics!)
/////
/////  Start->  ##
/////         / | \
/////       ## ## ## <- Goal
/////      /   /\
/////    ##  ## ##
//function go_to_rightmost_lowest_child(Current:TBlock; var Unreachable:Boolean; var depth_counter:int32):TBlock;
//var found_end:boolean;
//
//begin
//  {$ifdef write_to_console}
//  write('<GTRLC W:start> ');
//  {$endif}
//  Unreachable := True;
//  Result:=Current;
//  //depth_counter := depth_counter
//  current:= FindLast(current); //make sure we start furthest to the right (siblings)
//  if current <> NIL then
//  begin
//    if current.Child <> Nil then
//    begin
//      Current:=StepDown(Current, depth_counter);
//      {$ifdef write_to_console}
//      DumpStatus(current);
//      {$endif}
//      found_end := False;
//      while (not found_end) and (depth_counter < MAX_TREE_CHILD_DEPTH) do
//      begin
//        current:= FindLast(current);
//        {$ifdef write_to_console}
//        write('<GTRLC F:StepDown> ');
//        DumpStatus(current);
//        {$endif}
//
//        if current.child = Nil then
//        begin
//          {$ifdef write_to_console}
//          write('<GTRLC W:FoundEnd> ');
//          if current.Next <> Nil then writeln('***GTRLC ERROR findlast failed!');
//          {$endif}
//          Unreachable := False;
//          found_end := True;
//        end
//        else
//        begin
//          {$ifdef write_to_console}
//          write('<GTRLC W:StepDown> ');
//          {$endif}
//          Current:=StepDown(Current, depth_counter);
//        end;
//      end;
//    end;
//  end;
//  {$ifdef write_to_console}
//  write('<GTRLC result> ');
//  DumpStatus(current);
//  {$endif}
//  Result:=Current;
//end;
//
///// @param current is changed to the node one level up (parent)
///// @param ReachedOriginal depth
///// Idea is to handle the tree structure as with a writing cursor
//function StepUp_and_goto_child(current:TBlock; var depth_counter:Int32; var ReachedOriginalDepth:boolean):TBlock;
//var unreachable:boolean;
//    startpoint:TBlock;
//begin
//  ReachedOriginalDepth:=False;
//  unreachable:=False;
//  startpoint := current;
//  if current <> Nil then
//  begin
//    {$ifdef write_to_console}
//    write('<SteUp: current <> Nil>');
//    {$endif}
//    if (current.Parent <> Nil) and (depth_counter > 1) then
//    begin
//      {$ifdef write_to_console}
//      write('<SteUp: current.parent <> Nil> and depth >1');
//      {$endif}
//      current:=current.Parent;
//      {$ifdef write_to_console}
//      if current = startpoint then write ('*** Stepup: parent = child ? ***');
//      {$endif}
//      depth_counter -= 1;
//      if depth_counter <=0 then
//      begin
//        ReachedOriginalDepth := True;
//        {$ifdef write_to_console}
//        writeln('*** Reached original depth');
//        {$endif}
//      end
//      else
//      begin
//        {$ifdef write_to_console}
//        write('<SteUp: call GTRLC');
//        {$endif}
//        current:=go_to_rightmost_lowest_child(current,unreachable,depth_counter);
//        {$ifdef write_to_console}
//        if current = startpoint then
//        begin
//          writeln('***StepUp: current=startpoint -> This may be bad ***');
//          //readln;
//        end;
//        {$endif}
//
//      end;
//    end
//    else
//    begin
//      {$ifdef write_to_console}
//      writeln('*** parent is nil or depth <= 1 ***');
//      {$endif}
//    end;
//  end;
//  Result:=Current;
//end;
//
//function StepLeft(current:TBlock):TBlock;
//begin
//  if current <> Nil then
//  begin
//    if (current.Prev <> Nil)  then
//    begin
//      current:=current.Prev;
//    end;
//  end;
//  Result:=Current;
//end;
//
//function StepRight(current:TBlock):TBlock;
//begin
//  if current <> Nil then
//  begin
//    if (current.Next <> Nil)  then
//    begin
//      current:=current.Next;
//    end;
//  end;
//  Result:=Current;
//end;
//
///// Erases current block and returns next usefull block.
///// It gives an error if this block has any children or a next sibling (rightside)
//function EraseCurrent(var current:TBlock;var depth_counter:Int32;
//                      var Reached_Original_Depth:boolean;
//                      var where_to_next:integer ):TBlock;
//
//Var //where_to_next:integer;
//    unreachable:boolean;
//    next, block_to_delete:TBlock;
//Begin
//  where_to_next:=GO_NO_WHERE;
//  next:=Nil;
//  unreachable:=false;
//  block_to_delete := NIL;
//  if current<>NIL Then
//  Begin
//    If (current.Child=NIL) and (current.next = NIL) Then
//    Begin
//      if current.Prev <> Nil then
//      begin
//        block_to_delete:=current;
//        //current.Prev.Next:=Nil;
//        next:=StepLeft(current);
//        {$ifdef write_to_console}
//        if (next.DEBUG_blockid1 <> MAGIC_NUMBER_42) then write('***next left is wrong! ');
//        {$endif}
//        next.Next:=Nil;
//        where_to_next:=GO_RIGHT;
//        next:= go_to_rightmost_lowest_child(next, unreachable,depth_counter);
//        {$ifdef write_to_console}
//        if (next.DEBUG_blockid1 <> MAGIC_NUMBER_42) then write('***next left gtrmc is wrong! ');
//        writeln('Erase current: GO_LEFT');
//        DumpStatus(next);
//        {$endif}
//      end
//      else
//      begin
//        block_to_delete := current;
//{$ifdef oldver}
//        if current.parent <> Nil then
//        begin
//          current.Parent.Child:=Nil;
//          current := current.parent;
//          next:=StepUp_and_goto_child(Current, depth_counter, Reached_Original_Depth);
//        end
//        else
//        begin
////          current:=Nil;
//          writeln('No more tree !!!!');
//          block_to_delete := NIL; // Do not erase root
//          Reached_Original_Depth:=True;
//          next:=current;
//        end;
//{$else}
//        if current.parent <> Nil then
//        begin
//          current.Parent.Child:=Nil;
//          current := current.parent;
//          depth_counter:=depth_counter-1;
//          next:=StepUp_and_goto_child(Current, depth_counter, Reached_Original_Depth);
//        end
//        else
//        begin
//        //          current:=Nil;
//          {$ifdef write_to_console}
//          writeln('No more tree !!!!');
//          {$endif}
//          block_to_delete := current; // erase root
//          Reached_Original_Depth:=True;
//          next:=current;
//        end;
//
//{$endif}
//
//        if not( Reached_Original_Depth) then
//        begin
//          if (next.DEBUG_blockid1 <> MAGIC_NUMBER_42) then
//          begin
//          {$ifdef write_to_console}
//            write('***next up is wrong! ');
//          {$endif}
//          end
//          else
//          begin
//            if next = current then
//            begin
//              write('..~-## this turns ugly! ##~-..');
//              where_to_next:=GO_NO_WHERE;
//            end
//            else
//            begin
//              where_to_next:=GO_UP;
//              writeln('Erase current: GO_UP');
//              DumpStatus(current);
//              DumpStatus(next);
//            end;
//          end;
//
//          if current = next then
//          begin
//            {$ifdef write_to_console}
//            writeln('*** next is the same as current !!!! ***');
//
//            writeln('Depth=', depth_counter);
//            {$endif}
//            where_to_next:=GO_NO_WHERE;
//            Reached_Original_Depth := True;
//          end;
//        end
//        else
//        begin
//          {$ifdef write_to_console}
//          writeln('Reached original depth');
//          {$endif}
//          where_to_next:=GO_NO_WHERE;
//        end;
//      end;
//      if block_to_delete <> nil then
//      begin
//        {$ifdef write_to_console}
//        writeln('Erase block_to_delete: before free block');
//        {$endif}
//        NodeFreeandNil(block_to_delete);
//        {$ifdef write_to_console}
//        writeln('Erase block_to_delete: freed block');
//        {$endif}
//        DecreaseNodeCount;
//        {$ifdef write_to_console}
//        writeln('Erase current: node count down');
//        {$endif}
//      end;
//      current:=next;
//      {$ifdef write_to_console}
//      writeln('Erase current: point to next');
//      DumpStatus(current);
//      {$endif}
//    End
//    else
//    begin
//      if (current.next <> NIL) then
//      begin
//        {$ifdef write_to_console}
//        writeln('Erase current error: current.next <> NIL. Current Depth=',depth_counter);
//        DumpStatus(current);
//        {$endif}
//        where_to_next:=GO_NO_WHERE;
//        //next:=go_to_rightmost_lowest_child(current,unreachable,depth_counter);
//      end
//      else
//      begin
//        if (current.Child <> NIL) then
//        begin
//          {$ifdef write_to_console}
//          writeln('Erase current error: current.Child <> NIL. Current Depth=',depth_counter);
//          DumpStatus(current);
//          {$endif}
//          where_to_next:=GO_NO_WHERE;
//        end;
//      end;
//    end;
//  End;
//  Result:=next;
//end;
//
//
//// Find the next to erase in the tree
//// (sequence: (1) rightmost lowest child
//// (2) left (prev) sibling
//// (9) Parent if the depth is not lower than were we started at.
//function FindNextToErase(var current:TBlock; var Depth_counter:int32; var error_code:TTreeError):TBlock;
//var unreachable:boolean;
//begin
//  Result:=Nil;
//  unreachable:=False;
//  error_code:=TreeInternalEraseError;
//  if current <> NIL then
//  begin
//
//    if current.child <> NIL then //This should not occur, but if it does we try to recover
//    begin
//      Result := go_to_rightmost_lowest_child(current, unreachable,depth_counter);
//      if unreachable then
//      begin
//        error_code:=TreeInternalSearchError;
//        current := Nil;
//      end
//      else
//      begin
//        error_code:=TreeNoError;
//      end;
//    end
//    else //There are no children
//    begin
//      if current.Prev <> Nil then //check if there is a sibling to the left
//      begin
//        Result:=current.Prev;
//        error_code:=TreeNoError;
//      end
//      else
//      begin ////check parent link
//        if current.Parent <> Nil then
//        begin
//          if Depth_counter >= 1 then //We are still below our start level
//          begin
//            Result:=current.Parent;
//            error_code:=TreeNoError;
//          end
//          else
//          begin //Stop looking further up
//            Result:=Nil;
//            error_code:=TreeNoError;
//          end;
//        end;
//      end;
//    end;
//  end
//  else //There is no current (current is root -> do not erase!!!)
//  begin
//    Result:=Nil;
//    error_code:=TreeEmpty;
//  end;
//end;
//
//Var curr : TBlock;
//    startBlock : TBlock;
//    found_last:boolean;
//    reached_original_depth,
//    stop_searching:boolean;
//    where_to_next,depth_counter : Int32;
//    unreachable:boolean;
//Begin
//  found_last:=False;
//  curr:=current;
//  startBlock:=current; //We use starting point for checking
//  {$ifdef write_to_console}
//  writeln;
//  writeln('Removechildren test-------------------------------------------------------------------------------------------');
//  DumpStatus(current);
//  {$endif}
//  if curr <> NIL Then
//  Begin
//    depth_counter := 0; //Relative level of depth in tree where we are pointing
//    // first step go to rightmost lowest placed child
//    unreachable := False;
//    where_to_next := GO_NO_WHERE;
//    reached_original_depth := False;
//    curr:= go_to_rightmost_lowest_child(curr, unreachable, depth_counter);
//    DumpStatus(curr);
//    if not unreachable then
//    begin
//      while (not reached_original_depth) and
//            (not unreachable) and
//            (curr <> Nil) do
//      begin
//        curr:=EraseCurrent(curr, depth_counter, reached_original_depth, where_to_next);
//        if curr = startBlock then
//        begin
//          if (*you have*) not(reached_original_depth) then
//          begin
//            writeln('There may be an error in your logic for erase. depth=',depth_counter,' next dir=', WhereTo2Str(where_to_next));
//          end
//          else
//          begin
//            write('So far your logic holds for erase.');
//            found_last:=True;
//          end;
//        end;
//
//        {$ifdef write_to_console}
//        write('<RemoveChildren>');
//        DumpStatus(curr);
//        {$endif}
//        if curr = Nil then unreachable:=True;
//        if where_to_next = GO_NO_WHERE then unreachable:=True;
//        if depth_counter > MAX_TREE_CHILD_DEPTH then unreachable:=True;
//      end;
//      {$ifdef write_to_console}
//      writeln;
//      writeln('<RemoveChildren done>');
//      {$endif}
//    end;
//  End;
//  {$ifdef write_to_console}
//  writeln('exit removechildren. Found target = ', found_last);
//  {$endif}
//End;


{********************************************************************
Description: removesd single node and ensures siblings and child links are maintained
Inputs:
Outputs:
********************************************************************}
Function TTreehandler.Remove(Current:TBlock):Boolean;
Var return:Boolean;
    link,scanner,prev,Next:TBlock;

Begin
  Return:=True;
  If Current<>NIL Then  // any item to remove ?
  Begin
    If current<>Root Then //We cannot delete root -> there is always min 1 node
    Begin
      Next:=Current.Next; //Store sibling pointers
      Prev:=Current.prev;
      If Current.Child<>NIL Then
      Begin //idea is that we pull children in and insert all children in between next and prev
        If Next<>NIL Then
        Begin
          Next.Prev:=FindLast(Current.child);
          if Next.Prev<>NIL Then
          Begin
            Next.Prev.Next:=next;
          End;
        End;
        If Prev<>NIL Then
        Begin
          Prev.Next:=FindFirst(Current.Child);
          if Prev.Next<>NIL Then
          Begin
            Prev.Next.Prev:=Prev;
          End;
        End;
      End // if Current.Child<>NIL
      Else
      Begin
        If Next<>NIL Then
        Begin
          Next.Prev:=Prev;
        End;
        If Prev<>NIL Then
        Begin
          Prev.Next:=Next;
        End;
      End; // if Current.Child=NIL
      if current.Parent<>NIL Then
      Begin
        if next<>NIL Then
        Begin
          link:=FindFirst(next); //find first child in chain
        End
        Else
        Begin
          if Prev<>NIL Then
          Begin
            link:=FindFirst(Prev); //find first child in chain
          End
          Else // There were only one child to parent and we just removed that one
          Begin
            if current.Child<>NIL Then
            Begin
              link:=FindFirst(current.Child);
            End
            Else
            Begin
              link:=NIL; //no children to parent anymore
            end;
          End;
        End;
        current.Parent.Child:=link; // Attach link to first child in chain
      End;
      scanner:=Current.Child;
      While scanner<>NIL do
      Begin
        scanner.Parent:=Current.Parent;
        scanner:=scanner.Next;
      End;
      //This part find the point of focus. It will point cursor to that point.
      SetSelect(root,FALSE);
      If Current.Child<>NIL Then
      Begin
        cursor:=Current.Parent.Child;
      End
      Else
      Begin
        If Current.Next<>NIL Then
        Begin
          cursor:=Current.Next;
        End
        Else
        Begin
          If Current.Prev<>NIL Then
          Begin
            cursor:=Current.Prev;
          End
          Else
          Begin
            If Current.Parent<>NIL Then
            Begin
              cursor:=Current.Parent;
            End
            Else
            Begin
              //ShowMessage('Undskyld, men der er opstået en intern fejl'+
              //            ' i programmet. Fejlen er ikke farlig og'+
              //            ' programmet fortsætter');
            End;
          End;
        End;
      End;
      Cursor.Selected:=TRUE;

      Try
        //Dispose(current);
        FreeAndNil(current);
        DecreaseNodeCount; //Assume success and decrease count of nodes
        current:=Nil;
      Except
        Return:=False;
      End; // try
    End
    Else //current=ROOT
    Begin
      Return:=FALSE;
    End;
  End
  Else //current=NIL
  Begin
    Return:=False;
  End;
  Remove:=Return;
End;


{********************************************************************
Description: adds paylod to given block as idx in tlist
             Removepayload frees also memory for payload,
             (remove will always call removepayload unconditionally)
Inputs: block (dest) for payload and pointer to payload
Outputs:
********************************************************************}
function TTreehandler.AddPayload(block:TBlock; payload:pointer):integer;
var idx:integer;
begin
  try
    //The memory the pointer is pointing to is not deallocated.
    Result:=0;
  except
    Result:=-1;
  end;
end;

{********************************************************************
Description: Sets selection state for tree from startblock and down *
Inputs: Startblock and selection state                              *
Outputs:                                                            *
********************************************************************}
function TTreehandler.RemovePayload(block:TBlock; payload:pointer):boolean;
begin
end;

{********************************************************************
Description: Sets selection state for tree from startblock and down *
Inputs: Startblock and selection state                              *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.SetSelect(startblock:TBlock; state:boolean);

  Procedure Deselect(Current:TBlock);
  Begin
    While Current<>NIL Do
    Begin
      current.Selected:=state;
///@todo change recoursive deselect to same method as delete
      //If Current.Child<>NIL Then
      //Begin
      //  Deselect(Current.Child);
      //End;
      current:=current.Next;
    End;
  End;

Begin
  Deselect(startblock);
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.SetCursor(NewPointer:TBlock);
Begin
  if NewPointer<>NIL Then
  Begin
    SetSelect(root,false);
    cursor:=NewPointer;
    cursor.Selected:=true;
  End;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.GoUp;
Begin
  if cursor.direction=Dirup Then
  Begin
    If Cursor.Parent<>NIL Then
    Begin
      SetSelect(root, false);
      Cursor:=Cursor.Parent;
    End;
  end;
  SetDirUp;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.GoDown;
Begin
  if cursor.direction=DirDown Then
  Begin
    If Cursor.Child<>NIL Then
    Begin
      if Cursor.Folded then Cursor.Folded:=False; //unfold if folded and we move down there
      SetSelect(root, false);
      Cursor:=Cursor.Child;
      Cursor:=FindMiddle(Cursor) ;
    End;
  end;
  SetDirDown;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.GoLeft;
Begin
  if cursor.direction=DirLeft Then
  Begin
    If Cursor.Prev<>NIL Then
    Begin
      SetSelect(root, false);
      Cursor:=Cursor.prev;
    End;
  end;
  SetDirLeft;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.GoRight;
Begin
  if cursor.direction=DirRight Then
  Begin
    If Cursor.next<>NIL Then
    Begin
      SetSelect(root, false);
      Cursor:=Cursor.next;
    End;
  End;
  SetDirRight;
End;


{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.SetDirRight;
Begin
  if cursor<>root then
    cursor.direction:=DirRight
  else
    cursor.direction:=DirDown;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.SetDirLeft;
Begin
  if cursor<>root then
    cursor.direction:=DirLeft
  else
    cursor.direction:=DirDown;
End;
{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TTreehandler.SetDirUp;
Begin
  if cursor<>root then
    cursor.direction:=DirUp
  else
    cursor.direction:=DirDown;
End;

{********************************************************************
Description: set direction of write cursor
Inputs:
Outputs:
********************************************************************}
Procedure TTreehandler.SetDirDown;
Begin
  if cursor <> Nil then
  begin
    cursor.direction:=DirDown;
  end;
End;


{********************************************************************
Description: return pointer to block in direction from current TBlock
Inputs: current = block to look into, Direction:Tdirection = which direction to look at
Outputs: pointer to block in 'direction' from 'current' TBlock
********************************************************************}
function TTreehandler.CheckBlockinDirection(current:TBlock; Direction:TDirection):Tblock;
begin
  Result:=Current;
  if Current <>Nil then
  begin
    case Direction of
      DirUp: Result:= Current.Parent;
      DirDown: Result:= Current.Child;
      DirLeft: Result:= Current.Prev;
      DirRight: Result:= Current.Next;
    end;
  end;
end;

function TTreehandler.CheckBlockinDirection(Direction:TDirection):Tblock;
begin
  Result:= CheckBlockinDirection(cursor,Direction);
end;

function TTreehandler.GoInDirection(current:TBlock; Direction:TDirection):Tblock;
begin
  Result:=CheckBlockinDirection(current, Direction);
end;

function TTreehandler.GoInDirection(Direction:TDirection):Tblock;
begin
  Result:=GoInDirection(cursor, Direction);
end;

{********************************************************************
Description: Checks wether it would be possible to move a subtree to*
             given input block                                      *
Inputs:      to_block (destination)                                 *
Outputs:     Was operation valid or not                             *
********************************************************************}
function TTreehandler.ValidMoveDestination(to_block:TBlock):boolean;
begin
  if to_block.Child = Nil then Result:=True else Result:=False;
end;

{********************************************************************
Description: Move on block (including children to another block with*
             no children.                                           *
Inputs:      from_block (source) to_block (destination)             *
Outputs:     Was operation valid or not                             *
********************************************************************}
function TTreehandler.MoveSubTree(from_block, to_block:TBlock):boolean;
begin
  if ValidMoveDestination(to_block) then
  begin
    to_block.Child:=from_block;
    //Fix siblings
    if from_block.prev <> Nil then
    begin
      from_block.Prev.Next:=from_block.Next;
    end;
    if from_block.next <> Nil then
    begin
      from_block.next.Prev:=from_block.prev;
    end;
    //Clear references to siblings
    from_block.prev:=Nil;
    from_block.next:=Nil;
    //Attached from block to new parent
    from_block.Parent:=to_block;
    Result:=True;
  end
  else
  begin
    Result:=False;
  end;
end;


{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TTreehandler.TreeToTextFile(filnavn:string):boolean;

  function convert_string_to_name(indstr:string):string;
  begin
    Result:=ReplaceStr(indstr,' ','_');
  end;

  Procedure WritelnWithIndent(Var fil:Text; content:String; indent:integer);
  Var i:integer;
  Begin
    i:=0;
    While indent>i do
    Begin
      write(fil,'  ');
      i:=i+1;
    End;
    writeln(fil,content);
  End;

  procedure writeItem(Var fil:Text; token:string; level:integer; content:string);
  Begin
    //writelnWithIndent(fil,'"'+content+'"',level);
//    writelnWithIndent(fil,''+token+' '+content+';',level);
    writelnWithIndent(fil,''+token+''+content,level);
  End;

  function Parse_Children_find_first(current:TBlock):TBlock;
  begin
    Result := Current.Child;
  end;

  function Parse_Children_find_next(current:TBlock):TBlock;
  begin
    Result:=current.Next;
  end;

  function CheckLinks(current:TBlock):string;
  begin
    Result:=' P';
    if current.Parent=nil then
      Result:=Result + '* '
    else
      if current.Parent.Child = Current then
        Result:=Result + '+ '
      else
      begin
        if current.Prev <> Nil then
           if Current.Prev.Parent = current.Parent then
              Result:=Result + '+ '
           else
             Result:=Result + '-! '
        else
          Result:=Result + '-! ';
      end;
    Result:=Result + 'C';
    if current.Child=nil then
      Result:=Result + '* '
    else
      if current.Child.Parent = Current then
        Result:=Result + '+ '
      else
        Result:=Result + '-! ';
    Result:=Result + 'L';
    if current.Prev=nil then
      Result:=Result + '* '
    else
      if current.prev.next = Current then
        Result:=Result + '+ '
      else
        Result:=Result + '-! ';
    Result:=Result + 'R';
    if current.next=nil then
      Result:=Result + '* '
    else
      if current.next.prev = Current then
        Result:=Result + '+ '
      else
        Result:=Result + '-! ';

  end;

  Procedure writeCodeRecord(Var fil:Text; current:TBlock; level:integer);
  var child:TBlock;
  Begin
    While Current<>NIL Do
    Begin
      Writeitem(fil,'"',level,current.BlockLabel+'"'+CheckLinks(current));
      child := Parse_Children_find_first(current);
      If Current.Child<>NIL Then
      Begin
        writeCodeRecord(fil,Current.Child, level+1);
      End;
      current:=current.Next;
    End;


  End;

Var udfil:Text;
Begin
  Try
    TreeToTextFile:=True;
    AssignFile(udfil,filnavn);
    ReWrite(udfil);
    Try
      writeln(udfil,'//Tree structure - ASCII version');

      writeCodeRecord(udfil,root,0);
    Finally
      Close(udfil);
    End;
  Except
    writeln('Kunne ikke åbne fil');
    TreeToTextFile:=false;
  End;
End;
{$ifdef unit_test}
{********************************************************************
@short

Outputs:
********************************************************************}
function test_tree(success:boolean):boolean;

function RandomDirection:TDirection;
var direction:integer;
begin
  direction:=Random(4);
  {$ifdef write_to_console}
  write('* Rand Dir=',direction,'* ');
  {$endif}
  case direction of
    0: Result:=DirDown;
    1: Result:=Dirleft;
    2: Result:=DirRight;
    3: Result:=DirRight;
    4: Result:=DirDown;
  otherwise
    Result:=DirDown;
  end;
end;

var TestTree: TTreeHandler;
    idx, count_to_create:integer;
    newblock:TBlock;
begin
  Result := success;
  if Success then
  begin
    log.LogStatus('Running TTreehandler unit test','Treehandler.pas');
    TestTree:=TTreeHandler.Create('First');
    //Test with only a root and we free children (there are none)
    if TestTree.IsInitialized then
    begin
      log.LogStatus('Created root','Treehandler.pas');
      try
        try
          TestTree.RemoveChildren(TestTree.Root);
          TestTree.SetCursor(TestTree.Root);
          //if we reach here both functions worked correctly
          log.LogStatus('Remove children in empty tree succeeded','Treehandler.pas');
        except
          log.LogError('Remove children in empty tree created an exception...','Treehandler.pas');
        end;
      finally
        FreeAndNil(TestTree); //.Destroy;
      end;
      log.LogStatus('Root free on empty tree','Treehandler.pas');

      try
        TestTree:=TTreeHandler.Create('Root');
        if TestTree.IsInitialized then
        begin
          log.LogStatus('Created root','Treehandler.pas');
//          count_to_create := 833000;//Random(100000);
          count_to_create := (Int32(Random(5)*4)) + 2;
          if count_to_create > 0 then
          begin
            //The first node has to be a child to the root node
            Randomize;
            //There is always the root node
            newblock := TestTree.InsertInDirection(TestTree.cursor, 'first item',DirDown);
            if newblock <> Nil then
            begin
              idx:= 2;
              while (idx < count_to_create) and
                    (newblock <> Nil) do
              begin
                //if (idx mod 250000)=0 then
                //begin
                //  writeln('Sleeps 100 ms');
                //  Sleep(100);
                //end;
                newblock:=TestTree.InsertInDirection(TestTree.cursor, 'item'+IntToStr(idx), DirDown); // RandomDirection);
                if newblock = Nil then
                  log.LogError('InsertInDirection could not allocate block! ','Treehandler.pas');
                newblock:=TestTree.InsertInDirection(TestTree.cursor, 'item'+IntToStr(idx), RandomDirection); // RandomDirection);
                if newblock = Nil then
                  log.LogError('InsertInDirection could not allocate block! ','Treehandler.pas');
                newblock:=TestTree.InsertInDirection(TestTree.cursor, 'item'+IntToStr(idx), RandomDirection); // RandomDirection);
                if newblock = Nil then
                  log.LogError('InsertInDirection could not allocate block! ','Treehandler.pas');
                newblock:=TestTree.InsertInDirection(TestTree.cursor, 'item'+IntToStr(idx), RandomDirection); // RandomDirection);
                if newblock = Nil then
                  log.LogError('InsertInDirection could not allocate block! ','Treehandler.pas');

                idx:=idx + 4;
              end;
              writeln('Node count to create = ',count_to_create,' - Number created=',idx);
            end
            else
              log.LogError('First InsertInDirection could not allocate block! ','Treehandler.pas');
          end;
          if testtree.nodes_in_tree = (count_to_create+1) then
            log.LogStatus('Nodes in tree='+IntToStr(testtree.nodes_in_tree) +
                        ' Children+root='+IntToStr(count_to_create+1) ,'Treehandler.pas')
          else
            log.LogError('Nodes in tree '+IntToStr(testtree.nodes_in_tree) +
            ' differs from Children+root='+IntToStr(count_to_create+1) ,'Treehandler.pas');
          try
            TestTree.TreeToTextFile('testwriter.txt');
            log.LogStatus('Write to text file done.','Treehandler.pas');
          except
            log.LogStatus('Write to text file failed','Treehandler.pas');
          end;
          try
            try
              idx:=0;
              TestTree.SetCursor(TestTree.Root);
              while (idx <= (count_to_create/10)) do
              begin
                TestTree.GoDown;
                idx:=idx+1;
              end;

              TestTree.RemoveChildren(TestTree.cursor);
              {$ifdef write_to_console}
              writeln('Return from REmoveChildren');
              {$endif}
              TestTree.SetCursor(TestTree.Root);
              //writeln('New root set');
              log.LogStatus('Remove children in singular tree succeeded','Treehandler.pas');
              log.LogStatus('Nodes in tree='+IntToStr(testtree.nodes_in_tree), 'Treehandler.pas');
              //writeln('Log done.');
            except
              log.LogError('Remove children in singular tree created an exception...','Treehandler.pas');
              writeln('RemoveChildren Exception');
            end;
          finally
            writeln('Trying to Remove main tree');
            try
              //if TestTree <> Nil then TestTree.destroy;
              freeandnil(TestTree);
              //writeln('No Op:', TestTree.NodesInTree);
              writeln('Did Remove main tree');
            except
              log.LogError('Remove tree in singular tree created an exception...','Treehandler.pas');
              writeln('Tree.destroy Exception');
            end;
          end;
          log.LogStatus('Root free on singular tree','Treehandler.pas')
        end
        else
        begin
          log.LogError('Could not create root','Treehandler.pas');
        end;
      except
        log.LogError('Could not create tree','Treehandler.pas');
      end;
    end
    else
    begin
      log.LogError('Could not create root','Treehandler.pas');
    end;
  end;
end;

//initialization
//begin
//  if test_tree(True) then
//    log.LogStatus('unit test success :"test_tree"','Treehandler.pas')
//  else
//    log.LogError('unit test fail :"test_tree"', 'Treehandler.pas');
//
//  Writeln('Unit test Done "Treehandler.pas"');
//end;

//finalization
//begin
//end;

{$endif unit_test}
end.
{$endif}

