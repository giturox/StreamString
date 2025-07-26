{********************************************************************
Description:                                                        *
Inputs:                                                             *
********************************************************************}
Unit diagramhandler;

{$MODE Delphi}

Interface
Uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls , vectorgraphics;

Const
  DeclareSize=4096;
  InitSize=4096;
  BodySize=16384;
  EndSize=4096;

Type
  TDirection=(DirLeft,DirRight,DirUp,DirDown);

  p_TBlock=^TBlock;
  TBlock = Record
    Next,Prev:p_TBlock;       //Next/previous in the double-chain
    Parent,Child:p_TBlock;    //Parent (if any) and first-child (if any children)
    ModuleName:String[255];   //Label of the structure
    Selected:Boolean;         //Is the square selected by user
    Folded:boolean;
    x,y:Real;                 //Coordinates
    width,height:Real;        //Width of children underneath
    direction:TDirection;     //What way are user looking?
    loop:Boolean;             //Is this a square with a loop?
    selection:Boolean;        //Is this a sqaure with an if/case construct?
    LoopText:String[255];     //Loop belongs to parent
    SelectionText:String[255];//Selectiontext belongs to children
    Declares:Array [0..DeclareSize] of Char;
    Init:Array [0..InitSize] of Char;
    Body:Array [0..BodySize] of Char;
    Ends:Array [0..EndSize] of Char;
    //IMPLEMENTER DET HER
  End; //Record

{procedure TForm1.Button1Click(Sender: TObject);
  var
  Buffer: PChar;
begin
  GetMem(Buffer,Length(Label1.Caption) + Length(Edit1.Text) + 1);
  StrCopy(Buffer, PChar(Label1.Caption));
  StrCat(Buffer, PChar(Edit1.Text));
  Label1.Caption := Buffer;
  Edit1.Clear;
  FreeMem(Buffer);
End;
}

  TDiagramHandler = Class(TObject)
    Private
      { Private declarations \}
      sizefactor,
      BoxWidth,
      BoxHeight,
      VerticalSpacing,
      HorisontalSpacing:Real;


      Procedure AddToEnd(current,NewSibling:p_TBlock);
      Procedure InsertAfter(current,NewSibling:p_TBlock);
      Procedure InsertBefore(current,NewSibling:p_TBlock);
      Procedure InitCurrent(current:p_TBlock);
    Public
      { Public declarations \}
      cursor,
      Root:p_TBlock;

      PageWidth,
      PageHeight,
      offsetx,offsety,
      MaximumWidth,MaximumHeight:Real;
      Function CreateRoot(name:String):Boolean;
      Function AddChildToEnd(Parent:p_TBlock; name:String):p_TBlock;
      Function InsertChild(Parent:p_TBlock; name:String; pos:integer):p_TBlock;
      Function Insert(Parent,Current:p_TBlock; name:String):p_TBlock;
      Function InsertInDirection(Current:p_TBlock; name:String;
                                 direction:TDirection):p_TBlock;
      Function Remove(Current:p_TBlock):Boolean;
      Function FindFirst(current:p_TBLock):p_TBlock;
      Function FindLast(current:p_TBLock):p_TBlock;
      Function FindMiddle(current:p_TBLock):p_TBlock;
      function GetCurrent:p_TBlock;
      Procedure RemoveChildren(current:p_TBlock);
      Procedure UpdateTreeGraphics;
      Procedure DrawTree(DrawCanvas:TCanvas; printing:boolean);
      Function  FindHit(x,y:real):p_TBlock;
      Procedure SetSelect(state:boolean);
      Procedure SetCursor(NewPointer:p_TBlock);
      Procedure SetScale(newsize:Real);
      Function  GetScale:Real;
      procedure ToggleFolded;
      procedure GoUp;
      Procedure GoDown;
      Procedure GoLeft;
      Procedure GoRight;
      procedure SetDirUp;
      Procedure SetDirDown;
      Procedure SetDirLeft;
      Procedure SetDirRight;
      Function  SaveToFile(filnavn:String):boolean;
      Function  ReadFromFile(filnavn:String):boolean;
  End; //Class

Implementation

{********************************************************************
Description:                                                        *
********************************************************************}



Function TDiagramhandler.CreateRoot(name:String):Boolean;
Var return:Boolean;
Begin
  Return:=True;
  Try
    New(Root);
    InitCurrent(root);
    root^.Child:=NIL;
    Root^.Next:=NIL;
    Root^.Prev:=NIL;
    Root^.Parent:=NIL;
    Root^.ModuleName:=name;
  Except
    Return:=False;
  End;
  CreateRoot:=Return;
  //Sizes are in millimeters
  PageWidth:=420.2;         //A3 landscape
  PageHeight:=297.0;        //-"-
  SetSCale(3.0);
  cursor:=root;
End;

Procedure TDiagramhandler.SetScale(NewSize:Real);
Begin
  sizefactor:=NewSize;
  BoxWidth:=25.0*sizefactor;
  BoxHeight:=15.0*sizefactor;
  VerticalSpacing:=15.0*sizefactor;
  HorisontalSpacing:=2.50*sizefactor;
End;

Function TDiagramhandler.GetScale:Real;
Begin
  GetScale:=sizefactor;
End;

Procedure TDiagramhandler.InitCurrent(current:p_TBlock);
Begin
  current^.Child:=NIL;
  current^.Next:=NIL;
  Current^.Prev:=NIL;
  Current^.Parent:=NIL;
  Current^.x:=0.0;
  Current^.y:=0.0;
  Current^.width:=0.0;
  Current^.height:=0.0;
  Current^.Selected:=FALSE;
  current^.ModuleName:='';
  Current^.selection:=FALSE;
  Current^.loop:=FALSE;
  current^.Folded:=False;
  Current^.direction:=DirDown;
End;

//
function TDiagramhandler.GetCurrent:p_TBlock;
begin
  if cursor <> Nil then
  begin
    Result:=cursor;
  end
  else
  begin
    if root <> Nil then
    begin
      Result:=root;
    end
    else
    begin
      Result:=Nil;
    end;
  end;
end;


{********************************************************************
Description: Adds a new sibling to the end of a list of TBlocks     *
Inputs:                                                             *
  <current> = Current sibling to which list we will add a sibling   *
  <NewSibling> = new sibling to add                                 *
********************************************************************}
Procedure TDiagramhandler.AddToEnd(current,NewSibling:p_TBlock);
var count: Uint32;
Begin
  count:=0;
  While current^.Next<>NIL Do //Continue until end of list
  Begin
    current:=current^.Next;
  End; //while
  Current.Next:=NewSibling; //Add new sibling to end of list
End;

{********************************************************************
Description: Inserts a new sibling after current                    *
Inputs:                                                             *
  <current> = Current sibling to which list we will add a sibling   *
  <NewSibling> = new sibling to add                                 *
********************************************************************}
Procedure TDiagramhandler.InsertAfter(current,NewSibling:p_TBlock);
Var next:p_TBlock;
Begin
  if current<>NIL Then
  Begin
    next:=current.Next;
    Current.Next:=NewSibling;
    NewSibling^.Prev:=current;
    NewSibling^.Next:=next; //If current is at the end of the list then next will
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
Procedure TDiagramhandler.InsertBefore(current,NewSibling:p_TBlock);
Var prev:p_TBlock;
Begin
  if current<>NIL Then
  Begin
    prev:=current^.Prev;
    Current^.prev:=NewSibling;
    NewSibling^.Prev:=prev; //If current is at the end of the list then
                            //prev will be NIL
    NewSibling^.Next:=Current;
    NewSibling^.Parent:=Current^.Parent;
    NewSibling^.Parent^.Child:=findfirst(current);
    If prev<>NIL Then
    Begin
      prev^.Next:=NewSibling;
    End
    Else
    Begin
      //Do nothing
    End;
  End;
End;


{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.AddChildToEnd(Parent:p_TBlock; name:String):p_TBlock;
Var return:p_TBlock;
    child:p_TBlock;
Begin
  Try
    New(child);
    InitCurrent(child);
    Child^.ModuleName:=name;
    Child^.Parent:=Parent; //It has a parent
    Child^.Child:=NIL;     //Still no children
    Child^.next:=NIL;      //It will be attached to evt. siblings
    Child^.Prev:=NIL;

    If Parent^.Child=NIL Then //If parent had no children before do a simple add
    Begin
      Parent^.Child:=Child;
    End
    Else //Insert new child at end of the line of children
    Begin
      AddToEnd(Parent^.Child,Child);
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
Function TDiagramhandler.InsertChild(Parent:p_TBlock; name:String;
                                     Pos:Integer):p_TBlock;
Var
  return:p_TBlock;
  current,
  child:p_TBlock;
  i:Integer;

Begin
  Try
    New(child);
    InitCurrent(child);
    Child^.ModuleName:=name;
    Child^.Parent:=Parent; //It has a parent
    Child^.Child:=NIL;     //Still no children
    Child^.next:=NIL;      //It will be attached to evt. siblings
    Child^.Prev:=NIL;

    If Parent^.Child=NIL Then //If parent had no children before do a simple add
    Begin
      Parent^.Child:=Child;
    End
    Else //Insert new child in the line of children at pos
    Begin
      If pos<=0 Then
      Begin
        InsertBefore(Parent^.Child,Child);
      End
      Else //pos<>0
      Begin
        i:=1;
        Current:=Parent^.Child;
        While (i<pos) AND (Current^.Next<>NIL) Do //Continue until position OR
                                                  //end-of-list is found
        Begin
          Current:=Current^.Next;
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

Function TDiagramhandler.Insert(Parent,Current:p_TBlock; name:String):p_TBlock;
var Child,Retur:p_TBlock;
Begin
  Retur:=NIL;
  If (Current<>NIL) OR (Parent<>NIL) Then
  Begin
    Try
      New(child);
      InitCurrent(child);
      Child^.ModuleName:=name;
      if parent=NIL Then
      Begin
        Parent:=Current^.Parent; //They have the same parent
      End;
      Child^.Parent:=Parent; //It has a parent
      Child^.Child:=NIL;     //Still no children
      Child^.next:=NIL;      //It will be attached to evt. siblings
      Child^.Prev:=NIL;

      If Parent^.Child=NIL Then //If parent had no children before do a simple add
      Begin
        if current<>NIL Then
        Begin
          ShowMessage('Til alle andre: Programmet fortsætter og Uffe '+
                      'undskylder fejlen. '+
                      'Det er en god ide at gemme NU under et ANDET filnavn '+
                      'og genstarte programmet!!! '+
                      'Til Uffe: Der er en lille fejl i programmet. '+
                      'Diagramhandler.insert:Parent^.child=0 og current != 0!');
        End;
        Parent^.Child:=Child;
      End
      Else //Insert new child in the line of children at pos
      Begin
        if current=NIL Then
        Begin
{          temp:=parent^.Child;
          temp^.Prev:=Child;
          Child^.Next:=Temp;
          parent^.Child:=child;}
          InsertAfter(FindLast(parent^.Child),child);
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
Function TDiagramhandler.InsertInDirection(Current:p_TBlock; name:String;
                                           direction:TDirection):p_TBlock;
var scanner,newblock,prev,next,retur:p_TBlock;
Begin
  retur:=NIL;
  If current<>NIL Then
  Begin
    Case direction of
      DirLeft:
      Begin
        Try
          New(newBlock);
          InitCurrent(NewBlock);
          NewBlock^.ModuleName:=name;
          InsertBefore(current,newBlock);
        Except
          NewBlock:=NIL;
        End;
      End;
      DirRight:
      Begin
        insert(NIL,Current,name);
      End;
      DirUp:
      Begin
        Try
          New(newBlock);
          InitCurrent(NewBlock);
          prev:=current^.Parent;
          NewBlock^.Child:=current;
          NewBlock^.Parent:=Current^.Parent;
          NewBlock^.Next:=Current^.Next;
          NewBlock^.Prev:=Current^.Prev;
          Current^.Next:=NIL;
          Current^.Prev:=NIL;
          If NewBlock^.Next<>NIL Then NewBlock^.Next^.Prev:=Newblock;
          If NewBlock^.Prev<>NIL Then NewBlock^.Prev^.Next:=Newblock;
          NewBlock^.Child^.Parent:=Newblock;
          NewBlock^.Parent^.Child:=FindFirst(newblock);
          NewBlock^.ModuleName:=name;
        Except
          NewBlock:=NIL;
        End;
      End;
      DirDown:
      Begin
        Try
          New(newBlock);
          InitCurrent(NewBlock);

          NewBlock^.Child:=FindFirst(current^.Child); // Make sure tree is coherent
          NewBlock^.Parent:=Current;

          Current^.Child:=Newblock;
          NewBlock^.ModuleName:=name;

          scanner:=NewBlock^.Child; //Give new parent
          While scanner<>NIL do
          Begin
            scanner^.Parent:=newblock;
            scanner:=scanner^.Next;
          End;
        Except
          NewBlock:=NIL;
        End;
      End;
    End;
  End;

  InsertInDirection:=Retur;
End;

{********************************************************************
Description: Finds first item on the same level                     *
Inputs: Current block                                               *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.FindFirst(current:p_TBLock):p_TBlock;
Begin
  if current<>NIL Then
  Begin
    While (Current.Prev<>NIL) do
    Begin
      Current:=Current.Prev;
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

  FindFirst:=Current;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.FindLast(current:p_TBLock):p_TBlock;
Begin
  if current<>NIL Then
  Begin
    While (Current.Next<>NIL) do
    Begin
      Current:=Current.Next;
    End;
  End
  Else
  Begin
    //Do nothing, since current is nil
  End;
  FindLast:=Current;
End;

{********************************************************************
Description: Finds first item on the same level                     *
Inputs: Current block                                               *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.FindMiddle(current:p_TBLock):p_TBlock;
Var count,middle:integer;
    first:p_TBlock;
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
Description: Removes all children to current                        *
Inputs: current element, whose children are to be removed           *
Outputs: *None*                                                     *
********************************************************************}
Procedure TDiagramhandler.RemoveChildren(current:p_TBlock);
Var curr,next:p_TBlock;
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

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.Remove (Current:p_TBlock):Boolean;
Var return:Boolean;
    link,scanner,prev,Next:p_TBlock;

Begin
  Return:=True;
  If Current<>NIL Then
  Begin
    If current<>Root Then
    Begin
      Next:=Current^.Next;
      Prev:=Current^.prev;
      If Current.Child<>NIL Then
      Begin
        If Next<>NIL Then
        Begin
          Next^.Prev:=FindLast(Current^.child);
          if Next^.Prev<>NIL Then
          Begin
            Next^.Prev^.Next:=next;
          End;
        End;
        If Prev<>NIL Then
        Begin
          Prev^.Next:=FindFirst(Current^.Child);
          if Prev^.Next<>NIL Then
          Begin
            Prev^.Next^.Prev:=Prev;
          End;
        End;
      End // if Current.Child<>NIL
      Else
      Begin
        If Next<>NIL Then
        Begin
          Next^.Prev:=Prev;
        End;
        If Prev<>NIL Then
        Begin
          Prev^.Next:=Next;
        End;
      End; // if Current.Child=NIL

      if current^.Parent<>NIL Then
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
            if current^.Child<>NIL Then
            Begin
              link:=FindFirst(current^.Child);
            End
            Else
            Begin
              link:=NIL; //no children to parent anymore
            end;
          End;
        End;

        current^.Parent^.Child:=link; // Attach link to first child in chain
      End;

      scanner:=Current^.Child;
      While scanner<>NIL do
      Begin
        scanner^.Parent:=Current^.Parent;
        scanner:=scanner^.Next;
      End;

      //This part find the point of focus. It will point cursor to that point.
      SetSelect(FALSE);

      If Current^.Child<>NIL Then
      Begin
        cursor:=Current^.Parent^.Child;
      End
      Else
      Begin
        If Current^.Next<>NIL Then
        Begin
          cursor:=Current^.Next;
        End
        Else
        Begin
          If Current^.Prev<>NIL Then
          Begin
            cursor:=Current^.Prev;
          End
          Else
          Begin
            If Current^.Parent<>NIL Then
            Begin
              cursor:=Current^.Parent;
            End
            Else
            Begin
              ShowMessage('Undskyld, men der er opstået en intern fejl'+
                          ' i programmet. Fejlen er ikke farlig og'+
                          ' programmet fortsætter');
            End;
          End;
        End;
      End;
      Cursor^.Selected:=TRUE;

      Try
        Dispose(current);

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
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.UpdateTreeGraphics;

  {********************************************************************
  Description:                                                        *
  Inputs:                                                             *
  Outputs:                                                            *
  ********************************************************************}
  Procedure ScanAndUpdateTree(Current:p_TBlock;
                              Var CurrentX:Real; CurrentY:Real);
  Var
    oldx,width:Real;
  Begin
    While Current<>NIL Do
    Begin
      if Current^.Child=NIL Then
      Begin
        CurrentX:=CurrentX + (BoxWidth + HorisontalSpacing);
        Current^.x:=CurrentX;
      End
      Else
      Begin
        OldX:=CurrentX;
        ScanAndUpdateTree(Current^.Child,
                          CurrentX,
                          CurrentY + (BoxHeight+VerticalSpacing) );
        Width:=(CurrentX + (BoxWidth + HorisontalSpacing)) - OldX;
        Current^.width:=width; //Store the width for use in loop drawing
        Current^.x:=OldX+Width/2.0;
      End;
      Current^.y:=CurrentY;
      Current:=Current^.Next
    End;
    If CurrentX>MaximumWidth Then MaximumWidth:=CurrentX;
    If Currenty>MaximumHeight Then MaximumHeight:=Currenty;
  End;

Var CurrX,CurrY:Real;
Begin
  MaximumWidth :=0;
  MaximumHeight:=0;
  CurrX:=0;
  CurrY:=VerticalSpacing;
  ScanAndUpdateTree(root,CurrX,CurrY);
  MaximumWidth :=MaximumWidth  + (BoxWidth);
  MaximumHeight:=MaximumHeight + (BoxHeight);
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
procedure TDiagramhandler.drawTree(DrawCanvas:TCanvas; printing:boolean);

  procedure drawit(Current:p_TBlock);
  Begin
    While Current<>NIL Do
    Begin
      if current^.Selected Then
      Begin
        DrawCanvas.Pen.Color:=$00000000;
        DrawCanvas.Brush.Color:=$00000000;
      End
      Else
      Begin
        DrawCanvas.Pen.Color:=$00AAAAAA;
        DrawCanvas.Brush.Color:=$00AAAAAA;
      End;
      DrawCanvas.Rectangle(Round(Current^.x - boxwidth/2.0 + boxwidth/20.0 - offsetx),
                           Round(Current^.y + BoxHeight/20.0 - offsety ),
                           Round(Current^.x + boxwidth/2.0 + boxwidth/20.0 - offsetx),
                           Round(Current^.y + boxheight + BoxHeight/20.0 - offsety));
      if current^.Selected Then
      Begin
        DrawCanvas.Pen.Color:=RGB(128,128,128);
        DrawCanvas.Brush.Color:=$00FFE2C6; //RGB(210,210,255);
      End
      Else
      Begin
        DrawCanvas.Pen.Color:=RGB(0,0,0);
        DrawCanvas.Brush.Color:=RGB(255,255,255);
      End;
      if Current^.Folded then
      begin
        DrawCanvas.Pen.Color:=rgb(0,255,0);
      end;

      DrawCanvas.Rectangle(Round(Current^.x - boxwidth/2.0 - offsetx),
                           Round(Current^.y - offsety),
                           Round(Current^.x + boxwidth/2.0 - offsetx),
                           Round(Current^.y + boxheight - offsety));
      DrawCanvas.TextRect(Rect(Round(Current^.x - boxwidth/2.0 - offsetx )+1,
                               Round(Current^.y - offsety)+1,
                               Round(Current^.x + boxwidth/2.0 - offsetx)-1,
                               Round(Current^.y + boxheight - offsety)-1),
                          Round(Current^.x - boxwidth/2.0 - offsetx)+1,
                          Round(Current^.y - offsety)+1,
                          Current^.ModuleName);

      //if current^.Folded then showmessage('Folded item');
      If current^.selection Then
      Begin
        if current^.Selected Then
        Begin
            ClipArtHandler.Drawselect(DrawCanvas,Current^.x-offsetx,
                                      Current^.y-offsety,
                                      BoxHeight*0.15,
                                      RGB(0,0,0),RGB(128,0,0));
        end
        Else
        Begin
            ClipArtHandler.Drawselect(DrawCanvas,Current^.x-offsetx,
                                      Current^.y-offsety,
                                      BoxHeight*0.15,
                                      RGB(0,0,0),RGB(0,0,0));
        End;
      End;
//      If current^.Child<>NIL Then //We only draw the loop if current has any children
      If (current^.Child<>NIL) Then //We only draw the loop if current has any children
      Begin
        If current^.loop Then
        Begin
          if current^.Selected Then
          Begin
            ClipArtHandler.Drawloop(DrawCanvas,Current^.x-offsetx,
                                    Current^.y+boxheight*1.25-offsety,
                                    current^.width-boxwidth-HorisontalSpacing*2,
                                    BoxHeight*0.5,
                                    RGB(255,0,0),clNone);
          end
          Else
          Begin
            ClipArtHandler.Drawloop(DrawCanvas,Current^.x-offsetx,
                                    Current^.y+boxheight*1.25-offsety,
                                    current^.width-boxwidth-HorisontalSpacing*2,
                                    BoxHeight*0.5,
                                    RGB(0,0,0),clNone);
          End;
        End;
      end;
      If Current^.Parent<>NIL Then
      Begin
        if current^.Parent^.Selected Then
        Begin
          DrawCanvas.Pen.Color:=RGB(255,0,0);
          DrawCanvas.Brush.Color:=RGB(255,255,255);
        End
        Else
        Begin
          DrawCanvas.Pen.Color:=RGB(0,0,0);
          DrawCanvas.Brush.Color:=RGB(255,255,255);
        End;
        DrawCanvas.MoveTo(Round(Current^.parent^.x - offsetx),
                          Round(Current^.parent^.y+BoxHeight - offsety));
        If current^.selection Then
        Begin
          DrawCanvas.LineTo(Round(Current^.x - offsetx),Round(Current^.y - offsety - BoxHeight*0.14));
        End
        Else
        Begin
          DrawCanvas.LineTo(Round(Current^.x - offsetx),Round(Current^.y - offsety));
        End;
      End;

      If (current=cursor) AND NOT printing Then  //cursor direction arrow
      Begin //Vi ønsker ikke at se cursor når vi printer
        Case current^.direction of
          DirLeft:
          Begin
            ClipartHandler.DrawArrow(DrawCanvas,
                                     Current^.x-BoxWidth/2 - offsetx,
                                     Current^.y+boxheight/2 - offsety,
                                     boxheight/20,180,
                                     RGB(0,0,0),RGB(220,128,64));
          End;
          DirRight:
          Begin
            ClipartHandler.DrawArrow(DrawCanvas,
                                     Current^.x+BoxWidth/2 - offsetx,
                                     Current^.y+boxheight/2 - offsety,
                                     boxheight/20,0,
                                     RGB(0,0,0),RGB(220,128,64));
          End;
          DirUp:
          Begin
            ClipartHandler.DrawArrow(DrawCanvas,
                                     Current^.x - offsetx,
                                     Current^.y - offsety,
                                     boxheight/20,90,
                                     RGB(0,0,0),RGB(220,128,64));
          End;
          DirDown:
          Begin
            ClipartHandler.DrawArrow(DrawCanvas,
                                     Current^.x - offsetx,
                                     Current^.y+boxheight - offsety,
                                     boxheight/20,-90,
                                     RGB(0,0,0),RGB(220,128,64));
          End;
        End;
      End;
      If (Current^.Child<>NIL) and
         (current^.Folded = False) Then
      Begin
        DrawIt(Current^.Child);
      End;
      current:=current^.Next;
    End;
  End;

Begin
  If NOT(printing) Then
  Begin
    Cursor^.Selected:=TRUE;
    DrawCanvas.Font.Size:=Round(2.7*sizefactor);
  End
  Else
  Begin
    DrawCanvas.Font.Size:=Round(2.7*sizefactor/15);
  End;
  DrawCanvas.Brush.Color:=RGB(255,255,255);
  DrawCanvas.Refresh;
  DrawCanvas.Font.Name:='Times New Roman';

  DrawIt(Root);
  //DrawCursor(DrawCanvas);
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.FindHit(x,y:real):p_TBlock;

  function FindIt(Current:p_TBlock; x,y:real):p_TBlock;
  Var found:p_TBlock;

    Function WithinHitBox(curr:p_TBlock; x,y:real):boolean;
    Var TempX:Real;
        retur:boolean;
    Begin
      retur:=false;
      TempX:=Curr^.x - (boxwidth/2.0);
      If x >= TempX Then
      Begin
        If x <= (TempX+BoxWidth) Then
        Begin
          If y >= Curr^.y Then
          Begin
            If y <= (Curr^.y + BoxHeight) Then
            Begin
              retur:=true;
            End;
          End;
        End;
      End;
      WithinHitBox:=retur;
    End;

  Begin
    found:=NIL;
    While (Current<>NIL) AND (found=NIL) Do
    Begin
      If WithinHitBox(current,x,y) Then
      Begin
        found:=current;
        cursor:=current;
        current^.Selected:=true;
        current:=NIL;
        //showMessage('HIT!!!');
      End
      Else
      Begin
        If Current^.Child<>NIL Then
        Begin
          found:=FindIt(Current^.Child,x,y);
        End;
      End;
      if current<>NIL Then Current:=Current^.Next;
    End;
    //if (found=NIL) Then showmessage('----');
    findit:=found;
  End;

Begin
  FindHit:=FindIt(root,x,y);
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetSelect(state:boolean);

  Procedure Deselect(Current:p_TBlock);
  Begin
    While Current<>NIL Do
    Begin
      current^.Selected:=state;
      If Current^.Child<>NIL Then
      Begin
        Deselect(Current^.Child);
      End;
      current:=current^.Next;
    End;
  End;

Begin
  Deselect(root);
End;

Procedure TDiagramhandler.ToggleFolded;
var
   current_block:p_TBlock;
Begin
  current_block:=GetCurrent;
  if current_block<> Nil then
  begin
    if current_block^.Folded = False then
    begin
//      ShowMessage('Folded');
      current_block^.Folded:=True;
    end
    else
    begin
//      ShowMessage('UnFolded');
      current_block^.Folded:=False;
    end;
  end;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetCursor(NewPointer:p_TBlock);
Begin
  if NewPointer<>NIL Then
  Begin
    SetSelect(false);
    cursor:=NewPointer;
    cursor^.Selected:=true;
  End;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.GoUp;
Begin
  if cursor^.direction=Dirup Then
  Begin
    If Cursor^.Parent<>NIL Then
    Begin
      SetSelect(false);
      Cursor:=Cursor^.Parent;
    End;
  end;
  SetDirUp;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.GoDown;
Begin
  if cursor^.direction=DirDown Then
  Begin
    If Cursor^.Child<>NIL Then
    Begin
      if Cursor^.Folded then Cursor^.Folded:=False; //unfold if folded and we move down there
      SetSelect(false);
      Cursor:=Cursor^.Child;
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
Procedure TDiagramhandler.GoLeft;
Begin
  if cursor^.direction=DirLeft Then
  Begin
    If Cursor^.Prev<>NIL Then
    Begin
      SetSelect(false);
      Cursor:=Cursor^.prev;
    End;
  end;
  SetDirLeft;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.GoRight;
Begin
  if cursor^.direction=DirRight Then
  Begin
    If Cursor^.next<>NIL Then
    Begin
      SetSelect(false);
      Cursor:=Cursor^.next;
    End;
  End;
  SetDirRight;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetDirRight;
Begin
  if cursor<>root then
    cursor^.direction:=DirRight
  else
    cursor^.direction:=DirDown;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetDirLeft;
Begin
  if cursor<>root then
    cursor^.direction:=DirLeft
  else
    cursor^.direction:=DirDown;
End;
{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetDirUp;
Begin
  if cursor<>root then
    cursor^.direction:=DirUp
  else
    cursor^.direction:=DirDown;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Procedure TDiagramhandler.SetDirDown;
Begin
  cursor^.direction:=DirDown;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.SaveToFile(filnavn:string):boolean;

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

  procedure writeStartToken(Var fil:Text; token:string; level:integer; content:string);
  Begin
    writelnWithIndent(fil,'<'+token+'>',level);
    writelnWithIndent(fil,'"'+content+'"',level);
  End;

  procedure writeEndToken(Var fil:Text; token:string; level:integer);
  Begin
    writelnWithIndent(fil,'</'+token+'>',level);
  End;

  Procedure writeRecord(Var fil:Text; current:p_TBlock; level:integer);
  Begin
    While Current<>NIL Do
    Begin
      WriteStartToken(fil,'RECORD',level,current^.ModuleName);
      If Current^.selection=true then
      Begin
        WriteStartToken(fil,'SELECTION',level,'TRUE');
      End
      Else
      Begin
        WriteStartToken(fil,'SELECTION',level,'FALSE');
      End;
      WriteEndToken(fil,'SELECTION',level);

      If Current^.Child<>NIL Then
      Begin
        WriteRecord(fil,Current^.Child, level+1);
      End;
      WriteEndToken(fil,'RECORD',level);
      current:=current^.Next;
    End;


  End;

Var udfil:Text;
Begin
  Try
    SaveToFile:=false;
    AssignFile(udfil,filnavn);
    ReWrite(udfil);
    Try
      writeln(udfil,'<STRUKTURE>');
      writerecord(udfil,root,0);
      writeln(udfil,'</STRUKTURE>');
    Finally
      Close(udfil);
    End;
  Except
    ShowMessage('Kunne ikke åbne fil');
    SaveToFile:=false;
  End;
End;

{********************************************************************
Description:                                                        *
Inputs:                                                             *
Outputs:                                                            *
********************************************************************}
Function TDiagramhandler.ReadFromFile(filnavn:string):boolean;

  type
    tag_tupple = record
      tagname:string;
      tag_id:integer;
    end;

  const
    TagType_unknown = 0;
    TagType_generic = 1;
    TagType_selection = 2;
    TagType_loop = 3;
    TagType_folded = 4;

    linetype_unknown = 0;
    linetype_record = 1;
    linetype_record_end = 2;
    linetype_tag = 3;
    linetype_tag_end = 4;
    linetype_fileid = 3;
    linetype_fileid_end = 4;

  Type TokenRead= Record
                    indent_level:integer;
                    TokenName:String;
                    EndToken:Boolean;
                    MalFormed:Boolean;
                    linetype:integer;
                    tagtype:integer;
                  End;
    TagNamelist = array [0..4] of tag_tupple := (("SLECTION", TagType_selection

  function find_indentation_level(var indstr:string):integer;
  var len_of_str,i,indent_level:integer;
      ok_to_search:Boolean;
  begin
    indent_level:=0;
    len_of_str:=length(indstr);

    i:=1;
    if len_of_str > 1 then ok_to_search := True else ok_to_search := False;
    while( (i <= len_of_str) and (ok_to_search)) do
    begin
      if indstr[i] <>' ' then
      begin
        ok_to_search:=False;
      end
      else
      begin
        i:=i+1;
      end;
    end;
    if i>2 then indstr:=Copy(indstr, i, (len_of_str - i) +1);
    showmessage('Indent i='+IntToStr(i));
    indent_level := (i - 1) div 2;
    Result := indent_level;
  end;

//  function Readdata(Var fil:Text):string;
  function Readdata(Var fil:Text):TokenRead;
  Var DataAvailable:boolean;
      tempstr, dumpstr:string;
      c:char;
  Begin
    DataAvailable:=True;
    tempstr:='';
    While (not(EOF(fil))) and ((DataAvailable)) do
    Begin
      read(fil,c);
      If (c='"') Then
      Begin
        DataAvailable:=true;
      End
      Else
      Begin
        If ((c='"') or (c=#10) or (c=#13)) and DataAvailable Then
        Begin
          DataAvailable:=false;
          readln(fil,dumpstr);
        End
        Else
        Begin
          if DataAvailable Then
          Begin
            tempstr:=tempstr+c;
          End;
        End;
      End;
    End;
//    showmessage('readdata = "'+ tempstr + '"');
    Result.indent_level:= find_indentation_level(tempstr);
    showmessage('readdata = "'+ tempstr + '"');
    Result.TokenName:=tempstr;
  End;

  function determine_line_type

  Function readtoken(text_line:string; current_indent_level:integer):TokenRead;
//  Function readtoken(var fil:Text; current_indent_level:integer):TokenRead;
  Var TokenIn:TokenRead;
      indent_level:Integer;
    procedure invalidtoken;
    Begin
      TokenIn.Malformed:=True;
      TokenIn.EndToken:=false;
      TokenIn.TokenName:='';
    End;
  Begin
    TokenIn.TokenName := text_line;
    //Readln(fil,TokenIn.TokenName);
    TokenIn.indent_level:= find_indentation_level(TokenIn.TokenName);
    If length(TokenIn.TokenName)>3 Then
    Begin
      ShowMessage('token.tokenname > 3chars "'+TokenIn.TokenName+'" -> lenght='+ IntToStr(length(TokenIn.TokenName)));

      If (TokenIn.TokenName[1]='<') Then
      Begin
        If (TokenIn.TokenName[length(TokenIn.TokenName)]='>') Then
        Begin
          TokenIn.MalFormed:=False;
          If (TokenIn.TokenName[2]='/') Then
          Begin
            TokenIn.EndToken:=True;
            TokenIn.TokenName:=AnsiUpperCase(copy(TokenIn.TokenName,3,
                                             length(TokenIn.TokenName)-3));
          End
          Else
          Begin
            TokenIn.EndToken:=False;
            TokenIn.TokenName:=AnsiUpperCase(copy(TokenIn.TokenName,2,
                                             length(TokenIn.TokenName)-2));
          End;
        End
        Else
        Begin
          TokenIn.EndToken:=False;
          invalidtoken;
          ShowMessage('Invalid token due to end bracket');
        End;
      End
      Else
      Begin
        TokenIn.EndToken:=False;
        ShowMessage('Invalid token due to start bracket $'+
                     IntToHex( byte(TokenIn.TokenName[1]),2) +
                     '="' + TokenIn.TokenName[1] + '"');
        //ShowMessage('Invalid token due to start bracket lenght='+ IntToStr( length(TokenIn.TokenName)));
        invalidtoken;
      End;
    End
    Else
    Begin
      TokenIn.EndToken:=False;
      invalidtoken;
      ShowMessage('Invalid token due length of token and brackets <= 3 "'+
                  TokenIn.TokenName + '"');
    End;
    ReadToken:=TokenIn;
  End;


Function ReadRecord(var fil:Text; current:p_TBlock;
                    var current_indent_level:integer):p_TBlock;
Var RecordRead:p_TBlock;
    CurrentToken:TokenRead;
    EndToken:TokenRead;
    datastr:String;
Begin
  RecordRead:=NIL;
  If current<>NIL Then
  Begin
    Repeat
      showmessage ('Expects start token');
      CurrentToken:=ReadToken(fil,current_indent_level);
      If (CurrentToken.EndToken=FALSE) Then
      Begin
        If (CurrentToken.TokenName='RECORD') Then
        Begin
          showmessage ('Expects data line');
          datastr:=ReadData(fil);
          RecordRead:=insert(current,NIL,datastr);
          RecordRead:=ReadRecord(fil,FindLast(current^.Child) , current_indent_level);
          showmessage ('Expects end token');
          EndToken:=ReadToken(fil, current_indent_level);
        End
        Else
        Begin
          showmessage ('Expects data line');
          datastr:=ReadData(fil);
          showmessage ('Expects end token');
          EndToken:=ReadToken(fil, current_indent_level);
        End;
      end;
    Until ((CurrentToken.EndToken=TRUE) AND (CurrentToken.TokenName='RECORD')) OR
          (CurrentToken.MalFormed=TRUE);
  end;
  ReadRecord:=RecordRead;
End;

Var indfil:Text;
    TestToken:TokenRead;
    current_indent_level:integer;
Begin
  ReadFromFile:=false;
  current_indent_level := 0;
  Try
    AssignFile(indfil,filnavn);
    Reset(indfil);
    Try
      showmessage ('Expects start token');
      TestToken:=ReadToken(indfil, current_indent_level);
      ShowMessage('start token:"' + TestToken.TokenName + '"');
      If TestToken.MalFormed=false Then
      Begin
        if (TestToken.TokenName='STRUKTURE') Then
        Begin
          RemoveChildren(Root);
          Dispose(root);
          showmessage ('Read record (first)');
          Root:=ReadRecord(indfil,root, current_indent_level);
          if root=nil then
          Begin
            CreateRoot('Åbnede ikke?')
          End;
        End
        Else
        Begin
          ShowMessage('Filen er af en ukendt type. Det kan være en XML-fil til anden brug.');
        End;
      End
      Else
      Begin
        ShowMessage('Filen er ikke af en gyldig struktur. Evt. ukendt fil type???');
      End;

    Finally
      Close(indfil);
    End;
  Except
    ShowMessage('Kunne ikke åbne fil');
    ReadFromFile:=false;
  End;
End;


End. //implementation

