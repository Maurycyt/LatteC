declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @_concatStr(i8*, i8*)
declare i8* @_calloc(i32, i32)
declare void @_free(i8*)

@_g0 = constant [12 x i8] c"\49\27\6d\20\61\20\73\68\61\70\65\00"
@_g1 = constant [17 x i8] c"\49\27\6d\20\6a\75\73\74\20\61\20\73\68\61\70\65\00"
@_g3 = constant [20 x i8] c"\49\27\6d\20\72\65\61\6c\6c\79\20\61\20\63\69\72\63\6c\65\00"
@_g2 = constant [23 x i8] c"\49\27\6d\20\72\65\61\6c\6c\79\20\61\20\72\65\63\74\61\6e\67\6c\65\00"
@_g4 = constant [20 x i8] c"\49\27\6d\20\72\65\61\6c\6c\79\20\61\20\73\71\75\61\72\65\00"

%Square = type {%Square_vtable_type*}
%Square_vtable_type = type {void(%Shape*)*, void(%Square*)*}
@Square_vtable_data = global %Square_vtable_type{void(%Shape*)* @__Shape_tell, void(%Square*)* @__Square_tellAgain}
define void @_Square_constructor(%Square* %self) {
B0:
  %_r55 = getelementptr %Square, %Square* %self ,  i32 0, i32 0
  store %Square_vtable_type* @Square_vtable_data, %Square_vtable_type** %_r55
  ret void
}

%Circle = type {%Circle_vtable_type*}
%Circle_vtable_type = type {void(%Shape*)*, void(%Circle*)*}
@Circle_vtable_data = global %Circle_vtable_type{void(%Shape*)* @__Shape_tell, void(%Circle*)* @__Circle_tellAgain}
define void @_Circle_constructor(%Circle* %self) {
B0:
  %_r52 = getelementptr %Circle, %Circle* %self ,  i32 0, i32 0
  store %Circle_vtable_type* @Circle_vtable_data, %Circle_vtable_type** %_r52
  ret void
}

%Rectangle = type {%Rectangle_vtable_type*}
%Rectangle_vtable_type = type {void(%Shape*)*, void(%Rectangle*)*}
@Rectangle_vtable_data = global %Rectangle_vtable_type{void(%Shape*)* @__Shape_tell, void(%Rectangle*)* @__Rectangle_tellAgain}
define void @_Rectangle_constructor(%Rectangle* %self) {
B0:
  %_r49 = getelementptr %Rectangle, %Rectangle* %self ,  i32 0, i32 0
  store %Rectangle_vtable_type* @Rectangle_vtable_data, %Rectangle_vtable_type** %_r49
  ret void
}

%Shape = type {%Shape_vtable_type*}
%Shape_vtable_type = type {void(%Shape*)*, void(%Shape*)*}
@Shape_vtable_data = global %Shape_vtable_type{void(%Shape*)* @__Shape_tell, void(%Shape*)* @__Shape_tellAgain}
define void @_Shape_constructor(%Shape* %self) {
B0:
  %_r46 = getelementptr %Shape, %Shape* %self ,  i32 0, i32 0
  store %Shape_vtable_type* @Shape_vtable_data, %Shape_vtable_type** %_r46
  ret void
}

%Stack = type {%Stack_vtable_type*, %Node*}
%Stack_vtable_type = type {void(%Stack*, %Shape*)*, i1(%Stack*)*, %Shape*(%Stack*)*, void(%Stack*)*}
@Stack_vtable_data = global %Stack_vtable_type{void(%Stack*, %Shape*)* @__Stack_push, i1(%Stack*)* @__Stack_isEmpty, %Shape*(%Stack*)* @__Stack_top, void(%Stack*)* @__Stack_pop}
define void @_Stack_constructor(%Stack* %self) {
B0:
  %_r41 = getelementptr %Stack, %Stack* %self ,  i32 0, i32 0
  store %Stack_vtable_type* @Stack_vtable_data, %Stack_vtable_type** %_r41
  ret void
}

%Node = type {%Node_vtable_type*, %Shape*, %Node*}
%Node_vtable_type = type {void(%Node*, %Shape*)*, void(%Node*, %Node*)*, %Shape*(%Node*)*, %Node*(%Node*)*}
@Node_vtable_data = global %Node_vtable_type{void(%Node*, %Shape*)* @__Node_setElem, void(%Node*, %Node*)* @__Node_setNext, %Shape*(%Node*)* @__Node_getElem, %Node*(%Node*)* @__Node_getNext}
define void @_Node_constructor(%Node* %self) {
B0:
  %_r6 = getelementptr %Node, %Node* %self ,  i32 0, i32 0
  store %Node_vtable_type* @Node_vtable_data, %Node_vtable_type** %_r6
  ret void
}

define i32 @main() {
B0:
  %_r56 = call i8* @_calloc(i32 16, i32 1)
  %_r57 = bitcast i8* %_r56 to %Stack*
  call void @_Stack_constructor(%Stack* %_r57)
  %_r58 = call i8* @_calloc(i32 8, i32 1)
  %_r59 = bitcast i8* %_r58 to %Shape*
  call void @_Shape_constructor(%Shape* %_r59)
  %_r60 = getelementptr %Stack, %Stack* %_r57 ,  i32 0, i32 0
  %_r61 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r62 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r61 ,  i32 0, i32 0
  %_r63 = load void(%Stack*, %Shape*)*, void(%Stack*, %Shape*)** %_r62
  call void %_r63(%Stack* %_r57, %Shape* %_r59)
  %_r65 = call i8* @_calloc(i32 8, i32 1)
  %_r66 = bitcast i8* %_r65 to %Rectangle*
  call void @_Rectangle_constructor(%Rectangle* %_r66)
  %_r67 = bitcast %Rectangle* %_r66 to %Shape*
  %_r69 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r70 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r69 ,  i32 0, i32 0
  %_r71 = load void(%Stack*, %Shape*)*, void(%Stack*, %Shape*)** %_r70
  call void %_r71(%Stack* %_r57, %Shape* %_r67)
  %_r73 = call i8* @_calloc(i32 8, i32 1)
  %_r74 = bitcast i8* %_r73 to %Square*
  call void @_Square_constructor(%Square* %_r74)
  %_r75 = bitcast %Square* %_r74 to %Shape*
  %_r77 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r78 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r77 ,  i32 0, i32 0
  %_r79 = load void(%Stack*, %Shape*)*, void(%Stack*, %Shape*)** %_r78
  call void %_r79(%Stack* %_r57, %Shape* %_r75)
  %_r81 = call i8* @_calloc(i32 8, i32 1)
  %_r82 = bitcast i8* %_r81 to %Circle*
  call void @_Circle_constructor(%Circle* %_r82)
  %_r83 = bitcast %Circle* %_r82 to %Shape*
  %_r85 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r86 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r85 ,  i32 0, i32 0
  %_r87 = load void(%Stack*, %Shape*)*, void(%Stack*, %Shape*)** %_r86
  call void %_r87(%Stack* %_r57, %Shape* %_r83)
  br label %B2
B2:
  %_r109 = phi %Shape* [%_r83, %B0], [%_r93, %B1]
  %_r110 = getelementptr %Stack, %Stack* %_r57 ,  i32 0, i32 0
  %_r111 = load %Stack_vtable_type*, %Stack_vtable_type** %_r110
  %_r112 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r111 ,  i32 0, i32 1
  %_r113 = load i1(%Stack*)*, i1(%Stack*)** %_r112
  %_r114 = call i1 %_r113(%Stack* %_r57)
  %_r115 = icmp eq i1 false, %_r114
  br i1 %_r115 , label %B1 , label %B3
B1:
  %_r90 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r91 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r90 ,  i32 0, i32 2
  %_r92 = load %Shape*(%Stack*)*, %Shape*(%Stack*)** %_r91
  %_r93 = call %Shape* %_r92(%Stack* %_r57)
  %_r94 = getelementptr %Shape, %Shape* %_r93 ,  i32 0, i32 0
  %_r95 = load %Shape_vtable_type*, %Shape_vtable_type** %_r94
  %_r96 = getelementptr %Shape_vtable_type, %Shape_vtable_type* %_r95 ,  i32 0, i32 0
  %_r97 = load void(%Shape*)*, void(%Shape*)** %_r96
  call void %_r97(%Shape* %_r93)
  %_r100 = load %Shape_vtable_type*, %Shape_vtable_type** %_r94
  %_r101 = getelementptr %Shape_vtable_type, %Shape_vtable_type* %_r100 ,  i32 0, i32 1
  %_r102 = load void(%Shape*)*, void(%Shape*)** %_r101
  call void %_r102(%Shape* %_r93)
  %_r105 = load %Stack_vtable_type*, %Stack_vtable_type** %_r60
  %_r106 = getelementptr %Stack_vtable_type, %Stack_vtable_type* %_r105 ,  i32 0, i32 3
  %_r107 = load void(%Stack*)*, void(%Stack*)** %_r106
  call void %_r107(%Stack* %_r57)
  br label %B2
B3:
  ret i32 0
}

define void @__Square_tellAgain(%Square* %self) {
B0:
  %_r53 = bitcast [20 x i8]* @_g4 to i8*
  call void @printString(i8* %_r53)
  ret void
}

define void @__Circle_tellAgain(%Circle* %self) {
B0:
  %_r50 = bitcast [20 x i8]* @_g3 to i8*
  call void @printString(i8* %_r50)
  ret void
}

define void @__Rectangle_tellAgain(%Rectangle* %self) {
B0:
  %_r47 = bitcast [23 x i8]* @_g2 to i8*
  call void @printString(i8* %_r47)
  ret void
}

define void @__Shape_tellAgain(%Shape* %self) {
B0:
  %_r44 = bitcast [17 x i8]* @_g1 to i8*
  call void @printString(i8* %_r44)
  ret void
}

define void @__Shape_tell(%Shape* %self) {
B0:
  %_r42 = bitcast [12 x i8]* @_g0 to i8*
  call void @printString(i8* %_r42)
  ret void
}

define void @__Stack_pop(%Stack* %self) {
B0:
  %_r33 = getelementptr %Stack, %Stack* %self ,  i32 0, i32 1
  %_r35 = load %Node*, %Node** %_r33
  %_r36 = getelementptr %Node, %Node* %_r35 ,  i32 0, i32 0
  %_r37 = load %Node_vtable_type*, %Node_vtable_type** %_r36
  %_r38 = getelementptr %Node_vtable_type, %Node_vtable_type* %_r37 ,  i32 0, i32 3
  %_r39 = load %Node*(%Node*)*, %Node*(%Node*)** %_r38
  %_r40 = call %Node* %_r39(%Node* %_r35)
  store %Node* %_r40, %Node** %_r33
  ret void
}

define %Shape* @__Stack_top(%Stack* %self) {
B0:
  %_r26 = getelementptr %Stack, %Stack* %self ,  i32 0, i32 1
  %_r27 = load %Node*, %Node** %_r26
  %_r28 = getelementptr %Node, %Node* %_r27 ,  i32 0, i32 0
  %_r29 = load %Node_vtable_type*, %Node_vtable_type** %_r28
  %_r30 = getelementptr %Node_vtable_type, %Node_vtable_type* %_r29 ,  i32 0, i32 2
  %_r31 = load %Shape*(%Node*)*, %Shape*(%Node*)** %_r30
  %_r32 = call %Shape* %_r31(%Node* %_r27)
  ret %Shape* %_r32
}

define i1 @__Stack_isEmpty(%Stack* %self) {
B0:
  %_r22 = getelementptr %Stack, %Stack* %self ,  i32 0, i32 1
  %_r23 = load %Node*, %Node** %_r22
  %_r24 = bitcast i32* null to %Node*
  %_r25 = icmp eq %Node* %_r23, %_r24
  ret i1 %_r25
}

define void @__Stack_push(%Stack* %self, %Shape* %c) {
B0:
  %_r7 = call i8* @_calloc(i32 24, i32 1)
  %_r8 = bitcast i8* %_r7 to %Node*
  call void @_Node_constructor(%Node* %_r8)
  %_r9 = getelementptr %Node, %Node* %_r8 ,  i32 0, i32 0
  %_r10 = load %Node_vtable_type*, %Node_vtable_type** %_r9
  %_r11 = getelementptr %Node_vtable_type, %Node_vtable_type* %_r10 ,  i32 0, i32 0
  %_r12 = load void(%Node*, %Shape*)*, void(%Node*, %Shape*)** %_r11
  call void %_r12(%Node* %_r8, %Shape* %c)
  %_r15 = load %Node_vtable_type*, %Node_vtable_type** %_r9
  %_r16 = getelementptr %Node_vtable_type, %Node_vtable_type* %_r15 ,  i32 0, i32 1
  %_r17 = load void(%Node*, %Node*)*, void(%Node*, %Node*)** %_r16
  %_r18 = getelementptr %Stack, %Stack* %self ,  i32 0, i32 1
  %_r19 = load %Node*, %Node** %_r18
  call void %_r17(%Node* %_r8, %Node* %_r19)
  store %Node* %_r8, %Node** %_r18
  ret void
}

define %Node* @__Node_getNext(%Node* %self) {
B0:
  %_r4 = getelementptr %Node, %Node* %self ,  i32 0, i32 2
  %_r5 = load %Node*, %Node** %_r4
  ret %Node* %_r5
}

define %Shape* @__Node_getElem(%Node* %self) {
B0:
  %_r2 = getelementptr %Node, %Node* %self ,  i32 0, i32 1
  %_r3 = load %Shape*, %Shape** %_r2
  ret %Shape* %_r3
}

define void @__Node_setNext(%Node* %self, %Node* %n) {
B0:
  %_r1 = getelementptr %Node, %Node* %self ,  i32 0, i32 2
  store %Node* %n, %Node** %_r1
  ret void
}

define void @__Node_setElem(%Node* %self, %Shape* %c) {
B0:
  %_r0 = getelementptr %Node, %Node* %self ,  i32 0, i32 1
  store %Shape* %c, %Shape** %_r0
  ret void
}