declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @_concatStr(i8*, i8*)
declare i8* @_calloc(i32, i32)
declare void @_free(i8*)





define i32 @main() {
B0:
  %_r64 = call i8* @_calloc(i32 12, i32 1)
  %_r65 = bitcast i8* %_r64 to {i32, i32*}*
  %_r67 = call i8* @_calloc(i32 20, i32 1)
  %_r68 = bitcast i8* %_r67 to i32*
  %_r69 = getelementptr {i32, i32*}, {i32, i32*}* %_r65 ,  i32 0, i32 1
  store i32* %_r68, i32** %_r69
  %_r70 = getelementptr {i32, i32*}, {i32, i32*}* %_r65 ,  i32 0, i32 0
  store i32 5, i32* %_r70
  br label %B2
B2:
  %_r75 = phi i32 [0, %B0], [%_r74, %B1]
  %_r76 = getelementptr {i32, i32*}, {i32, i32*}* %_r65 ,  i32 0, i32 0
  %_r77 = load i32, i32* %_r76
  %_r78 = icmp slt i32 %_r75, %_r77
  br i1 %_r78 , label %B1 , label %B3
B1:
  %_r72 = load i32*, i32** %_r69
  %_r73 = getelementptr i32, i32* %_r72 ,  i32 %_r75
  store i32 %_r75, i32* %_r73
  %_r74 = add i32 %_r75, 1
  br label %B2
B3:
  call void @shiftLeft({i32, i32*}* %_r65)
  %_r80 = call {i32, i32*}* @doubleArray({i32, i32*}* %_r65)
  br label %B5
B5:
  %_r87 = phi i32 [0, %B3], [%_r86, %B4]
  %_r89 = load i32, i32* %_r70
  %_r90 = icmp slt i32 %_r87, %_r89
  br i1 %_r90 , label %B4 , label %B6
B4:
  %_r82 = load i32*, i32** %_r69
  %_r83 = getelementptr i32, i32* %_r82 ,  i32 %_r87
  %_r84 = load i32, i32* %_r83
  call void @printInt(i32 %_r84)
  %_r86 = add i32 %_r87, 1
  br label %B5
B6:
  br label %B8
B8:
  %_r97 = phi i32 [0, %B6], [%_r96, %B7]
  %_r98 = getelementptr {i32, i32*}, {i32, i32*}* %_r80 ,  i32 0, i32 0
  %_r99 = load i32, i32* %_r98
  %_r100 = icmp slt i32 %_r97, %_r99
  br i1 %_r100 , label %B7 , label %B9
B7:
  %_r91 = getelementptr {i32, i32*}, {i32, i32*}* %_r80 ,  i32 0, i32 1
  %_r92 = load i32*, i32** %_r91
  %_r93 = getelementptr i32, i32* %_r92 ,  i32 %_r97
  %_r94 = load i32, i32* %_r93
  call void @printInt(i32 %_r94)
  %_r96 = add i32 %_r97, 1
  br label %B8
B9:
  %_r101 = call i32 @scalProd({i32, i32*}* %_r65, {i32, i32*}* %_r80)
  call void @printInt(i32 %_r101)
  ret i32 0
}

define i32 @scalProd({i32, i32*}* %a, {i32, i32*}* %b) {
B0:
  br label %B2
B2:
  %_r60 = phi i32 [0, %B0], [%_r57, %B1]
  %_r59 = phi i32 [0, %B0], [%_r58, %B1]
  %_r61 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 0
  %_r62 = load i32, i32* %_r61
  %_r63 = icmp slt i32 %_r59, %_r62
  br i1 %_r63 , label %B1 , label %B3
B1:
  %_r48 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 1
  %_r49 = load i32*, i32** %_r48
  %_r50 = getelementptr i32, i32* %_r49 ,  i32 %_r59
  %_r51 = load i32, i32* %_r50
  %_r52 = getelementptr {i32, i32*}, {i32, i32*}* %b ,  i32 0, i32 1
  %_r53 = load i32*, i32** %_r52
  %_r54 = getelementptr i32, i32* %_r53 ,  i32 %_r59
  %_r55 = load i32, i32* %_r54
  %_r56 = mul i32 %_r51, %_r55
  %_r57 = add i32 %_r60, %_r56
  %_r58 = add i32 %_r59, 1
  br label %B2
B3:
  ret i32 %_r60
}

define void @shiftLeft({i32, i32*}* %a) {
B0:
  %_r24 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 1
  %_r25 = load i32*, i32** %_r24
  %_r26 = getelementptr i32, i32* %_r25 ,  i32 0
  %_r27 = load i32, i32* %_r26
  br label %B2
B2:
  %_r37 = phi i32 [0, %B0], [%_r31, %B1]
  %_r38 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 0
  %_r39 = load i32, i32* %_r38
  %_r40 = sub i32 %_r39, 1
  %_r41 = icmp slt i32 %_r37, %_r40
  br i1 %_r41 , label %B1 , label %B3
B1:
  %_r29 = load i32*, i32** %_r24
  %_r30 = getelementptr i32, i32* %_r29 ,  i32 %_r37
  %_r31 = add i32 %_r37, 1
  %_r33 = load i32*, i32** %_r24
  %_r34 = getelementptr i32, i32* %_r33 ,  i32 %_r31
  %_r35 = load i32, i32* %_r34
  store i32 %_r35, i32* %_r30
  br label %B2
B3:
  %_r43 = load i32, i32* %_r38
  %_r44 = sub i32 %_r43, 1
  %_r46 = load i32*, i32** %_r24
  %_r47 = getelementptr i32, i32* %_r46 ,  i32 %_r44
  store i32 %_r27, i32* %_r47
  ret void
}

define {i32, i32*}* @doubleArray({i32, i32*}* %a) {
B0:
  %_r0 = call i8* @_calloc(i32 12, i32 1)
  %_r1 = bitcast i8* %_r0 to {i32, i32*}*
  %_r2 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 0
  %_r3 = load i32, i32* %_r2
  %_r4 = mul i32 %_r3, 4
  %_r5 = call i8* @_calloc(i32 %_r4, i32 1)
  %_r6 = bitcast i8* %_r5 to i32*
  %_r7 = getelementptr {i32, i32*}, {i32, i32*}* %_r1 ,  i32 0, i32 1
  store i32* %_r6, i32** %_r7
  %_r8 = getelementptr {i32, i32*}, {i32, i32*}* %_r1 ,  i32 0, i32 0
  store i32 %_r3, i32* %_r8
  br label %B2
B2:
  %_r20 = phi i32 [0, %B0], [%_r17, %B1]
  %_r19 = phi i32 [0, %B0], [%_r18, %B1]
  %_r21 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 0
  %_r22 = load i32, i32* %_r21
  %_r23 = icmp slt i32 %_r19, %_r22
  br i1 %_r23 , label %B1 , label %B3
B1:
  %_r9 = getelementptr {i32, i32*}, {i32, i32*}* %a ,  i32 0, i32 1
  %_r10 = load i32*, i32** %_r9
  %_r11 = getelementptr i32, i32* %_r10 ,  i32 %_r19
  %_r12 = load i32, i32* %_r11
  %_r14 = load i32*, i32** %_r7
  %_r15 = getelementptr i32, i32* %_r14 ,  i32 %_r20
  %_r16 = mul i32 2, %_r12
  store i32 %_r16, i32* %_r15
  %_r17 = add i32 %_r20, 1
  %_r18 = add i32 %_r19, 1
  br label %B2
B3:
  ret {i32, i32*}* %_r1
}