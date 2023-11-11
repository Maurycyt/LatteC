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
  %_r0 = call i8* @_calloc(i32 12, i32 1)
  %_r1 = bitcast i8* %_r0 to {i32, i32*}*
  %_r3 = call i8* @_calloc(i32 40, i32 1)
  %_r4 = bitcast i8* %_r3 to i32*
  %_r5 = getelementptr {i32, i32*}, {i32, i32*}* %_r1 ,  i32 0, i32 1
  store i32* %_r4, i32** %_r5
  %_r6 = getelementptr {i32, i32*}, {i32, i32*}* %_r1 ,  i32 0, i32 0
  store i32 10, i32* %_r6
  br label %B2
B2:
  %_r11 = phi i32 [0, %B0], [%_r10, %B1]
  %_r12 = getelementptr {i32, i32*}, {i32, i32*}* %_r1 ,  i32 0, i32 0
  %_r13 = load i32, i32* %_r12
  %_r14 = icmp slt i32 %_r11, %_r13
  br i1 %_r14 , label %B1 , label %B3
B1:
  %_r8 = load i32*, i32** %_r5
  %_r9 = getelementptr i32, i32* %_r8 ,  i32 %_r11
  store i32 %_r11, i32* %_r9
  %_r10 = add i32 %_r11, 1
  br label %B2
B3:
  br label %B5
B5:
  %_r21 = phi i32 [0, %B3], [%_r20, %B4]
  %_r23 = load i32, i32* %_r6
  %_r24 = icmp slt i32 %_r21, %_r23
  br i1 %_r24 , label %B4 , label %B6
B4:
  %_r16 = load i32*, i32** %_r5
  %_r17 = getelementptr i32, i32* %_r16 ,  i32 %_r21
  %_r18 = load i32, i32* %_r17
  call void @printInt(i32 %_r18)
  %_r20 = add i32 %_r21, 1
  br label %B5
B6:
  call void @printInt(i32 45)
  ret i32 0
}