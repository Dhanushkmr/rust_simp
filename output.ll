; ModuleID = 'main'
source_filename = "main"

define i32 @main() {
entry:
  %var = alloca i32, align 4
  store i32 4, ptr %var, align 4
  store i32 4, ptr %var, align 4
  %var1 = alloca i32, align 4
  store i32 0, ptr %var1, align 4
  store i32 0, ptr %var1, align 4
  %var2 = alloca i32, align 4
  store i32 1, ptr %var2, align 4
  store i32 1, ptr %var2, align 4
  %var3 = alloca i32, align 4
  store i32 0, ptr %var3, align 4
  store i32 0, ptr %var3, align 4
  %var4 = alloca i32, align 4
  store i32 0, ptr %var4, align 4
  store i32 0, ptr %var4, align 4
  br label %cond

cond:                                             ; preds = %body, %entry
  %c = load i32, ptr %var3, align 4
  %x = load i32, ptr %var, align 4
  %slt = icmp slt i32 %c, %x
  br i1 %slt, label %body, label %end

body:                                             ; preds = %cond
  %f = load i32, ptr %var1, align 4
  store i32 %f, ptr %var4, align 4
  %s = load i32, ptr %var2, align 4
  store i32 %s, ptr %var1, align 4
  %t = load i32, ptr %var4, align 4
  %f5 = load i32, ptr %var1, align 4
  %add = add i32 %t, %f5
  store i32 %add, ptr %var2, align 4
  %c6 = load i32, ptr %var3, align 4
  %add7 = add i32 %c6, 1
  store i32 %add7, ptr %var3, align 4
  br label %cond

end:                                              ; preds = %cond
  %s8 = load i32, ptr %var2, align 4
  ret i32 %s8
}
