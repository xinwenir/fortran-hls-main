; ModuleID = 'tmp/dataflow.bc'
source_filename = "FIRModule"
target triple = "fpga64-xilinx-none"

declare void @llvm.sideeffect() #4
declare token @llvm.directive.scope.entry() #5
declare void @llvm.directive.scope.exit(token) #5
declare void @llvm.fpga.set.stream.depth(...) #6

declare i32 @llvm.fpga.fifo.pop.i32.p0i32(i32* %V) #2
declare double @llvm.fpga.fifo.pop.f64.p0f64(double* %V) #2


declare i1 @llvm.fpga.fifo.not.full.p0i32(i32* nocapture) #1
  

declare i1 @llvm.fpga.fifo.not.empty.p0i32(i32* nocapture)  #1
  
declare i1 @llvm.fpga.fifo.not.empty.p0f64(double* nocapture)  #1
  
declare i32 @_start_df_call()

declare void @_end_df_call()

; Function Attrs: nounwind uwtable
define void @_Z7entry_([10000 x i32]* %a, [10000 x i32]* %b, [10000 x i32]* %c) #0 {
label_0:
  call void @llvm.sideeffect() #3 [ "xlx_axis"([10000 x i32]* %a, i1 false, i64 2, i64 -1, [0 x i8] zeroinitializer, [0 x i8] zeroinitializer) ]

  call void @llvm.sideeffect() #3 [ "xlx_m_axi"([10000 x i32]* %b, [5 x i8] c"gmem1", i64 -1, [5 x i8] c"slave", [0 x i8] zeroinitializer, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1) ]

  call void @llvm.sideeffect() #3 [ "xlx_m_axi"([10000 x i32]* %c, [5 x i8] c"gmem2", i64 -1, [5 x i8] c"slave", [0 x i8] zeroinitializer, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1, i64 -1) ]

  
  %0 = call token @llvm.directive.scope.entry() [ "xcl_outline"() ]
  call void @_Z7compute0_([10000 x i32]* %a, [10000 x i32]* %b)
  call void @llvm.directive.scope.exit(token %0)
  %1 = call token @llvm.directive.scope.entry() [ "xcl_outline"() ]
  call void @_Z7compute1_([10000 x i32]* %b, [10000 x i32]* %c)
  call void @llvm.directive.scope.exit(token %1)
  ret void
}

; Function Attrs: nounwind uwtable
define void @_Z7compute0_([10000 x i32]* %a, [10000 x i32]* %b) #1 {
label_0:
  %0 = alloca i32, i64 1, align 4
  br label %label_1

label_1:                                          ; preds = %label_2, %label_0
  %1 = phi i32 [ %14, %label_2 ], [ 1, %label_0 ]
  %2 = phi i64 [ %15, %label_2 ], [ 10000, %label_0 ]
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %label_2, label %label_3

label_2:                                          ; preds = %label_1
  store i32 %1, i32* %0, align 4
  %4 = load i32, i32* %0, align 4
  %5 = sext i32 %4 to i64
  %6 = sub i64 %5, 1
  %7 = getelementptr [10000 x i32], [10000 x i32]* %a, i32 0, i64 %6
  %8 = load i32, i32* %7, align 4
  %9 = mul i32 %8, 13413
  %10 = mul i32 %9, %8
  %11 = mul i32 %10, %8
  %12 = getelementptr [10000 x i32], [10000 x i32]* %b, i32 0, i64 %6
  store i32 %11, i32* %12, align 4
  %13 = load i32, i32* %0, align 4
  %14 = add i32 %13, 1
  %15 = sub i64 %2, 1
  br label %label_1

label_3:                                          ; preds = %label_1
  store i32 %1, i32* %0, align 4
  ret void
}

; Function Attrs: nounwind uwtable
define void @_Z7compute1_([10000 x i32]* %a, [10000 x i32]* %b) #2 {
label_0:
  %0 = alloca i32, i64 1, align 4
  br label %label_1

label_1:                                          ; preds = %label_2, %label_0
  %1 = phi i32 [ %13, %label_2 ], [ 1, %label_0 ]
  %2 = phi i64 [ %14, %label_2 ], [ 10000, %label_0 ]
  %3 = icmp sgt i64 %2, 0
  br i1 %3, label %label_2, label %label_3

label_2:                                          ; preds = %label_1
  store i32 %1, i32* %0, align 4
  %4 = load i32, i32* %0, align 4
  %5 = sext i32 %4 to i64
  %6 = sub i64 %5, 1
  %7 = getelementptr [10000 x i32], [10000 x i32]* %a, i32 0, i64 %6
  %8 = load i32, i32* %7, align 4
  %9 = mul i32 %8, 1234123
  %10 = mul i32 %9, %8
  %11 = getelementptr [10000 x i32], [10000 x i32]* %b, i32 0, i64 %6
  store i32 %10, i32* %11, align 4
  %12 = load i32, i32* %0, align 4
  %13 = add i32 %12, 1
  %14 = sub i64 %2, 1
  br label %label_1

label_3:                                          ; preds = %label_1
  store i32 %1, i32* %0, align 4
  ret void
}

declare void @_Z7_interface_axis_none_([10000 x i32]*)

declare void @_Z7_interface_maxi_gmem1_([10000 x i32]*)

declare void @_Z7_interface_maxi_gmem2_([10000 x i32]*)

declare void @_Z7_dataflow_()

attributes #0 = {  nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="entry" "fpga.top.func"="entry" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false"  "fpga.dataflow.func"="0" }
attributes #1 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="compute0" "fpga.top.func"="compute0" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="compute1" "fpga.top.func"="compute1" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

attributes #3 = { inaccessiblememonly nounwind "xlx.port.bitwidth"="320000" }

attributes #4 = { inaccessiblememonly nounwind }

attributes #5 = { noduplicate nounwind }

attributes #6 = { nounwind }

attributes #7 = { argmemonly nounwind }
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.0.0 "}
