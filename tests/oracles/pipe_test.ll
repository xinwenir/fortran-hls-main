; ModuleID = 'tmp/dataflow.bc'
source_filename = "FIRModule"
target triple = "fpga64-xilinx-none"

; Function Attrs: nounwind uwtable
define void @_Z7pipe_test_([100 x i32]* %a) #0 {
label_0:
  %0 = alloca i32, i64 1, align 4
  %1 = alloca i32, i64 1, align 4
  store i32 1, i32* %1, align 4
  br label %label_1

label_1:                                          ; preds = %label_2, %label_0
  %2 = phi i32 [ %13, %label_2 ], [ 1, %label_0 ]
  %3 = phi i64 [ %14, %label_2 ], [ 100, %label_0 ]
  %4 = icmp sgt i64 %3, 0
  br i1 %4, label %label_2, label %label_3

label_2:                                          ; preds = %label_1
  store i32 %2, i32* %0, align 4
  %5 = load i32, i32* %1, align 4
  %6 = add i32 %5, 2
  %7 = sext i32 %5 to i64
  %8 = sub i64 %7, 1
  %9 = getelementptr [100 x i32], [100 x i32]* %a, i32 0, i64 %8
  store i32 %6, i32* %9, align 4
  %10 = load i32, i32* %1, align 4
  %11 = add i32 %10, 1
  store i32 %11, i32* %1, align 4
  %12 = load i32, i32* %0, align 4
  %13 = add i32 %12, 1
  %14 = sub i64 %3, 1
  br label %label_1, !llvm.loop !0

label_3:                                          ; preds = %label_1
  store i32 %2, i32* %0, align 4
  ret void
}

declare void @llvm.sideeffect() #2
declare token @llvm.directive.scope.entry() #3
declare void @llvm.directive.scope.exit(token) #3
declare void @llvm.fpga.set.stream.depth(...) #4

declare i32 @llvm.fpga.fifo.pop.i32.p0i32(i32* %V) #2
declare double @llvm.fpga.fifo.pop.f64.p0f64(double* %V) #2


declare i1 @llvm.fpga.fifo.not.full.p0i32(i32* nocapture) #1
  

declare i1 @llvm.fpga.fifo.not.empty.p0i32(i32* nocapture)  #1
  
declare i1 @llvm.fpga.fifo.not.empty.p0f64(double* nocapture)  #1
  
attributes #0 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="pipe_test" "fpga.top.func"="pipe_test" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!0 = distinct !{!0, !1}
!1 = !{!"llvm.loop.pipeline.enable", i32 76, i1 false, i8 -1}

attributes #1 = { inaccessiblememonly nounwind "xlx.port.bitwidth"="320000" }

attributes #2 = { inaccessiblememonly nounwind }

attributes #3 = { noduplicate nounwind }

attributes #4 = { nounwind }

attributes #5 = { argmemonly nounwind }
!2 = !{i32 1, !"wchar_size", i32 4}
!3 = !{!"clang version 7.0.0 "}
