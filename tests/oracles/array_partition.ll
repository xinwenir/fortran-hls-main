; ModuleID = 'tmp/dataflow.bc'
source_filename = "FIRModule"
target triple = "fpga64-xilinx-none"

; Function Attrs: nounwind uwtable
define void @_Z7array_partition_([123 x i32]* %a) #0 {
label_0:
  %0 = alloca [123 x [123 x i32]], i64 1, align 4
  %1 = alloca i32, i64 1, align 4
  %2 = alloca [123 x i32], i64 1, align 4
  %3 = alloca [123 x i32], i64 1, align 4
  %4 = alloca [123 x i32], i64 1, align 4
  %5 = alloca [123 x i32], i64 1, align 4
  call void @llvm.sideeffect() #2 [ "xlx_array_partition"([123 x i32]* %4, i32 0, i32 67, i32 1, i1 false) ]
  call void @llvm.sideeffect() #2 [ "xlx_array_partition"([123 x i32]* %5, i32 1, i32 12, i32 1, i1 false) ]
  call void @llvm.sideeffect() #2 [ "xlx_array_partition"([123 x i32]* %2, i32 2, i32 0, i32 1, i1 false) ]
  call void @llvm.sideeffect() #2 [ "xlx_array_partition"([123 x [123 x i32]]* %0, i32 1, i32 22, i32 1, i1 false) ]
  br label %label_1

label_1:                                          ; preds = %label_2, %label_0
  %6 = phi i32 [ %30, %label_2 ], [ 1, %label_0 ]
  %7 = phi i64 [ %31, %label_2 ], [ 123, %label_0 ]
  %8 = icmp sgt i64 %7, 0
  br i1 %8, label %label_2, label %label_3

label_2:                                          ; preds = %label_1
  store i32 %6, i32* %1, align 4
  %9 = load i32, i32* %1, align 4
  %10 = sext i32 %9 to i64
  %11 = sub i64 %10, 1
  %12 = getelementptr [123 x i32], [123 x i32]* %3, i32 0, i64 %11
  store i32 %9, i32* %12, align 4
  %13 = load i32, i32* %1, align 4
  %14 = sext i32 %13 to i64
  %15 = sub i64 %14, 1
  %16 = getelementptr [123 x i32], [123 x i32]* %4, i32 0, i64 %15
  store i32 %13, i32* %16, align 4
  %17 = load i32, i32* %1, align 4
  %18 = sext i32 %17 to i64
  %19 = sub i64 %18, 1
  %20 = getelementptr [123 x i32], [123 x i32]* %5, i32 0, i64 %19
  store i32 %17, i32* %20, align 4
  %21 = load i32, i32* %1, align 4
  %22 = sext i32 %21 to i64
  %23 = sub i64 %22, 1
  %24 = getelementptr [123 x i32], [123 x i32]* %2, i32 0, i64 %23
  store i32 %21, i32* %24, align 4
  %25 = load i32, i32* %1, align 4
  %26 = sext i32 %25 to i64
  %27 = sub i64 %26, 1
  %28 = getelementptr [123 x [123 x i32]], [123 x [123 x i32]]* %0, i32 0, i64 -1, i64 %27
  store i32 %25, i32* %28, align 4
  %29 = load i32, i32* %1, align 4
  %30 = add i32 %29, 1
  %31 = sub i64 %7, 1
  br label %label_1

label_3:                                          ; preds = %label_1
  store i32 %6, i32* %1, align 4
  br label %label_4

label_4:                                          ; preds = %label_5, %label_3
  %32 = phi i32 [ %54, %label_5 ], [ 1, %label_3 ]
  %33 = phi i64 [ %55, %label_5 ], [ 123, %label_3 ]
  %34 = icmp sgt i64 %33, 0
  br i1 %34, label %label_5, label %label_6

label_5:                                          ; preds = %label_4
  store i32 %32, i32* %1, align 4
  %35 = load i32, i32* %1, align 4
  %36 = sext i32 %35 to i64
  %37 = sub i64 %36, 1
  %38 = getelementptr [123 x i32], [123 x i32]* %3, i32 0, i64 %37
  %39 = load i32, i32* %38, align 4
  %40 = getelementptr [123 x i32], [123 x i32]* %4, i32 0, i64 %37
  %41 = load i32, i32* %40, align 4
  %42 = add i32 %39, %41
  %43 = getelementptr [123 x i32], [123 x i32]* %5, i32 0, i64 %37
  %44 = load i32, i32* %43, align 4
  %45 = add i32 %42, %44
  %46 = getelementptr [123 x i32], [123 x i32]* %2, i32 0, i64 %37
  %47 = load i32, i32* %46, align 4
  %48 = add i32 %45, %47
  %49 = getelementptr [123 x [123 x i32]], [123 x [123 x i32]]* %0, i32 0, i64 -1, i64 %37
  %50 = load i32, i32* %49, align 4
  %51 = add i32 %48, %50
  %52 = getelementptr [123 x i32], [123 x i32]* %a, i32 0, i64 %37
  store i32 %51, i32* %52, align 4
  %53 = load i32, i32* %1, align 4
  %54 = add i32 %53, 1
  %55 = sub i64 %33, 1
  br label %label_4

label_6:                                          ; preds = %label_4
  store i32 %32, i32* %1, align 4
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
  
declare void @_Z7partition_cyclic_67_1_([123 x i32]*)

declare void @_Z7partition_block_12_1_([123 x i32]*)

declare void @_Z7partition_complete_0_1_([123 x i32]*)

declare void @_Z7partition_block_22_2_([123 x [123 x i32]]*)

attributes #0 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="array_partition" "fpga.top.func"="array_partition" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

attributes #1 = { inaccessiblememonly nounwind "xlx.port.bitwidth"="320000" }

attributes #2 = { inaccessiblememonly nounwind }

attributes #3 = { noduplicate nounwind }

attributes #4 = { nounwind }

attributes #5 = { argmemonly nounwind }
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.0.0 "}
