; ModuleID = 'tmp/dataflow.bc'
source_filename = "FIRModule"
target triple = "fpga64-xilinx-none"

%_QM__fortran_type_infoTcomponent = type { { i8*, i64, i32, i8, i8, i8, i8 }, i8, i8, i8, i8, [4 x i8], i64, %_QM__fortran_type_infoTvalue, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] }, %_QM__fortran_builtinsT__builtin_c_ptr }
%_QM__fortran_type_infoTvalue = type { i8, [7 x i8], i64 }
%_QM__fortran_type_infoTderivedtype = type { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] }, { i8*, i64, i32, i8, i8, i8, i8 }, i64, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] }, i32, i8, i8, i8, i8, [4 x i8] }
%_QM__fortran_type_infoTbinding = type { %_QM__fortran_builtinsT__builtin_c_funptr, { i8*, i64, i32, i8, i8, i8, i8 } }
%_QM__fortran_builtinsT__builtin_c_funptr = type { i64 }
%_QM__fortran_type_infoTprocptrcomponent = type { { i8*, i64, i32, i8, i8, i8, i8 }, i64, %_QM__fortran_builtinsT__builtin_c_funptr }
%_QM__fortran_type_infoTspecialbinding = type { i8, i8, [6 x i8], %_QM__fortran_builtinsT__builtin_c_funptr }
%_QM__fortran_builtinsT__builtin_c_ptr = type { i64 }
%_QMhls_streamThlsstream_integer = type { i32 }

@_QMhls_streamE.n.data_integer = linkonce_odr constant [12 x i8] c"data_integer"
@_QMhls_streamE.n.data_real = linkonce_odr constant [9 x i8] c"data_real"
@_QMhls_streamE.n.hlsstream = linkonce_odr constant [9 x i8] c"hlsstream"
@_QMhls_streamE.n.hlsstream_integer = linkonce_odr constant [17 x i8] c"hlsstream_integer"
@_QMhls_streamE.n.hlsstream_real = linkonce_odr constant [14 x i8] c"hlsstream_real"
@_QMhls_streamE.c.hlsstream = linkonce_odr constant [1 x %_QM__fortran_type_infoTcomponent] [%_QM__fortran_type_infoTcomponent { { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([12 x i8], [12 x i8]* @_QMhls_streamE.n.data_integer, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 12), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i8 1, i8 0, i8 4, i8 0, [4 x i8] undef, i64 0, %_QM__fortran_type_infoTvalue { i8 1, [7 x i8] undef, i64 0 }, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 2, i8 42, i8 1, i8 1, [2 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)], [3 x i64] [i64 1, i64 0, i64 0]], i8* null, [1 x i64] undef }, %_QM__fortran_builtinsT__builtin_c_ptr zeroinitializer }]
@_QMhls_streamE.c.hlsstream_integer = linkonce_odr constant [1 x %_QM__fortran_type_infoTcomponent] [%_QM__fortran_type_infoTcomponent { { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([12 x i8], [12 x i8]* @_QMhls_streamE.n.data_integer, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 12), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i8 1, i8 0, i8 4, i8 0, [4 x i8] undef, i64 0, %_QM__fortran_type_infoTvalue { i8 1, [7 x i8] undef, i64 0 }, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 2, i8 42, i8 1, i8 1, [2 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)], [3 x i64] [i64 1, i64 0, i64 0]], i8* null, [1 x i64] undef }, %_QM__fortran_builtinsT__builtin_c_ptr zeroinitializer }]
@_QMhls_streamE.c.hlsstream_real = linkonce_odr constant [1 x %_QM__fortran_type_infoTcomponent] [%_QM__fortran_type_infoTcomponent { { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([9 x i8], [9 x i8]* @_QMhls_streamE.n.data_real, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 9), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i8 1, i8 1, i8 8, i8 0, [4 x i8] undef, i64 0, %_QM__fortran_type_infoTvalue { i8 1, [7 x i8] undef, i64 0 }, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 2, i8 42, i8 1, i8 1, [2 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)], [3 x i64] [i64 1, i64 0, i64 0]], i8* null, [1 x i64] undef }, %_QM__fortran_builtinsT__builtin_c_ptr zeroinitializer }]
@_QMhls_streamE.dt.hlsstream = linkonce_odr constant %_QM__fortran_type_infoTderivedtype { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([9 x i8], [9 x i8]* @_QMhls_streamE.n.hlsstream, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 9), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i64 4, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i64* null, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i32 20180515, i8 1, i8 10, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i8* null, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i32 20180515, i8 1, i8 7, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTcomponent* getelementptr inbounds ([1 x %_QM__fortran_type_infoTcomponent], [1 x %_QM__fortran_type_infoTcomponent]* @_QMhls_streamE.c.hlsstream, i32 0, i32 0), i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 0, i64 1, i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTprocptrcomponent* null, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTspecialbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, i32 0, i8 0, i8 1, i8 1, i8 1, [4 x i8] undef }
@_QMhls_streamE.dt.hlsstream_integer = linkonce_odr constant %_QM__fortran_type_infoTderivedtype { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([17 x i8], [17 x i8]* @_QMhls_streamE.n.hlsstream_integer, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 17), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i64 4, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i64* null, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i32 20180515, i8 1, i8 10, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i8* null, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i32 20180515, i8 1, i8 7, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTcomponent* getelementptr inbounds ([1 x %_QM__fortran_type_infoTcomponent], [1 x %_QM__fortran_type_infoTcomponent]* @_QMhls_streamE.c.hlsstream_integer, i32 0, i32 0), i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 0, i64 1, i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTprocptrcomponent* null, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTspecialbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, i32 0, i8 0, i8 1, i8 1, i8 1, [4 x i8] undef }
@_QMhls_streamE.dt.hlsstream_real = linkonce_odr constant %_QM__fortran_type_infoTderivedtype { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([14 x i8], [14 x i8]* @_QMhls_streamE.n.hlsstream_real, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 14), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i64 8, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i64* null, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i32 20180515, i8 1, i8 10, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i8* null, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i32 20180515, i8 1, i8 7, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTcomponent* getelementptr inbounds ([1 x %_QM__fortran_type_infoTcomponent], [1 x %_QM__fortran_type_infoTcomponent]* @_QMhls_streamE.c.hlsstream_real, i32 0, i32 0), i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 0, i64 1, i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTprocptrcomponent* null, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTspecialbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, i32 0, i8 0, i8 1, i8 1, i8 1, [4 x i8] undef }
@_QM__fortran_builtinsE.dt.__builtin_c_funptr = linkonce_odr constant %_QM__fortran_type_infoTderivedtype { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([18 x i8], [18 x i8]* @_QM__fortran_builtinsE.n.__builtin_c_funptr, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 18), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i64 8, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i64* null, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i32 20180515, i8 1, i8 10, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i8* null, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i32 20180515, i8 1, i8 7, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTcomponent* getelementptr inbounds ([1 x %_QM__fortran_type_infoTcomponent], [1 x %_QM__fortran_type_infoTcomponent]* @_QM__fortran_builtinsE.c.__builtin_c_funptr, i32 0, i32 0), i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 0, i64 1, i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTprocptrcomponent* null, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTspecialbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, i32 0, i8 0, i8 1, i8 1, i8 1, [4 x i8] undef }
@_QM__fortran_builtinsE.n.__builtin_c_funptr = linkonce_odr constant [18 x i8] c"__builtin_c_funptr"
@_QM__fortran_builtinsE.c.__builtin_c_funptr = linkonce_odr constant [1 x %_QM__fortran_type_infoTcomponent] [%_QM__fortran_type_infoTcomponent { { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([9 x i8], [9 x i8]* @_QM__fortran_builtinsE.n.__address, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 9), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i8 1, i8 0, i8 8, i8 0, [4 x i8] undef, i64 0, %_QM__fortran_type_infoTvalue { i8 1, [7 x i8] undef, i64 0 }, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 2, i8 42, i8 1, i8 1, [2 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)], [3 x i64] [i64 1, i64 0, i64 0]], i8* null, [1 x i64] undef }, %_QM__fortran_builtinsT__builtin_c_ptr zeroinitializer }]
@_QM__fortran_builtinsE.n.__address = linkonce_odr constant [9 x i8] c"__address"
@_QM__fortran_builtinsE.dt.__builtin_c_ptr = linkonce_odr constant %_QM__fortran_type_infoTderivedtype { { %_QM__fortran_type_infoTbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTbinding* getelementptr (%_QM__fortran_type_infoTbinding, %_QM__fortran_type_infoTbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([15 x i8], [15 x i8]* @_QM__fortran_builtinsE.n.__builtin_c_ptr, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 15), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i64 8, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { i64*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i64* null, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64), i32 20180515, i8 1, i8 10, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i64* getelementptr (i64, i64* null, i32 1) to i64)]] }, { i8*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]] } { i8* null, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i32 20180515, i8 1, i8 7, i8 1, i8 0, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64)]] }, { %_QM__fortran_type_infoTcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTcomponent* getelementptr inbounds ([1 x %_QM__fortran_type_infoTcomponent], [1 x %_QM__fortran_type_infoTcomponent]* @_QM__fortran_builtinsE.c.__builtin_c_ptr, i32 0, i32 0), i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 0, i64 1, i64 ptrtoint (%_QM__fortran_type_infoTcomponent* getelementptr (%_QM__fortran_type_infoTcomponent, %_QM__fortran_type_infoTcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTprocptrcomponent*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTprocptrcomponent* null, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTprocptrcomponent* getelementptr (%_QM__fortran_type_infoTprocptrcomponent, %_QM__fortran_type_infoTprocptrcomponent* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTspecialbinding*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTspecialbinding* null, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTspecialbinding* getelementptr (%_QM__fortran_type_infoTspecialbinding, %_QM__fortran_type_infoTspecialbinding* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, i32 0, i8 0, i8 1, i8 1, i8 1, [4 x i8] undef }
@_QM__fortran_builtinsE.n.__builtin_c_ptr = linkonce_odr constant [15 x i8] c"__builtin_c_ptr"
@_QM__fortran_builtinsE.c.__builtin_c_ptr = linkonce_odr constant [1 x %_QM__fortran_type_infoTcomponent] [%_QM__fortran_type_infoTcomponent { { i8*, i64, i32, i8, i8, i8, i8 } { i8* getelementptr inbounds ([9 x i8], [9 x i8]* @_QM__fortran_builtinsE.n.__address, i32 0, i32 0), i64 mul (i64 ptrtoint (i8* getelementptr (i8, i8* null, i32 1) to i64), i64 9), i32 20180515, i8 0, i8 40, i8 1, i8 0 }, i8 1, i8 0, i8 8, i8 0, [4 x i8] undef, i64 0, %_QM__fortran_type_infoTvalue { i8 1, [7 x i8] undef, i64 0 }, { %_QM__fortran_type_infoTderivedtype*, i64, i32, i8, i8, i8, i8, i8*, [1 x i64] } { %_QM__fortran_type_infoTderivedtype* null, i64 ptrtoint (%_QM__fortran_type_infoTderivedtype* getelementptr (%_QM__fortran_type_infoTderivedtype, %_QM__fortran_type_infoTderivedtype* null, i32 1) to i64), i32 20180515, i8 0, i8 42, i8 1, i8 1, i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [1 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 1, i8 42, i8 1, i8 1, [1 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)]], i8* null, [1 x i64] undef }, { %_QM__fortran_type_infoTvalue*, i64, i32, i8, i8, i8, i8, [2 x [3 x i64]], i8*, [1 x i64] } { %_QM__fortran_type_infoTvalue* null, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64), i32 20180515, i8 2, i8 42, i8 1, i8 1, [2 x [3 x i64]] [[3 x i64] [i64 1, i64 0, i64 ptrtoint (%_QM__fortran_type_infoTvalue* getelementptr (%_QM__fortran_type_infoTvalue, %_QM__fortran_type_infoTvalue* null, i32 1) to i64)], [3 x i64] [i64 1, i64 0, i64 0]], i8* null, [1 x i64] undef }, %_QM__fortran_builtinsT__builtin_c_ptr zeroinitializer }]

define i32 @_Z7_QMhls_streamPhls_lowered_read_integer(i32* %a) {
label_0:
  %0 = alloca i32, i64 1, align 4
  %1 = load i32, i32* %0, align 4
  ret i32 %1
}

define void @_Z7_QMhls_streamPhls_lowered_write_integer(i32 %a, i32* %b) {
label_0:
  %0 = alloca i32, i64 1, align 4
  store i32 %a, i32* %0, align 4
  ret void
}

define i8 @_Z7_QMhls_streamPhls_lowered_full_integer(i32* %a) {
label_0:
  %0 = alloca i8, i64 1, align 1
  %1 = load i8, i8* %0, align 1
  ret i8 %1
}

define i8 @_Z7_QMhls_streamPhls_lowered_empty_integer(i32* %a) {
label_0:
  %0 = alloca i8, i64 1, align 1
  %1 = load i8, i8* %0, align 1
  ret i8 %1
}

define void @_Z7_QMhls_streamPset_depth_integer(i32* %a, i32 %b) {
label_0:
  %0 = alloca i32, i64 1, align 4
  store i32 %b, i32* %0, align 4
  ret void
}

define double @_Z7_QMhls_streamPhls_lowered_read_real(double* %a) {
label_0:
  %0 = alloca double, i64 1, align 8
  %1 = load double, double* %0, align 8
  ret double %1
}

define void @_Z7_QMhls_streamPhls_lowered_write_real(double %a, double* %b) {
label_0:
  %0 = alloca double, i64 1, align 8
  store double %a, double* %0, align 8
  ret void
}

define i8 @_Z7_QMhls_streamPhls_lowered_full_real(double* %a) {
label_0:
  %0 = alloca i8, i64 1, align 1
  %1 = load i8, i8* %0, align 1
  ret i8 %1
}

define i8 @_Z7_QMhls_streamPhls_lowered_empty_real(double* %a) {
label_0:
  %0 = alloca i8, i64 1, align 1
  %1 = load i8, i8* %0, align 1
  ret i8 %1
}

define void @_Z7_QMhls_streamPset_depth_real(double* %a, i32 %b) {
label_0:
  %0 = alloca i32, i64 1, align 4
  store i32 %b, i32* %0, align 4
  ret void
}

; Function Attrs: nounwind uwtable
define void @_Z7entry_([100 x i32]* %a, [100 x i32]* %b) #0 {
label_0:
  %0 = alloca float, i64 1, align 4
  %1 = alloca i32, i64 1, align 4
  %2 = alloca i32, i64 1, align 4
  %3 = alloca i32, i64 1, align 4
  %4 = alloca i32, i64 1, align 4
  %5 = alloca i32, i64 1, align 4
  %6 = alloca i32, i64 1, align 4
  %7 = alloca %_QMhls_streamThlsstream_integer, i64 1, align 8
  %8 = getelementptr %_QMhls_streamThlsstream_integer, %_QMhls_streamThlsstream_integer* %7, i32 0, i32 0
  call void (...) @llvm.fpga.set.stream.depth(i32* %8, i32 0)
  store i32 1234, i32* %2, align 4
  store i32 0, i32* %3, align 4
  store float 1.000000e+00, float* %0, align 4
  store i32 0, i32* %5, align 4
  br label %label_1

label_1:                                          ; preds = %label_2, %label_0
  %9 = phi i32 [ %14, %label_2 ], [ 1, %label_0 ]
  %10 = phi i64 [ %15, %label_2 ], [ 2, %label_0 ]
  %11 = icmp sgt i64 %10, 0
  br i1 %11, label %label_2, label %label_3

label_2:                                          ; preds = %label_1
  store i32 %9, i32* %1, align 4
  %12 = load i32, i32* %2, align 4
  call void @llvm.fpga.fifo.push.i32.p0i32(i32 %12, i32* %8) #6
  %13 = load i32, i32* %1, align 4
  %14 = add i32 %13, 1
  %15 = sub i64 %10, 1
  br label %label_1

label_3:                                          ; preds = %label_1
  store i32 %9, i32* %1, align 4
  br label %label_4

label_4:                                          ; preds = %label_5, %label_3
  %16 = load i32, i32* %3, align 4
  %17 = icmp eq i32 %16, 0
  br i1 %17, label %label_5, label %label_6

label_5:                                          ; preds = %label_4
  %18 = call i32 @llvm.fpga.fifo.pop.i32.p0i32(i32* %8)
  store i32 %18, i32* %6, align 4
  %19 = call i1 @llvm.fpga.fifo.not.empty.p0i32(i32* %8) #6
  %lnot.20 = xor i1 %19, true
  %20 = zext i1 %lnot.20 to i32
  store i32 %20, i32* %3, align 4
  br label %label_4

label_6:                                          ; preds = %label_4
  %21 = load i32, i32* %3, align 4
  %22 = icmp eq i32 %21, 1
  br i1 %22, label %label_7, label %label_8

label_7:                                          ; preds = %label_6
  %23 = getelementptr [100 x i32], [100 x i32]* %b, i32 0, i64 0
  store i32 412341, i32* %23, align 4
  br label %label_9

label_8:                                          ; preds = %label_6
  %24 = getelementptr [100 x i32], [100 x i32]* %b, i32 0, i64 0
  store i32 666, i32* %24, align 4
  br label %label_9

label_9:                                          ; preds = %label_8, %label_7
  %25 = load i32, i32* %3, align 4
  %26 = getelementptr [100 x i32], [100 x i32]* %b, i32 0, i64 1
  store i32 %25, i32* %26, align 4
  ret void
}

declare void @llvm.sideeffect() #2
declare token @llvm.directive.scope.entry() #3
declare void @llvm.directive.scope.exit(token) #3
declare void @llvm.fpga.set.stream.depth(...) #4

declare i32 @llvm.fpga.fifo.pop.i32.p0i32(i32* %V) #2
declare double @llvm.fpga.fifo.pop.f64.p0f64(double* %V) #2

declare void @llvm.fpga.fifo.push.i32.p0i32(i32, i32* nocapture) #2

declare i1 @llvm.fpga.fifo.not.full.p0i32(i32* nocapture) #1
  

declare i1 @llvm.fpga.fifo.not.empty.p0i32(i32* nocapture)  #1
  
declare i1 @llvm.fpga.fifo.not.empty.p0f64(double* nocapture)  #1
  
attributes #0 = { nounwind uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "fpga.demangled.name"="entry" "fpga.top.func"="entry" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

attributes #1 = { inaccessiblememonly nounwind "xlx.port.bitwidth"="320000" }

attributes #2 = { inaccessiblememonly nounwind }

attributes #3 = { noduplicate nounwind }

attributes #4 = { nounwind }

attributes #5 = { argmemonly nounwind }
!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 7.0.0 "}
!2 = !{!3}
!3 = !{!"discrete.components", !4}
!4 = !{!5}
!5 = !{!"stream", %_QMhls_streamThlsstream_integer* null}
