/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#define gen_hls_functions(stream_type) \
    stream_type function hls_lowered_read_##stream_type(stream_data) ; \
        stream_type :: stream_data ; \
    end function; \
    ; \
    subroutine hls_lowered_write_##stream_type(input_data, stream_data) ; \
        stream_type, value :: input_data; \
        stream_type :: stream_data ; \
    end subroutine ; \
    integer(kind=1) function hls_lowered_full_##stream_type(stream_data) ; \
        stream_type :: stream_data ; \
    end function ; \
    integer(kind=1) function hls_lowered_empty_##stream_type(stream_data) ; \
        stream_type :: stream_data; \
    end function ; \
    subroutine set_depth_##stream_type(stream_data, depth) ; \
        stream_type :: stream_data; \
        integer, value :: depth; \
    end subroutine ;

#define _count(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, N, ...) N
#define count(...) _count(__VA_ARGS__, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

#define proto_hls_stream_derived( ... ) \
    module hls_stream ; \
        use derived_datatypes; \
        type HLSStream ; \
        end type ; \
        _specialised_hls_types( count(__VA_ARGS__), __VA_ARGS__ ) ; \
        contains ; \
        _proto_hls_stream( count(__VA_ARGS__), __VA_ARGS__ )

#define proto_hls_stream( ... ) \
    module hls_stream ; \
        type HLSStream ; \
            integer :: data_integer ; \
        end type ; \
        _specialised_hls_types( count(__VA_ARGS__), __VA_ARGS__ ) ; \
        contains ; \
        _proto_hls_stream( count(__VA_ARGS__), __VA_ARGS__ )

#define gen_specialised_hls_type(stream_type) \
    type HLSStream_##stream_type ; \
        stream_type :: data_##stream_type ; \
    end type ;


#define _specialised_hls_types( N, ... ) __specialised_hls_types( N, __VA_ARGS__ )
#define __specialised_hls_types( N, ... ) specialised_hls_types##N( __VA_ARGS__ )
#define specialised_hls_types1(stream_type) gen_specialised_hls_type(stream_type)
#define specialised_hls_types2(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types1(__VA_ARGS__)
#define specialised_hls_types3(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types2(__VA_ARGS__)
#define specialised_hls_types4(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types3(__VA_ARGS__)
#define specialised_hls_types5(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types4(__VA_ARGS__)
#define specialised_hls_types6(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types5(__VA_ARGS__)
#define specialised_hls_types7(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types6(__VA_ARGS__)
#define specialised_hls_types8(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types7(__VA_ARGS__)
#define specialised_hls_types9(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types8(__VA_ARGS__)
#define specialised_hls_types10(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types9(__VA_ARGS__)
#define specialised_hls_types11(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types10(__VA_ARGS__)
#define specialised_hls_types12(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types11(__VA_ARGS__)
#define specialised_hls_types13(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types12(__VA_ARGS__)
#define specialised_hls_types14(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types13(__VA_ARGS__)
#define specialised_hls_types15(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types14(__VA_ARGS__)
#define specialised_hls_types16(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types15(__VA_ARGS__)
#define specialised_hls_types17(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types16(__VA_ARGS__)
#define specialised_hls_types18(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types17(__VA_ARGS__)
#define specialised_hls_types19(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types18(__VA_ARGS__)
#define specialised_hls_types20(stream_type, ...) gen_specialised_hls_type(stream_type) specialised_hls_types19(__VA_ARGS__)


#define _proto_hls_stream( N, ... ) __proto_hls_stream( N, __VA_ARGS__ ) \
                                    end module ;
#define __proto_hls_stream( N, ... ) proto_hls_stream##N( __VA_ARGS__ )
#define proto_hls_stream1(stream_type) gen_hls_functions(stream_type)
#define proto_hls_stream2(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream1(__VA_ARGS__)
#define proto_hls_stream3(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream2(__VA_ARGS__)
#define proto_hls_stream4(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream3(__VA_ARGS__)
#define proto_hls_stream5(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream4(__VA_ARGS__)
#define proto_hls_stream6(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream5(__VA_ARGS__)
#define proto_hls_stream7(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream6(__VA_ARGS__)
#define proto_hls_stream8(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream7(__VA_ARGS__)
#define proto_hls_stream9(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream8(__VA_ARGS__)
#define proto_hls_stream10(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream9(__VA_ARGS__)
#define proto_hls_stream11(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream10(__VA_ARGS__)
#define proto_hls_stream12(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream11(__VA_ARGS__)
#define proto_hls_stream13(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream12(__VA_ARGS__)
#define proto_hls_stream14(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream13(__VA_ARGS__)
#define proto_hls_stream15(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream14(__VA_ARGS__)
#define proto_hls_stream16(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream15(__VA_ARGS__)
#define proto_hls_stream17(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream16(__VA_ARGS__)
#define proto_hls_stream18(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream17(__VA_ARGS__)
#define proto_hls_stream19(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream18(__VA_ARGS__)
#define proto_hls_stream20(stream_type, ...) gen_hls_functions(stream_type) proto_hls_stream19(__VA_ARGS__)

! Used to get the type of the stream. We use an integer to keep the name
!#define set_hls_stream_type(stream, stream_type) integer :: _stream_type_##stream##_##stream_type
#define set_hls_stream_type(stream, stream_type) call set_depth_##stream_type(stream##%data_##stream_type, 0) 
#define set_hls_stream_type_kind(stream, stream_type, kind) call set_depth_##stream_type(stream##%data_##stream_type, 0) 
#define set_array_hls_stream_type(stream, stream_type, n_elems) \
    integer :: _i; \
    do _i=1,n_elems ; \
        call set_depth_##stream_type(stream(_i)##%data_##stream_type, 0); \
    end do;
