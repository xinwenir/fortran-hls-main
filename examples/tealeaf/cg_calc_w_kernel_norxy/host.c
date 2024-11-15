/***************************************************************
    Copyright 2023 Hewlett Packard Enterprise Development LP.
****************************************************************/

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <CL/cl.h>
#include "omp.h"

////////////////////////////////////////////////////////////////////////////////

// Use a static data size for simplicity
//
#define DATA_SIZE (10)
#define FORTRAN_IMPL 0
#define C_IMPL 1 


#define UNOPT 0
#define SHIFT_REGISTER 1
#define DATAFLOW 2

////////////////////////////////////////////////////////////////////////////////

int kernel_manager(int emulate, int implementation, int optimisation, cl_int * x_min, cl_int * x_max, cl_int * y_min, cl_int * y_max, cl_int * halo_exchange_depth, cl_double * p,
                   cl_double * w, cl_double * Kx, cl_double * Ky, cl_double * Di, cl_double * pw,
                   size_t volume_size, size_t density_size, size_t energy1_size, size_t u_size);
//int kernel_manager(emulation, implementation, optimisation, x_min, x_max, y_min, y_max, halo_exchange_depth, p,
//                     w, Kx, Ky, Di, pw,
//                     volume_size, density_size, energy1_size, u_size);

int main(int argc, char** argv)
{
    if(argc < 4) {
        printf("host <implementation> <optimisation> <emulation> <size>\nemulation: 0, 1\nimplementation: 0 - FORTRAN_IMPL, 1 - C_IMPL\noptimisation: 0 - UNOPT, 1 - SHIFT REGISTER\n"); 
    }
    int implementation = atoi(argv[1]);
    int optimisation = atoi(argv[2]);
    int emulation = atoi(argv[3]);
    int size = atoi(argv[4]);


    cl_int x_min[1] = {0};
    cl_int x_max[1] = {6};
    cl_int y_min[1] = {0};
    //cl_int y_max[1] = {500000};
    cl_int y_max[1] = {size};
    cl_int halo_exchange_depth[] = {1};

    size_t volume_size = ((x_max[0] - x_min[0] + 4 + 1) * (y_max[0] - y_min[0] + 4 + 1));
    size_t density_size = ((x_max[0] - x_min[0] + 2 * halo_exchange_depth[0] + 1) * (y_max[0] - y_min[0] + 2 * halo_exchange_depth[0] + 1));
    size_t energy1_size = density_size;
    size_t u_size = density_size;

    volume_size = 1200000;
    density_size = 1200000;
    energy1_size = 1200000;
    u_size = 1200000;

    double* p = (double*)malloc(volume_size * sizeof(double));
    double* w = (double*)malloc(density_size * sizeof(double));
    double* Kx = (double*)malloc(energy1_size * sizeof(double));
    double* Ky = (double*)malloc(u_size * sizeof(double));
    double* Di = (double*)malloc(u_size * sizeof(double));

    //int i = 0;
    //for(int k = 0; k < 6; k++) {
    //    for(int j = 0; j < 6; j++) {
    //        volume[j * 6 + k] = i + 1;
    //        i += 1;
    //    }
    //}

    //i = 0;
    //for(int k = 0; k < 2; k++) {
    //    for (int j = 0; j < 2; j++) {
    //        density[j * 2 + k] = i + 1;
    //        energy1[j * 2 + k] = i + 1;
    //        u[j * 2 + k] = i + 1;

    //        i = i+1;
    //    }
    //}
    //

    for(int i = 0; i < density_size; i++) {
        p[i] = (double)i;///100000.;
        w[i] = (double)i;///100000.;
        Kx[i] = (double)i;///100000.;
        Ky[i] = (double)i;///100000.;
        Di[i] = (double)i;///100000.;
        //printf("density[i] = %lf\n", density[i]);
        //printf("energy1[i] = %lf\n", energy1[i]);
        //printf("u[i] = %lf\n", u[i]);
    }


    cl_double pw[] = {0};
    int err;

    err = kernel_manager(emulation, implementation, optimisation, x_min, x_max, y_min, y_max, halo_exchange_depth, p,
                         w, Kx, Ky, Di, pw,
                         volume_size, density_size, energy1_size, u_size);

}

int kernel_manager(int emulate, int implementation, int optimisation, cl_int * x_min, cl_int * x_max, cl_int * y_min, cl_int * y_max, cl_int * halo_exchange_depth, cl_double * p,
                   cl_double * w, cl_double * Kx, cl_double * Ky, cl_double * Di, cl_double * pw,
                   size_t volume_size, size_t density_size, size_t energy1_size, size_t u_size) {
    printf("Hello from main\n");
    printf("x_min: %d, x_max: %d\n", x_min[0], x_max[0]);
    int err;                            // error code returned from api calls

    float data[DATA_SIZE];              // original data set given to device
    int results[DATA_SIZE];           // results returned from device
    unsigned int correct;               // number of correct results returned

    cl_device_id device_id;             // compute device id 
    cl_context context;                 // compute context
    cl_command_queue commands;          // compute command queue
    cl_program program;                 // compute program
    cl_kernel kernel;                   // compute kernel

    cl_mem p_buf;
    cl_mem w_buf;
    cl_mem Kx_buf;
    cl_mem Ky_buf;
    cl_mem Di_buf;

    cl_mem x_min_buf;
    cl_mem x_max_buf;
    cl_mem y_min_buf;
    cl_mem y_max_buf;
    cl_mem halo_exchange_depth_buf;

    cl_mem pw_buf;



    //
    err = clGetDeviceIDs(NULL, CL_DEVICE_TYPE_ACCELERATOR, 1, &device_id, NULL);

    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to create a device group!\n");
        return EXIT_FAILURE;
    }

    // Create a compute context 
    //
    context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
    if (!context)
    {
        printf("Error: Failed to create a compute context!\n");
        return EXIT_FAILURE;
    }

    // Create a command commands
    //
    commands = clCreateCommandQueue(context, device_id, 0, &err);
    if (!commands)
    {
        printf("Error: Failed to create a command commands!\n");
        return EXIT_FAILURE;
    }

    printf("Before loading bitstream\n");

    FILE * f;

    if (implementation == FORTRAN_IMPL) {
        if(optimisation == UNOPT) {
            printf("RUNNING FORTRAN UNOPTIMISED\n");
            f = fopen("/home/nx08/nx08/s2081362-2/HPE/fortran_hls/fortran_hls/tealeaf_optimised_bin/fpga/f_fpga_calc_w_kernel_norxy.xclbin", "r");
        }
        if(optimisation == SHIFT_REGISTER) {
            printf("RUNNING FORTRAN SHIFT REGISTER\n");
            //f = fopen("fpga_optimised/f_hw_partial_sums_field_summary_kernel.xclbin", "r");
            f = fopen("/home/nx08/nx08/s2081362-2/HPE/fortran_hls/fortran_hls/tealeaf_optimised_bin/fpga_optimised/f_optimised_shift_register_calc_w_kernel_norxy.xclbin", "r");
        }
    }
    else if (implementation == C_IMPL) {
        if(optimisation == UNOPT) {
            printf("RUNNING C UNOPTIMISED\n");
            f = fopen("/home/nx08/nx08/s2081362-2/HPE/fortran_hls/fortran_hls/tealeaf_optimised_bin/fpga/c_fpga_calc_w_kernel_norxy.xclbin", "r");
        }
        else if(optimisation == SHIFT_REGISTER) {
            printf("RUNNING C SHIFT REGISTER\n");
            //f = fopen("fpga_optimised/c_hw_partial_sums_field_summary_kernel_pipeline7.xclbin", "r");
            f = fopen("/home/nx08/nx08/s2081362-2/HPE/fortran_hls/fortran_hls/tealeaf_optimised_bin/fpga_optimised/c_shift_register_calc_w_kernel_norxy.xclbin", "r");
        }
    }

    fseek(f, 0, SEEK_END);
    size_t file_size = ftell(f);
    fseek(f, 0, SEEK_SET);

    const unsigned char * binary = malloc(file_size * sizeof(const unsigned char));
    fread(binary, file_size, 1, f);

    cl_int binary_status;
    program = clCreateProgramWithBinary(context, 1, &device_id, &file_size, &binary, &binary_status, &err);
    if (!program) {
        printf("Error: Failed to create compute program!\n");
    }


    //// Build the program executable
    //
    err = clBuildProgram(program, 0, NULL, NULL, NULL, NULL);
    if (err != CL_SUCCESS)
    {
        size_t len;
        char buffer[2048];

        printf("Error: Failed to build program executable!\n");
        clGetProgramBuildInfo(program, device_id, CL_PROGRAM_BUILD_LOG, sizeof(buffer), buffer, &len);
        printf("%s\n", buffer);
        exit(1);
    }


    //// Create the compute kernel in the program we wish to run
    ////
    kernel = clCreateKernel(program, "cg_calc_w_kernel_norxy", &err);
    if (!kernel || err != CL_SUCCESS)
    {
        printf("Error: Failed to create compute kernel!\n");
        exit(1);
    }

    //// Create the input and output arrays in device memory for our calculation
    ////

    x_min_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_int), NULL, NULL);
    x_max_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_int), NULL, NULL);
    y_min_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_int), NULL, NULL);
    y_max_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_int), NULL, NULL);
    halo_exchange_depth_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_int), NULL, NULL);

    p_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_double) * volume_size, NULL, NULL);
    w_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE, sizeof(cl_double) * density_size, NULL, NULL);
    Kx_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE,  sizeof(cl_double) * energy1_size, NULL, NULL);
    Ky_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE,  sizeof(cl_double) * energy1_size, NULL, NULL);
    Di_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE,  sizeof(cl_double) * u_size, NULL, NULL);

    pw_buf = clCreateBuffer(context,  CL_MEM_READ_WRITE,  sizeof(cl_double), NULL, NULL);
    

    //
    //// Write our data set into the input array in device memory 
    ////
    printf("Before write\n");
    err = clEnqueueWriteBuffer(commands, x_min_buf, CL_TRUE, 0, sizeof(cl_int), x_min, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, x_max_buf, CL_TRUE, 0, sizeof(cl_int), x_max, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, y_min_buf, CL_TRUE, 0, sizeof(cl_int), y_min, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, y_max_buf, CL_TRUE, 0, sizeof(cl_int), y_max, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, halo_exchange_depth_buf, CL_TRUE, 0, sizeof(cl_int), halo_exchange_depth, 0, NULL, NULL);

    printf("write.1\n");

    err = clEnqueueWriteBuffer(commands, p_buf, CL_TRUE, 0, sizeof(cl_double) * volume_size, p, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, w_buf, CL_TRUE, 0, sizeof(cl_double) * density_size, w, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, Kx_buf, CL_TRUE, 0, sizeof(cl_double) * energy1_size, Kx, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, Ky_buf, CL_TRUE, 0, sizeof(cl_double) * u_size, Ky, 0, NULL, NULL);
    err = clEnqueueWriteBuffer(commands, Di_buf, CL_TRUE, 0, sizeof(cl_double) * u_size, Di, 0, NULL, NULL);

    printf("write.2\n");

    err = clEnqueueWriteBuffer(commands, pw_buf, CL_TRUE, 0, sizeof(cl_double), pw, 0, NULL, NULL);

    printf("After write\n");

    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to write to source array!\n");
        exit(1);
    }

    //// Set the arguments to our compute kernel
    ////
    err = 0;
    //cl_int n = 2;
    //
    cl_int c_x_min = *x_min;
    cl_int c_x_max = *x_max;
    cl_int c_y_min = *y_min;
    cl_int c_y_max = *y_max;
    cl_int c_halo_exchange_depth = *halo_exchange_depth;
    


    printf("Before set kernel arg\n");
    if(implementation  == FORTRAN_IMPL && optimisation == UNOPT) {
        err  = clSetKernelArg(kernel, 0, sizeof(cl_mem), &x_min_buf);
        err  = clSetKernelArg(kernel, 1, sizeof(cl_mem), &x_max_buf);
        err  = clSetKernelArg(kernel, 2, sizeof(cl_mem), &y_min_buf);
        err  = clSetKernelArg(kernel, 3, sizeof(cl_mem), &y_max_buf);
        err  = clSetKernelArg(kernel, 4, sizeof(cl_mem), &halo_exchange_depth_buf);
    }
    else{
        err  = clSetKernelArg(kernel, 0, sizeof(cl_int), &c_x_min);
        err  = clSetKernelArg(kernel, 1, sizeof(cl_int), &c_x_max);
        err  = clSetKernelArg(kernel, 2, sizeof(cl_int), &c_y_min);
        err  = clSetKernelArg(kernel, 3, sizeof(cl_int), &c_y_max);
        err  = clSetKernelArg(kernel, 4, sizeof(cl_int), &c_halo_exchange_depth);
    }
    err  = clSetKernelArg(kernel, 5, sizeof(cl_mem), &p_buf);
    err  = clSetKernelArg(kernel, 6, sizeof(cl_mem), &w_buf);
    err  = clSetKernelArg(kernel, 7, sizeof(cl_mem), &Kx_buf);
    err  = clSetKernelArg(kernel, 8, sizeof(cl_mem), &Ky_buf);
    err  = clSetKernelArg(kernel, 9, sizeof(cl_mem), &Di_buf);
    err  = clSetKernelArg(kernel, 10, sizeof(cl_mem), &pw_buf);

    printf("After set kernel arg\n");

    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to set kernel arguments! %d\n", err);
        exit(1);
    }

    double start = omp_get_wtime();
    err = clEnqueueTask(commands, kernel, 0, NULL, NULL);
    if (err)
    {
        printf("Error: Failed to execute kernel!\n");
        return EXIT_FAILURE;
    }


    //// Wait for the command commands to get serviced before reading back results
    ////
    clFinish(commands);
    double exec_time = omp_get_wtime() - start;

    //// Read back the results from the device to verify the output
    ////
    printf("Before read buffer\n"); 
    err = clEnqueueReadBuffer( commands, pw_buf, CL_TRUE, 0, sizeof(double), pw, 0, NULL, NULL );  

    printf("After read buffer\n"); 
    if (err != CL_SUCCESS)
    {
        printf("Error: Failed to read output array! %d\n", err);
        exit(1);
    }

    printf("pw: %lf\n", *pw);

    printf("Execution time: %lf\n", exec_time);


    // Shutdown and cleanup
    //
    clReleaseMemObject(p_buf);
    clReleaseMemObject(w_buf);
    clReleaseMemObject(Kx_buf);
    clReleaseMemObject(Ky_buf);
    clReleaseMemObject(Di_buf);

    clReleaseMemObject(x_min_buf);
    clReleaseMemObject(x_max_buf);
    clReleaseMemObject(y_min_buf);
    clReleaseMemObject(y_max_buf);
    clReleaseMemObject(halo_exchange_depth_buf);

    clReleaseMemObject(pw_buf);

    clReleaseProgram(program);
    clReleaseKernel(kernel);
    clReleaseCommandQueue(commands);
    clReleaseContext(context);

    //free(volume);
    //free(density);
    //free(energy1);
    //free(u);

    fclose(f);

    return 0;
}

