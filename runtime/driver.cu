// Driver to execute a PTX program from the host

#include <iostream>
#include <cuda.h>

// Error checking macro
#define CUDA_CHECK(err) \
    if (err != CUDA_SUCCESS) { \
        std::cerr << "CUDA error: " << err << " in line " << __LINE__ << std::endl; \
        exit(-1); \
    }

int main() {
    CUdevice cuDevice;
    CUcontext cuContext;
    CUmodule cuModule;
    CUfunction cuFunction;

    // Initialize the CUDA driver API
    CUDA_CHECK(cuInit(0));

    // Get the first CUDA device
    CUDA_CHECK(cuDeviceGet(&cuDevice, 0));

    // Create a context on this device
    CUDA_CHECK(cuCtxCreate(&cuContext, 0, cuDevice));

    // Load the CUBIN module
    CUDA_CHECK(cuModuleLoad(&cuModule, "loop.cubin"));

    // Get the kernel function from the module
    CUDA_CHECK(cuModuleGetFunction(&cuFunction, cuModule, "main"));

    // Launch the kernel with a single block and single thread
    CUDA_CHECK(cuLaunchKernel(cuFunction,
                              1, 1, 1,  // Grid dimensions
                              1, 1, 1,  // Block dimensions
                              0, 0,     // Shared memory and stream
                              NULL, 0)  // Kernel arguments
    );

    // Synchronize the device to wait for the kernel to finish
    CUDA_CHECK(cuCtxSynchronize());

    // Cleanup resources
    CUDA_CHECK(cuModuleUnload(cuModule));
    CUDA_CHECK(cuCtxDestroy(cuContext));

    std::cout << "Kernel execution completed." << std::endl;
    return 0;
}