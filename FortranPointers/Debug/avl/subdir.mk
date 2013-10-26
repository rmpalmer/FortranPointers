################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../avl/avl_struct.f90 

OBJS += \
./avl/avl_struct.o 


# Each subdirectory must supply rules for building sources it contributes
avl/%.o: ../avl/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

avl/avl_struct.o: ../avl/avl_struct.f90


