yosys logger -notime -stderr
yosys -import
yosys read_verilog -defer -noautowire -sv  gen_rtl/adders.RippleCarry/RippleCarryAdder12_assert.sv
yosys read_verilog -defer -noautowire -sv  gen_rtl/adders.RippleCarry/RippleCarryAdder12.sv

yosys hierarchy -check  -top RippleCarryAdder12 

prep

# opt -full -purge -sat
# opt -full -purge -sat

#log -stdout "Running synthesis"
##synth -noabc -flatten
#hierarchy -check
#yosys proc
#flatten
#opt_expr
#opt_clean
#opt -nodffe -nosdff
#fsm
#opt
#opt -full -purge -sat
#wreduce
#peepopt
#opt_clean
#alumacc 
#share
#opt
#memory -nomap
#opt_clean
#
opt -fast -full
memory_map
#
opt -full
techmap
#opt -fast
opt -full -purge -sat
opt -fast


#abc -g AND,NAND,XOR
#opt -full -purge -sat
# opt -full -purge -sat
# replace undefined values with 0
# setundef -zero

opt_clean -purge

# opt -full
clean -purge

check
stat
#show -color orange t:*AND* -color orange t:*and* -stretch