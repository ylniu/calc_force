subroutine init()
	use kinds, only: DP
	use input, only: power_min, power_max, if_fit, fout, fdata, freq0, kspring, &
		amp, freqoffset, set
	implicit none
	!----------------------------------------------------------------------------
	if_fit     = .true.
	power_min  = -8
	power_max  =  4
	fout       = "scan.out"
	fdata      = "total.txt"
	freq0      = 27400.0_DP
	kspring    = 1800.0_DP
	amp        = 1.0E-10_DP
	freqoffset = 0.0_DP
	set        = 1
	!----------------------------------------------------------------------------
	return
end subroutine
