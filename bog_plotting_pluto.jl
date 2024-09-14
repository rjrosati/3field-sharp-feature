### A Pluto.jl notebook ###
# v0.19.46

using Markdown
using InteractiveUtils

# ╔═╡ 01c81a1b-40f4-417c-874b-1c06ea2a9c0d
begin
	import Pkg
	Pkg.activate(".")
end

# ╔═╡ 6f62718e-978b-4c1d-9a56-164588f4984c
begin
    using HCubature
    using StaticArrays
    using Parameters
    #the default special functions don't support complex order parameters for the Bessel functions, so we need to implement these ourselves
    import SpecialFunctions: gamma
    import HypergeometricFunctions: pFq
    besselj(nu,x) = (x/2)^nu / gamma(nu+1) * pFq((),(1+nu,),-(x/2)^2)
    function bessely(nu,x)
        sinp,cosp = sincospi(nu)
        return (besselj(nu,x)*cosp - besselj(-nu,x))/sinp
    end
    hankelh1(nu,x) = besselj(nu,x) + im*bessely(nu,x)
    hankelh2(nu,x) = besselj(nu,x) - im*bessely(nu,x)

    include("bog_coeffs_2f_masses.jl")
    include("bog_coeffs_3f_masses.jl")
end

# ╔═╡ d2b68c8e-dece-4079-a87d-32c4b2ece41c
begin
    function alphas_2f_mass(κ,Ω0,ξss,mss0)
        return [κ^2 + (3+ξss)/2*Ω0^2 - Ω0*sqrt(4κ^2+(3+ξss)^2/4*Ω0^2),
                κ^2 + (3+ξss)/2*Ω0^2 + Ω0*sqrt(4κ^2+(3+ξss)^2/4*Ω0^2)
       ]
    end

    function alphas_mass_viete(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0)
        a = 1
        b = -3κ^2 - (2+ξbb+ξss)*τ0^2 - (3+ξss)*Ω0^2 - mss0 - mbb0
        c = 3κ^4 + 2κ^2*((ξbb+ξss)*τ0^2 + (1+ξss)*Ω0^2 + mbb0 + mss0) + (mbb0 + (ξbb-1)*τ0^2)*(mss0+(ξss-1)*τ0^2+(ξss+3)*Ω0^2)
        d = -κ^2*(mbb0+κ^2 + (ξbb-1)*τ0^2)*(mss0+κ^2 + (ξss-1)*(τ0^2+Ω0^2))
        # Solve with Viète's formula, see https://en.wikipedia.org/wiki/Cubic_equation#Trigonometric_solution_for_three_real_roots
        p = Complex((3a*c - b^2)/(3a^2))
        q = Complex((2b^3-9a*b*c+27*a^2*d)/(27a^3))
        k = @SVector [0,1,2]
        t = 2*sqrt(-p/3).*cos.(1/3*acos(3q/(2p)*sqrt(-3/p)) .- 2pi/3 .* k)
        return t .- b/(3a)
    end
end

# ╔═╡ 36107a0a-28ca-43a8-97ca-3b97063a8f33
begin
	@with_kw struct SyntheticModelspec
		d::Int = 2
		feature_at::Float64 =23.0 # e-folds before the end of inflation
		feature_fwhm::Float64=0.25
		efolds::Float64 = 80.0
		ϵ::Float64=0.01
		Mss_ratio::Float64 =-3.0 # Mss/(Ω^2+τ^2)
		Mss_const::Float64=  0.0 # H^2
		Ω::Float64=1.0
		τ::Float64=1.0
		Msb_ratio::Float64= 0.0  # Msb/τ
		Msb_const::Float64= 0.0  # H^2
		Mbb_ratio::Float64= 1.0  # Mbb/τ^2
		Mbb_const::Float64= 0.0 # H^2
		Hinit::Float64 = 1e-4
	end
	function effective_width(sms::SyntheticModelspec)
        δ   = sms.feature_fwhm
        return δ
    end
    function getΩ2f(sms::SyntheticModelspec)
        if sms.d == 2
            return sms.Ω
        else
            @assert sms.d == 3
            return sqrt(sms.Ω^2+sms.τ^2)
        end
    end
end

# ╔═╡ 926c2414-e521-4de0-ad63-a9398f398bdf
begin
    function get_all_bogoliubov_mass(κ,sms::SyntheticModelspec,adhoc_phase=exp(-2im*κ*(1-exp(-effective_width(sms)/2))))
        if sms.d == 3
            ξss = sms.Mss_ratio
            ξsb = sms.Msb_ratio
            ξbb = sms.Mbb_ratio
            mss0= sms.Mss_const
            mbb0= sms.Mbb_const
            δ   = effective_width(sms)
            Ω0  = sms.Ω
            τ0  = sms.τ
            α1,α2,α3  =  alphas_mass_viete(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0)
            azz = alphaζζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            azs = alphaζs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            azb = alphaζb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            asz = alphasζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            ass = alphass_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            asb = alphasb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            abz = alphabζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            abs = alphabs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            abb = alphabb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bzz =  -adhoc_phase*betaζζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bzs =  -adhoc_phase*betaζs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bzb =  -adhoc_phase*betaζb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bsz =  -adhoc_phase*betasζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bss =  -adhoc_phase*betass_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bsb =  -adhoc_phase*betasb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bbz =  -adhoc_phase*betabζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bbs =  -adhoc_phase*betabs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            bbb =  -adhoc_phase*betabb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,α1,α2,α3)
            aij = @SMatrix [ azz azs azb ; asz ass asb ; abz abs abb]
            bij = @SMatrix [ bzz bzs bzb ; bsz bss bsb ; bbz bbs bbb]
        else
            @assert sms.d == 2
            ξss = sms.Mss_ratio
            ξsb = sms.Msb_ratio
            ξbb = sms.Mbb_ratio
            mss0= sms.Mss_const
            mbb0= sms.Mbb_const
            δ   = effective_width(sms)
            Ω0  = sms.Ω
            α1,α2 =  alphas2f_mass(κ,Ω0,ξss,mss0)
            azz = alphaζζ_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            azs = alphaζs_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            asz = alphasζ_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            ass = alphass_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            bzz =  -adhoc_phase*betaζζ_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            bzs =  -adhoc_phase*betaζs_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            bsz =  -adhoc_phase*betasζ_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            bss =  -adhoc_phase*betass_2f_mass(κ,Ω0,ξss,mss0,δ,α1,α2)
            aij = @SMatrix [ azz azs ; asz ass ]
            bij = @SMatrix [ bzz bzs ; bsz bss ]
        end

        return aij, bij
    end

    function verify_bog_identities(κ,sms::SyntheticModelspec,adhoc_phase=exp(-2im*κ*(1-exp(-effective_width(sms)/2))))
        # see (4.6), (4.7) of excited states paper
        α,β = get_all_bogoliubov(κ,sms,adhoc_phase)
        for i in 1:sms.d
            for j in 1:sms.d
                exp1 = α[i,:].*conj.(α[j,:]) .- conj.(β[i,:]).*β[j,:]
                exp2 = α[i,:].*conj.(β[j,:]) .- conj.(β[i,:]).*α[j,:]
                if i == j
                    @assert all(exp1 .≈ ones(sms.d))
                else
                    @assert all(exp1 .≈ zeros(sms.d))
                end
                @assert all(exp2 .≈ zeros(sms.d))
            end
        end
    end
end


# ╔═╡ 83a1390a-e82c-4a40-9c78-d9d729dc76c1
begin
	using Plots
	
	ρ = 0.5
	o2f = 23.7
	δ = 0.225
	constmass = 2.0
	sms = SyntheticModelspec(
		d=3,
		feature_fwhm = δ,
		Mss_ratio = -3.0,
		Mss_const = constmass,
		Mbb_ratio = 0.0,
		Mbb_const = constmass,
		Ω = o2f * sin(atan(ρ)),
		τ = o2f * cos(atan(ρ))
	)
	numks = 1000

    αnorms = zeros(sms.d,sms.d,numks)
    βnorms = zeros(sms.d,sms.d,numks)
    κs = 10.0 .^ (range(-2,2,length=numks))
    for (k,κ) in enumerate(κs)
        phase = exp(-2im*κ*(1-exp(-sms.feature_fwhm/2)))
        αs, βs = get_all_bogoliubov_mass(κ,sms,phase)
        for i in 1:3
            for j in 1:3
                αnorms[i,j,k] = abs(αs[i,j])
                βnorms[i,j,k] = abs(βs[i,j])
            end
        end
    end
    labels = ["z","s","b"]
    styles = [:solid,:dash,:dashdot]
    lw=2
    plot()
    for i in 1:3
        for j in 1:3
            k = i ==3 || j == 3 ? 3 : i==2 || j == 2 ? 2 : 1
            plot!(κs,αnorms[i,j,:],label="α$(labels[i])$(labels[j])",lw=lw,ls=styles[k])
            plot!(κs,βnorms[i,j,:],label="β$(labels[i])$(labels[j])",lw=lw,ls=styles[k])
        end
    end
	plot!(xscale=:log10,yscale=:log10,leg=false)
    plot!(xlabel="κ")
    plot!(title="Bog. coefficients, ρ = $ρ")
    plot!(yticks=[1e-5,1e-3,1e-1,1e1,1e3,1e5,1e7,1e9],minorticks=true)
	plot!(ylabel="|α,β|")
	plot!(legend=:outertopright)
end

# ╔═╡ 489f64bc-b53e-4d43-9824-6b7caab8dc06
begin
	    function analyticPz_3f_mass(κ,sms::SyntheticModelspec,adhoc_phase=exp(-2im*κ*(1-exp(-δ/2))))
        return analyticPz_3f_mass(κ,sms.Ω,sms.τ,sms.Mss_ratio,sms.Msb_ratio,sms.Mbb_ratio,sms.Mss_const,sms.Mbb_const,effective_width(sms),adhoc_phase)
    end

    function analyticPz_3f_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,adhoc_phase=exp(-2im*κ*(1-exp(-δ/2))))
        αs  =  alphas_mass_viete(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0)
        azz = alphaζζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        azs = alphaζs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        azb = alphaζb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        bzz =  -adhoc_phase*betaζζ_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        bzs =  -adhoc_phase*betaζs_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        bzb =  -adhoc_phase*betaζb_mass(κ,Ω0,τ0,ξss,ξsb,ξbb,mss0,mbb0,δ,αs...)
        return abs(azz+bzz)^2 + abs(azs+bzs)^2 + abs(azb+bzb)^2
    end

    function analyticPz_2f_mass(κ,sms::SyntheticModelspec,adhoc_phase=exp(-2im*κ*(1-exp(-effective_width(sms)/2))))
        @assert sms.d == 2
        ξss = sms.Mss_ratio
        mss0= sms.Mss_const
        Ω0  = sms.Ω
        δ   = effective_width(sms)
        return analyticPz_2f_mass(κ,Ω0,ξss,mss0,δ,adhoc_phase)
    end
	function analyticPz_2f_mass(κκ,Ω0,ξss,mss0,δ,adhoc_phase=exp(-2im*κ*(1-exp(-δ/2))))
        αs  =  alphas_2f_mass(κ,Ω0,τ0,ξss,mss0)
        azz = alphaζζ_2f_mass(κ,Ω0,ξss,mss0,δ,αs...)
        azs = alphaζs_2f_mass(κ,Ω0,ξss,mss0,δ,αs...)
        bzz =  -adhoc_phase*betaζζ_2f_mass(κ,Ω0,ξss,mss0,δ,αs...)
        bzs =  -adhoc_phase*betaζs_2f_mass(κ,Ω0,ξss,mss0,δ,αs...)
		return abs(azz+bzz)^2 + abs(azs+bzs)^2
    end
	function analyticPz(κ,sms::SyntheticModelspec,adhoc_phase=exp(-2im*κ*(1-exp(-effective_width(sms)/2))))
        if sms.d == 2
            return analyticPz_2f_mass(κ,sms,adhoc_phase)
        else
            return analyticPz_3f_mass(κ,sms,adhoc_phase)
        end
    end
end
	

# ╔═╡ 914152fc-2750-4b79-a3bd-99dd1cd3ab5a
begin
	plot(κs,[analyticPz(κ,sms) for κ in κs])
	plot!(yscale=:log10,xscale=:log10)
	plot!(xlabel="κ",ylabel="Pζ enhancement",leg=false)
end

# ╔═╡ Cell order:
# ╠═01c81a1b-40f4-417c-874b-1c06ea2a9c0d
# ╠═6f62718e-978b-4c1d-9a56-164588f4984c
# ╠═d2b68c8e-dece-4079-a87d-32c4b2ece41c
# ╠═36107a0a-28ca-43a8-97ca-3b97063a8f33
# ╠═489f64bc-b53e-4d43-9824-6b7caab8dc06
# ╠═926c2414-e521-4de0-ad63-a9398f398bdf
# ╠═83a1390a-e82c-4a40-9c78-d9d729dc76c1
# ╠═914152fc-2750-4b79-a3bd-99dd1cd3ab5a
