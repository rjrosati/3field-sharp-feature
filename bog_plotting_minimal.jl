include("bog_plotting_pluto.jl")


# make a plot of the Bogoliubov coefficients as a function of k/k_f
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
savefig("coeffs_k.png")

plot(κs,[analyticPz(κ,sms) for κ in κs])
plot!(yscale=:log10,xscale=:log10)
plot!(xlabel="κ",ylabel="Pζ enhancement",leg=false)
savefig("Pz.png")


