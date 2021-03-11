### A Pluto.jl notebook ###
# v0.12.21

using Markdown
using InteractiveUtils

# ╔═╡ c6c4fdc2-82ad-11eb-044a-abace30e2eac
using Distributions, Plots

# ╔═╡ d0469884-7eba-11eb-1d19-eb3680e350a3
md"
# 10-armed Testbed

10-armed bandit testbed to compare ϵ-greedy methods for various values of ϵ.

__ϵ-greedy method__\
Initialisation: ∀ a, q(a) = 0.\
p(ϵ): take a random action.\
p(1 - ϵ): take the greedy action.\
Update the action-value estimate with a sample-average.\

__testbed__\
q\*(a): sampled from 𝒩(0, 1).\
Rewards: sampled from 𝒩(q\*(a), 1).
"

# ╔═╡ c94c7f8e-82ad-11eb-0aa8-7ff748d044f1
qₓ = rand(Normal(), 10)

# ╔═╡ 55683178-82b0-11eb-21a0-118a2c5910ed
function make_ϵ_greedy(ϵ)
	function f()
		if rand(Uniform()) < ϵ
			rand(1:10, 1)
		else
			# Take the greedy action
			1  # temporary
		end
	end
end

# ╔═╡ 98f15cf6-82b4-11eb-3618-215e3f8fa2a4
f = make_ϵ_greedy(0.1)

# ╔═╡ Cell order:
# ╟─d0469884-7eba-11eb-1d19-eb3680e350a3
# ╟─c6c4fdc2-82ad-11eb-044a-abace30e2eac
# ╟─c94c7f8e-82ad-11eb-0aa8-7ff748d044f1
# ╠═55683178-82b0-11eb-21a0-118a2c5910ed
# ╠═98f15cf6-82b4-11eb-3618-215e3f8fa2a4
